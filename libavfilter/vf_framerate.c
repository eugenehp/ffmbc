/*
 *  Copyright (C) 2012 Mark Himsley
 *
 *  get_scene_score() Copyright (c) 2011 Stefano Sabatini
 *  taken from ffmpeg/libavfilter/vf_select.c
 *
 * This filter is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This filter is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * @file
 * filter for upsampling or downsampling a progressive source
 */

/* #define DEBUG */

#include "libavutil/avassert.h"
#include "libavutil/cpu.h"
#include "libavutil/common.h"
#include "libavutil/imgutils.h"
#include "libavutil/internal.h"
#include "libavutil/opt.h"
#include "libavutil/parseutils.h"
#include "libavutil/pixdesc.h"
#include "avfilter.h"

#undef NDEBUG
#include <assert.h>

#if CONFIG_AVCODEC
#include "libavcodec/dsputil.h"
#endif

#define N_SRCE 3
static const int frst = 0;
static const int crnt = (N_SRCE)>>1;
static int next;
static int prev;
static const int last = N_SRCE - 1;

typedef struct {
    const AVClass *class;
    // parameters
    AVRational dest_frame_rate;         ///< output frames per second
    int flags;                          ///< flags affecting frame rate conversion algorithm
    double scene_score;                 ///< score that denotes a scene change has happened
    int interp_start;                   ///< start of range to apply linear interpolation
    int interp_end;                     ///< end of range to apply linear interpolation

    int line_size[4];                   ///< bytes of pixel data per line for each plane
    int vsub;

    int pending_srce_frames;            ///< how many input frames are still waiting to be processed
    int flush;                          ///< are we flushing final frames
    int pending_end_frame;              ///< flag indicating we are waiting to call end_frame()

    AVRational srce_time_base;          ///< timebase of source
    int srce_w, srce_h;                 ///< input width and height

    AVRational dest_time_base;          ///< timebase of destination
    int32_t dest_frame_num;
    int64_t last_dest_frame_pts;        ///< pts of the last frame output
    int average_srce_pts_dest_delta;    ///< average input pts delta converted from input rate to output rate
    int average_dest_pts_delta;         ///< calculated average output pts delta

#if CONFIG_AVCODEC
    AVCodecContext *avctx;              ///< codec context required for the DSPContext (scene detect only)
    DSPContext c;                       ///< context providing optimized SAD methods   (scene detect only)
    double prev_mafd;                   ///< previous MAFD                             (scene detect only)
#endif

    AVFilterBufferRef *srce[N_SRCE];    ///< buffered source frames
    int64_t srce_pts_dest[N_SRCE];      ///< pts for source frames scaled to output timebase
    AVFilterBufferRef *work;            ///< frame we are working on

    char rate_out_expr[256];            ///< space for arg parsing
} FRAMERATEContext;

#define OFFSET(x) offsetof(FRAMERATEContext, x)
#define FRAMERATE_FLAG_SCD 01

static const AVOption framerate_options[] = {
    {"fps",                 "required output frames per second rate", OFFSET(dest_frame_rate), FF_OPT_TYPE_RATIONAL, {.dbl=50},                 0,       INT_MAX, 0 },

    {"interp_start",        "point to start linear interpolation",    OFFSET(interp_start),      FF_OPT_TYPE_INT,      {.dbl=15},                 0,       255,     0 },
    {"interp_end",          "point to end linear interpolation",      OFFSET(interp_end),        FF_OPT_TYPE_INT,      {.dbl=240},                0,       255,     0 },
    {"scene",               "scene change level",                     OFFSET(scene_score),       FF_OPT_TYPE_DOUBLE,   {.dbl=7.0},                0,       INT_MAX, 0 },

    {"flags",               "set flags",                              OFFSET(flags),             FF_OPT_TYPE_FLAGS,    {.dbl=1},                  0,       INT_MAX, 0, "flags" },
    {"scene_change_detect", "enable scene change detection",          0,                         FF_OPT_TYPE_CONST,    {.dbl=FRAMERATE_FLAG_SCD}, INT_MIN, INT_MAX, 0, "flags" },
    {"scd",                 "enable scene change detection",          0,                         FF_OPT_TYPE_CONST,    {.dbl=FRAMERATE_FLAG_SCD}, INT_MIN, INT_MAX, 0, "flags" },

    {NULL}
};

static const char *framerate_get_name(void *ctx)
{
    return "framerate";
}

static const AVClass framerate_class = {
    "FrameRateContext",
    framerate_get_name,
    framerate_options
};

static void next_source(AVFilterContext *ctx)
{
    FRAMERATEContext *priv_ctx = ctx->priv;

#ifdef DEBUG
    av_dlog(ctx,  "next_source()\n");
#endif

    if (priv_ctx->srce[last] && priv_ctx->srce[last] != priv_ctx->srce[last-1]) {
#ifdef DEBUG
        av_dlog(ctx, "next_source() unlink %d\n", last);
#endif
        avfilter_unref_buffer(priv_ctx->srce[last]);
    }
    for (int i = last; i > frst; i--) {
#ifdef DEBUG
        av_dlog(ctx, "next_source() copy %d to %d\n", i - 1, i);
#endif
        priv_ctx->srce[i] = priv_ctx->srce[i - 1];
    }
#ifdef DEBUG
    av_dlog(ctx, "next_source() make %d null\n", frst);
#endif
    priv_ctx->srce[frst] = NULL;
}

#if CONFIG_AVCODEC
static double get_scene_score(AVFilterContext *ctx, AVFilterBufferRef *crnt, AVFilterBufferRef *next)
{
    FRAMERATEContext *priv_ctx = ctx->priv;

    double ret = 0;

#ifdef DEBUG
    av_dlog(ctx, "get_scene_score()\n");
#endif

    if (crnt &&
            crnt->video->h    == next->video->h &&
            crnt->video->w    == next->video->w &&
            crnt->linesize[0] == next->linesize[0]) {
        int x, y;
        int64_t sad;
        double mafd, diff;
        uint8_t *p1 = crnt->data[0];
        uint8_t *p2 = next->data[0];
        const int linesize = crnt->linesize[0];

#ifdef DEBUG
        av_dlog(ctx, "get_scene_score() process\n");
#endif

        for (sad = y = 0; y < crnt->video->h; y += 8) {
            for (x = 0; x < linesize; x += 8) {
                sad += priv_ctx->c.sad[1](priv_ctx,
                                        p1 + y * linesize + x,
                                        p2 + y * linesize + x,
                                        linesize, 8);
            }
        }
        emms_c();
        mafd = sad / (crnt->video->h * crnt->video->w * 3);
        diff = fabs(mafd - priv_ctx->prev_mafd);
        ret  = av_clipf(FFMIN(mafd, diff), 0, 100.0);
        priv_ctx->prev_mafd = mafd;
    }
#ifdef DEBUG
        av_dlog(ctx, "get_scene_score() result is:%f\n", ret);
#endif
    return ret;
}
#endif

static int process_work_frame(AVFilterContext *ctx)
{
    FRAMERATEContext *priv_ctx = ctx->priv;
    int64_t work_next_pts;
    AVFilterBufferRef *copy_src1, *copy_src2;
    int interpolate;

#ifdef DEBUG
    av_dlog(ctx, "process_work_frame()\n");

    av_dlog(ctx, "process_work_frame() pending_input_frames:%d\n", priv_ctx->pending_srce_frames);

    if (priv_ctx->srce[prev]) av_dlog(ctx, "process_work_frame() srce prev pts:%"PRId64"\n", priv_ctx->srce[prev]->pts);
    if (priv_ctx->srce[crnt]) av_dlog(ctx, "process_work_frame() srce crnt pts:%"PRId64"\n", priv_ctx->srce[crnt]->pts);
    if (priv_ctx->srce[next]) av_dlog(ctx, "process_work_frame() srce next pts:%"PRId64"\n", priv_ctx->srce[next]->pts);
#endif

    if (!priv_ctx->srce[crnt]) {
        // the filter cannot do anything
#ifdef DEBUG
        av_dlog(ctx, "process_work_frame() no current frame cached: move on to next frame, do not output a frame\n");
#endif
        next_source(ctx);
        return 0;
    }

    av_assert0(priv_ctx->work);

    work_next_pts = priv_ctx->work->pts + priv_ctx->average_dest_pts_delta;

#ifdef DEBUG
    av_dlog(ctx, "process_work_frame() work crnt pts:%"PRId64"\n", priv_ctx->work->pts);
    av_dlog(ctx, "process_work_frame() work next pts:%"PRId64"\n", work_next_pts);
    av_dlog(ctx, "process_work_frame() dest time base:%u/%u\n",
            priv_ctx->dest_time_base.num, priv_ctx->dest_time_base.den);
    if (priv_ctx->srce[prev]) av_dlog(ctx, "process_work_frame() srce prev pts at dest time base:%"PRId64"\n",
            priv_ctx->srce_pts_dest[prev]);
    if (priv_ctx->srce[crnt]) av_dlog(ctx, "process_work_frame() srce crnt pts at dest time base:%"PRId64"\n",
            priv_ctx->srce_pts_dest[crnt]);
    if (priv_ctx->srce[next]) av_dlog(ctx, "process_work_frame() srce next pts at dest time base:%"PRId64"\n",
            priv_ctx->srce_pts_dest[next]);
#endif

    av_assert0(priv_ctx->srce[next]);

    // should filter be skipping input frame (output frame rate is lower than input frame rate)
    if (!priv_ctx->flush && priv_ctx->work->pts >= priv_ctx->srce_pts_dest[next]) {
#ifdef DEBUG
        av_dlog(ctx, "process_work_frame() work crnt pts >= srce next pts: SKIP FRAME, move on to next frame, do not output a frame\n");
#endif
        next_source(ctx);
        priv_ctx->pending_srce_frames--;
        return 0;
    }

    // calculate interpolation
    interpolate = (int) ((priv_ctx->work->pts - priv_ctx->srce_pts_dest[crnt]) * 256 / priv_ctx->average_srce_pts_dest_delta);
#ifdef DEBUG
    av_dlog(ctx, "process_work_frame() interpolate:%d/256\n", interpolate);
#endif
    copy_src1 = priv_ctx->srce[crnt];
    if (interpolate >priv_ctx->interp_end) {
#ifdef DEBUG
        av_dlog(ctx, "process_work_frame() source is:NEXT\n");
#endif
        copy_src1 = priv_ctx->srce[next];
    }
    if (priv_ctx->srce[prev] && interpolate < -priv_ctx->interp_end) {
#ifdef DEBUG
        av_dlog(ctx, "process_work_frame() source is:PREV\n");
#endif
        copy_src1 = priv_ctx->srce[prev];
    }

    // decide whether to blend two frames
    if ((interpolate >= priv_ctx->interp_start && interpolate <= priv_ctx->interp_end) || (interpolate <= -priv_ctx->interp_start && interpolate >= -priv_ctx->interp_end)) {
        double interpolate_scene_score = 0;

        if (interpolate > 0) {
#ifdef DEBUG
            av_dlog(ctx, "process_work_frame() interpolate source is:NEXT\n");
#endif
            copy_src2 = priv_ctx->srce[next];
        } else {
#ifdef DEBUG
            av_dlog(ctx, "process_work_frame() interpolate source is:PREV\n");
#endif
            copy_src2 = priv_ctx->srce[prev];
        }
        if (CONFIG_AVCODEC && (priv_ctx->flags & FRAMERATE_FLAG_SCD) && copy_src2) {
            interpolate_scene_score = get_scene_score(ctx, copy_src1, copy_src2);
#ifdef DEBUG
            av_dlog(ctx, "process_work_frame() interpolate scene score:%f\n", interpolate_scene_score);
#endif
        }
        // decide if the shot-change detection allows us to bland two frames
        if (interpolate_scene_score < priv_ctx->scene_score && copy_src2) {
            uint16_t src2_factor = abs(interpolate);
            uint16_t src1_factor = 256 - src2_factor;
#ifdef DEBUG
            av_dlog(ctx, "process_work_frame() INTERPOLATE to create work frame\n");
#endif
            for (int plane = 0; plane < 4 && copy_src1->data[plane] && copy_src2->data[plane]; plane++) {
                int cpy_line_width = priv_ctx->line_size[plane];
                uint8_t *cpy_src1_data = copy_src1->data[plane];
                int cpy_src1_line_size = copy_src1->linesize[plane];
                uint8_t *cpy_src2_data = copy_src2->data[plane];
                int cpy_src2_line_size = copy_src2->linesize[plane];
                int cpy_src_h = (plane > 0 && plane < 3) ? (copy_src1->video->h >> priv_ctx->vsub) : (copy_src1->video->h);
                uint8_t *cpy_dst_data = priv_ctx->work->data[plane];
                int cpy_dst_line_size = priv_ctx->work->linesize[plane];
                if (plane <1 || plane >2) {
                    // luma or alpha
                    for (int line = 0; line < cpy_src_h; line++) {
                        for (int pixel = 0; pixel < cpy_line_width; pixel++) {
                            // integer version of (src1 * src1_factor) + (src2 + src2_factor) + 0.5
                            // 0.5 is for rounding
                            // 128 is the integer representation of 0.5 << 8
                            cpy_dst_data[pixel] = ((cpy_src1_data[pixel] * src1_factor) + (cpy_src2_data[pixel] * src2_factor) + 128) >> 8;
                        }
                        cpy_src1_data += cpy_src1_line_size;
                        cpy_src2_data += cpy_src2_line_size;
                        cpy_dst_data += cpy_dst_line_size;
                    }
                } else {
                    // chroma
                    for (int line = 0; line < cpy_src_h; line++) {
                        for (int pixel = 0; pixel < cpy_line_width; pixel++) {
                            // as above
                            // because U and V are based around 128 we have to subtract 128 from the components.
                            // 32896 is the integer representation of 128.5 << 8
                            cpy_dst_data[pixel] = (((cpy_src1_data[pixel] - 128) * src1_factor) + ((cpy_src2_data[pixel] - 128) * src2_factor) + 32896) >> 8;
                        }
                        cpy_src1_data += cpy_src1_line_size;
                        cpy_src2_data += cpy_src2_line_size;
                        cpy_dst_data += cpy_dst_line_size;
                    }
                }
            }
            goto copy_done;
        }
#ifdef DEBUG
        else {
            av_dlog(ctx, "process_work_frame() CUT - DON'T INTERPOLATE\n");
        }
#endif
    }

#ifdef DEBUG
    av_dlog(ctx, "process_work_frame() COPY to the work frame\n");
#endif
    // copy the frame we decided is our base source
    for (int plane = 0; plane < 4 && copy_src1->data[plane]; plane++) {
        int cpy_line_width = priv_ctx->line_size[plane];
        uint8_t *cpy_src1_data = copy_src1->data[plane];
        int cpy_src1_line_size = copy_src1->linesize[plane];
        int cpy_src_h = (plane > 0 && plane < 3) ? (copy_src1->video->h >> priv_ctx->vsub) : (copy_src1->video->h);
        uint8_t *cpy_dst_data = priv_ctx->work->data[plane];
        int cpy_dst_line_size = priv_ctx->work->linesize[plane];
        for (int line = 0; line < cpy_src_h; line++) {
            memcpy(cpy_dst_data, cpy_src1_data, cpy_line_width);
            cpy_src1_data += cpy_src1_line_size;
            cpy_dst_data += cpy_dst_line_size;
        }
    }

copy_done:
    // should filter be re-using input frame (output frame rate is higher than input frame rate)
    if (!priv_ctx->flush && (work_next_pts + priv_ctx->average_dest_pts_delta) < (priv_ctx->srce_pts_dest[crnt] + priv_ctx->average_srce_pts_dest_delta)) {
#ifdef DEBUG
        av_dlog(ctx, "process_work_frame() REUSE input frame, same input frame will be used for next output frame\n");
#endif
    } else {
#ifdef DEBUG
        av_dlog(ctx, "process_work_frame() CONSUME input frame, move to next frame\n");
#endif
        priv_ctx->pending_srce_frames--;
        next_source(ctx);
    }
#ifdef DEBUG
    av_dlog(ctx, "process_work_frame() output a frame\n");
#endif
    return 1;
}

/** FFmpeg filter integration */


static void set_srce_frame_dest_pts(AVFilterContext *ctx)
{
    FRAMERATEContext *priv_ctx = ctx->priv;

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG, "set_srce_frame_output_pts()\n");
#endif

    // scale the input pts from the timebase differece between input and output
    if (priv_ctx->srce[prev])
        priv_ctx->srce_pts_dest[prev] = av_rescale_q(priv_ctx->srce[prev]->pts, priv_ctx->srce_time_base, priv_ctx->dest_time_base);
    if (priv_ctx->srce[crnt])
        priv_ctx->srce_pts_dest[crnt] = av_rescale_q(priv_ctx->srce[crnt]->pts, priv_ctx->srce_time_base, priv_ctx->dest_time_base);
    if (priv_ctx->srce[next])
        priv_ctx->srce_pts_dest[next] = av_rescale_q(priv_ctx->srce[next]->pts, priv_ctx->srce_time_base, priv_ctx->dest_time_base);
}

static void set_work_frame_pts(AVFilterContext *ctx)
{
    FRAMERATEContext *priv_ctx = ctx->priv;

    int pts, average_srce_pts_delta;

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG, "set_work_frame_pts()\n");
#endif

    av_assert0(priv_ctx->srce[next]);
    av_assert0(priv_ctx->srce[crnt]);

#ifdef DEBUG
    av_dlog(ctx, "set_work_frame_pts() srce crnt pts:%"PRId64"\n", priv_ctx->srce[crnt]->pts);
    av_dlog(ctx, "set_work_frame_pts() srce next pts:%"PRId64"\n", priv_ctx->srce[next]->pts);
    if (priv_ctx->srce[prev]) {
        av_dlog(ctx, "set_work_frame_pts() srce prev pts:%"PRId64"\n", priv_ctx->srce[prev]->pts);
    }
#endif

    average_srce_pts_delta = priv_ctx->average_srce_pts_dest_delta;
#ifdef DEBUG
    av_dlog(ctx, "set_work_frame_pts() initial average srce pts:%d\n", average_srce_pts_delta);
#endif
    // calculate the PTS delta
    if ((pts = (priv_ctx->srce[next]->pts - priv_ctx->srce[crnt]->pts))) {
        average_srce_pts_delta = average_srce_pts_delta?((average_srce_pts_delta+pts)>>1):pts;
    } else if (priv_ctx->srce[prev] && (pts = (priv_ctx->srce[crnt]->pts - priv_ctx->srce[prev]->pts))) {
        average_srce_pts_delta = average_srce_pts_delta?((average_srce_pts_delta+pts)>>1):pts;
    }

    priv_ctx->average_srce_pts_dest_delta = av_rescale_q(average_srce_pts_delta, priv_ctx->srce_time_base, priv_ctx->dest_time_base);

#ifdef DEBUG
    av_dlog(ctx, "set_work_frame_pts() calculated average srce pts:%d\n", average_srce_pts_delta);
    av_dlog(ctx, "set_work_frame_pts() calculated average srce pts:%d at dest time base:%u/%u\n",
            priv_ctx->average_srce_pts_dest_delta, priv_ctx->dest_time_base.num, priv_ctx->dest_time_base.den);
#endif

    set_srce_frame_dest_pts(ctx);

//    if (ctx->inputs[0] && !priv_ctx->average_dest_pts_delta) {
        // TODO does this overflow / underflow / lose precision
        // TODO why would this ever change?

//            priv_ctx->average_dest_pts_delta =
//                ctx->inputs[0]->time_base.den / ctx->inputs[0]->time_base.num *
//                priv_ctx->dest_frame_rate.den / priv_ctx->dest_frame_rate.num;

        priv_ctx->average_dest_pts_delta = av_rescale_q(
                ctx->inputs[0]->time_base.den / ctx->inputs[0]->time_base.num *
                priv_ctx->dest_frame_rate.den / priv_ctx->dest_frame_rate.num,
                priv_ctx->srce_time_base, priv_ctx->dest_time_base);

        //        av_log(ctx, AV_LOG_DEBUG, "set_frame_pts() average output pts from input timebase\n");
#ifdef DEBUG
        av_dlog(ctx, "set_work_frame_pts() calculated average dest pts:%d\n", priv_ctx->average_dest_pts_delta);
#endif
//    }
//        else {
//        priv_ctx->average_output_pts_delta = priv_ctx->average_input_pts_delta
//                * priv_ctx->input_frame_rate.num / priv_ctx->input_frame_rate.den
//                * priv_ctx->output_frame_rate.den / priv_ctx->output_frame_rate.num;
//        av_log(ctx, AV_LOG_DEBUG, "set_frame_pts() average output pts from source pts\n");
//    }


    if (!priv_ctx->dest_frame_num) {
        priv_ctx->work->pts = priv_ctx->last_dest_frame_pts = priv_ctx->srce_pts_dest[crnt];
    } else {
        priv_ctx->work->pts = priv_ctx->last_dest_frame_pts + priv_ctx->average_dest_pts_delta;
    }

#ifdef DEBUG
    av_dlog(ctx, "set_work_frame_pts() calculated dest pts:%"PRId64" at dest time base:%u/%u\n",
            priv_ctx->work->pts, priv_ctx->dest_time_base.num, priv_ctx->dest_time_base.den);
#endif
}

/** public general methods */

static av_cold int init(AVFilterContext *ctx, const char *args, void *opaque)
{
    FRAMERATEContext *priv_ctx = ctx->priv;
    char c;
    int count, raw, err = 0;

#ifdef DEBUG
            av_dlog(ctx, "init()\n");
#endif

            priv_ctx->dest_frame_num = 0;
    priv_ctx->class = &framerate_class;
    av_opt_set_defaults2(priv_ctx, 0, 0);

    next = crnt - 1;
    prev = crnt + 1;

    priv_ctx->dest_frame_rate = (AVRational){50, 1};

    if (args) {
        count = sscanf(args, "%255[^:]%c", priv_ctx->rate_out_expr, &c);
        if (count >= 1) { // found a number possibly followed by a non-number
#ifdef DEBUG
            av_dlog(ctx, "init() found at least %d parameters\n", count);
#endif
            if (!(strstr(priv_ctx->rate_out_expr, "="))) {
#ifdef DEBUG
                av_dlog(ctx, "init() found raw value to parse: %s\n", priv_ctx->rate_out_expr);
#endif
                if (av_parse_video_rate(&priv_ctx->dest_frame_rate, priv_ctx->rate_out_expr) < 0) {
                    if (count == 1) { // throw an error if this is the only input
                        av_log(ctx, AV_LOG_ERROR, "Invalid output rate '%s'.\n", priv_ctx->rate_out_expr);
                        return AVERROR(EINVAL);
                    }
                } else {
#ifdef DEBUG
                    av_dlog(ctx, "init() parsed raw value correctly\n");
#endif
                    raw = 1;
                    count--;
                }
            }
        }
        if (count >= 1) {
#ifdef DEBUG
            av_dlog(ctx, "init() there should be key=value parameters to decode\n");
#endif
            if (raw && (args = strchr(args,c))) { // move args on to the character after the colon
#ifdef DEBUG
                av_dlog(ctx, "init() skip raw parameter before decoding key=value parameters\n");
#endif
                args++;
            }
            if (args) { // parse the rest of the args
#ifdef DEBUG
                av_dlog(ctx, "init() decode key=value parameters\n");
#endif
                if ((err = (av_set_options_string(priv_ctx, args, "=", ":"))) < 0) {
                    av_log(ctx, AV_LOG_ERROR, "Error parsing options string: '%s'\n", args);
                    return err;
                }
            }
        }
    }

    return 0;
}

static av_cold void uninit(AVFilterContext *ctx)
{
    FRAMERATEContext *priv_ctx = ctx->priv;

    for (int i = frst + 1; i > last; i++) {
        if (priv_ctx->srce[i] && (priv_ctx->srce[i] != priv_ctx->srce[i + 1])) avfilter_unref_buffer(priv_ctx->srce[i]);
    }
    if (priv_ctx->srce[last]) avfilter_unref_buffer(priv_ctx->srce[last]);
}

static int query_formats(AVFilterContext *ctx)
{
    static const enum PixelFormat pix_fmts[] = {
        PIX_FMT_YUV422P,
        PIX_FMT_YUV420P,
        PIX_FMT_YUV411P,
        PIX_FMT_YUV410P,
        PIX_FMT_NONE
    };

    avfilter_set_common_pixel_formats(ctx, avfilter_make_format_list(pix_fmts));

    return 0;
}

/** public input methods */

static int config_input(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    FRAMERATEContext *priv_ctx = ctx->priv;
    const AVPixFmtDescriptor *pix_desc = &av_pix_fmt_descriptors[inlink->format];

    /** full an array with the number of bytes that the video
     *  data occupies per line for each plane of the input video */
    for (int plane = 0; plane < 4; plane++) {
        priv_ctx->line_size[plane] = av_image_get_linesize(
                inlink->format,
                inlink->w,
                plane);
    }

    priv_ctx->vsub = pix_desc->log2_chroma_h;

    if (CONFIG_AVCODEC) {
        priv_ctx->avctx = avcodec_alloc_context3(NULL);
        if (!priv_ctx->avctx) return AVERROR(ENOMEM);
        dsputil_init(&priv_ctx->c, priv_ctx->avctx);
    }

    priv_ctx->srce_time_base = inlink->time_base;
    priv_ctx->srce_w = inlink->w;
    priv_ctx->srce_h = inlink->h;

    return 0;
}

static void start_frame(AVFilterLink *inlink, AVFilterBufferRef *inpicref)
{
    AVFilterContext *ctx = inlink->dst;
    FRAMERATEContext *priv_ctx = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];

#ifdef DEBUG
    av_dlog(ctx, "start_frame()\n");
#endif

    // we have one new frame
    priv_ctx->pending_srce_frames++;

    if (inpicref->video->interlaced) {
        av_log(ctx, AV_LOG_WARNING, "Interlaced frame found - the output will not be correct\n");
    }
    // store the pointer to the new frame
    priv_ctx->srce[frst] = avfilter_ref_buffer(inpicref, ~0);

    if (!priv_ctx->pending_end_frame && priv_ctx->srce[crnt]) {
        int width, height;

#ifdef DEBUG
        av_dlog(ctx, "start_frame() WILL be able to output a frame\n");
#endif
        // get work-space for output frame
        width  = FFALIGN(inlink->w,   32);
        height = FFALIGN(inlink->h+2, 32);
        priv_ctx->work = avfilter_get_video_buffer(
                inlink,
                AV_PERM_WRITE,
                width,
                height);
        avfilter_copy_buffer_ref_props(priv_ctx->work, priv_ctx->srce[crnt]);
        set_work_frame_pts(ctx);

#ifdef DEBUG
        av_dlog(ctx, "start_frame() call next filter's start_frame()\n");
#endif
        avfilter_start_frame(outlink, priv_ctx->work);
        priv_ctx->pending_end_frame = 1;
    } else {
#ifdef DEBUG
        av_dlog(ctx, "start_frame() NOT able to output a frame\n");
#endif
        set_srce_frame_dest_pts(ctx);
    }
}

static void null_draw_slice(AVFilterLink *inlink, int y, int h, int slice_dir) { }

static void end_frame(AVFilterLink *link)
{
    AVFilterContext *ctx = link->dst;
    FRAMERATEContext *priv_ctx = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];
    AVFilterBufferRef *inpicref = link->cur_buf;

#ifdef DEBUG
    av_dlog(ctx, "end_frame()\n");
#endif

//    if (!priv_ctx->srce[crnt]) {
//        av_log(ctx, AV_LOG_DEBUG, "end_frame() no current frame\n");
//        return;
//    }

    if (process_work_frame(ctx)) {
#ifdef DEBUG
        av_dlog(ctx, "end_frame() we are outputing a frame\n");
#endif
        priv_ctx->dest_frame_num++;
        priv_ctx->last_dest_frame_pts = priv_ctx->work->pts;

#ifdef DEBUG
        av_dlog(ctx, "end_frame() call next filter's draw_slice() and end_frame()\n");
#endif
        avfilter_draw_slice(outlink, 0, outlink->h, 1);
        avfilter_end_frame(outlink);
        priv_ctx->pending_end_frame = 0;
    }
    avfilter_unref_buffer(inpicref);
}

/** public output methods */

static int config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    FRAMERATEContext *priv_ctx = ctx->priv;
    int exact;

#ifdef DEBUG
    av_dlog(ctx, "config_output()\n");
#endif

#ifdef DEBUG
    av_dlog(ctx,
           "config_output() input time base:%u/%u (%f)\n",
           ctx->inputs[0]->time_base.num,ctx->inputs[0]->time_base.den,
           av_q2d(ctx->inputs[0]->time_base));
#endif

    // make sure timebase is small enough to hold the framerate

    exact = av_reduce(&priv_ctx->dest_time_base.num, &priv_ctx->dest_time_base.den,
                      av_gcd((int64_t)priv_ctx->srce_time_base.num * priv_ctx->dest_frame_rate.num,
                             (int64_t)priv_ctx->srce_time_base.den * priv_ctx->dest_frame_rate.den ),
                      (int64_t)priv_ctx->srce_time_base.den * priv_ctx->dest_frame_rate.num, INT_MAX);

    av_log(ctx, AV_LOG_INFO,
           "time base:%u/%u -> %u/%u exact:%d\n",
           priv_ctx->srce_time_base.num, priv_ctx->srce_time_base.den,
           priv_ctx->dest_time_base.num, priv_ctx->dest_time_base.den, exact);
    if (!exact) {
        av_log(ctx, AV_LOG_WARNING, "Timebase conversion is not exact\n");
    }

    outlink->time_base = priv_ctx->dest_time_base;
    outlink->w = priv_ctx->srce_w;
    outlink->h = priv_ctx->srce_h;

#ifdef DEBUG
    av_dlog(ctx,
           "config_output() output time base:%u/%u (%f) w:%d h:%d\n",
           outlink->time_base.num, outlink->time_base.den,
           av_q2d(outlink->time_base),
           outlink->w, outlink->h);
#endif


    av_log(ctx, AV_LOG_INFO, "fps -> fps:%u/%u scene score:%f interpolate start:%d end:%d\n",
            priv_ctx->dest_frame_rate.num, priv_ctx->dest_frame_rate.den,
            priv_ctx->scene_score, priv_ctx->interp_start, priv_ctx->interp_end);

    return 0;
}

static int poll_frame(AVFilterLink *outlink, int flush)
{
    AVFilterContext *ctx = outlink->src;
    FRAMERATEContext *priv_ctx = ctx->priv;

#ifdef DEBUG
    av_dlog(ctx, "poll_frame()\n");
#endif

    // the destination filter is polling us for how many frames we have

    // if we have enough pending_input_frames process then say so
    if (priv_ctx->pending_srce_frames > prev) {
#ifdef DEBUG
        av_dlog(ctx, "poll_frame() pending > %d so we return 1\n", prev);
#endif
        return 1;
    }

    // otherwise if we are pending and flush is enabled
    if (priv_ctx->pending_srce_frames && flush) {
#ifdef DEBUG
        av_dlog(ctx, "poll_frame() pending and flush, we set flush and return 1\n");
#endif
        priv_ctx->flush = flush;
        return 1;
    }

    //  otherwise if we are not in flush then we need a frame, so poll our source
    if (!priv_ctx->flush) {
        int val;

#ifdef DEBUG
        av_dlog(ctx, "poll_frame() call source's poll_frame()\n");
#endif
        if ((val = avfilter_poll_frame(outlink->src->inputs[0], flush))) {
#ifdef DEBUG
            av_dlog(ctx, "poll_frame() source's poll_frame() returned %d so we return:1\n", val);
#endif
            return 1;
        }
#ifdef DEBUG
        av_dlog(ctx, "poll_frame() source's poll_frame() returned %d so we return:0\n", val);
#endif
        return 0;
    }

    // otherwise we're cannot return a frame
#ifdef DEBUG
    av_dlog(ctx, "poll_frame() we return 0\n");
#endif
    return 0;
}

static int request_frame(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    FRAMERATEContext *priv_ctx = ctx->priv;
    int val, width, height, i;

#ifdef DEBUG
    av_dlog(ctx, "request_frame()\n");
#endif

    // if there is no "next" frame AND we are not in flush
    // then get a frame from our input filter
    if (!priv_ctx->srce[frst] && !priv_ctx->flush) {
#ifdef DEBUG
        av_dlog(ctx, "request_frame() call source's request_frame()\n");
#endif
        if ((val = avfilter_request_frame(outlink->src->inputs[0])) < 0) {
#ifdef DEBUG
            av_dlog(ctx, "request_frame() source's request_frame() returned error:%d\n", val);
#endif
            return val;
        }
#ifdef DEBUG
        av_dlog(ctx, "request_frame() source's request_frame() returned:%d\n", val);
#endif
        return 0;
    }

#ifdef DEBUG
    av_dlog(ctx, "request_frame() REPEAT or FLUSH\n");
#endif

    if (priv_ctx->pending_srce_frames <= 0) {
#ifdef DEBUG
        av_dlog(ctx, "request_frame() nothing else to do, return:EOF\n");
#endif
        return AVERROR(AVERROR_EOF);
    }

    // otherwise, make brand-new frame and pass to our output filter
#ifdef DEBUG
    av_dlog(ctx, "request_frame() FLUSH\n");
#endif

    // back fill at end of file when source has no more frames
    for (i = last; i > frst; i--) {
        if (!priv_ctx->srce[i - 1] && priv_ctx->srce[i]) {
#ifdef DEBUG
            av_dlog(ctx, "request_frame() copy:%d to:%d\n", i, i - 1);
#endif
            priv_ctx->srce[i - 1] = priv_ctx->srce[i];
        }
    }

    // get work-space for output frame
    width  = FFALIGN(outlink->w,   32);
    height = FFALIGN(outlink->h+2, 32);
    priv_ctx->work = avfilter_get_video_buffer(
            outlink,
            AV_PERM_WRITE,
            width,
            height);
    avfilter_copy_buffer_ref_props(priv_ctx->work, priv_ctx->srce[crnt]);
    set_work_frame_pts(ctx);

    avfilter_start_frame(outlink, priv_ctx->work);

    if (process_work_frame(ctx)) {
#ifdef DEBUG
        av_dlog(ctx, "request_frame() we are outputing a frame\n");
#endif
        priv_ctx->dest_frame_num++;
        priv_ctx->last_dest_frame_pts = priv_ctx->work->pts;

#ifdef DEBUG
        av_dlog(ctx, "request_frame() call next filter's draw_slice() and end_frame()\n");
#endif
        avfilter_draw_slice(outlink, 0, outlink->h, 1);
        avfilter_end_frame(outlink);
    }
    return 0;
}

AVFilter avfilter_vf_framerate = {
    .name          = "framerate",
    .description   = NULL_IF_CONFIG_SMALL("Upsamples or downsamples progressive source between specified frame rates"),
    .priv_size     = sizeof(FRAMERATEContext),

    .init          = init,
    .uninit        = uninit,
    .query_formats = query_formats,
    .inputs        = (const AVFilterPad[]) {{
                                            .name           = "default",
                                            .type           = AVMEDIA_TYPE_VIDEO,
                                            .config_props   = config_input,
                                            .start_frame    = start_frame,
                                            .draw_slice     = null_draw_slice,
                                            .end_frame      = end_frame,
                                            .min_perms      = AV_PERM_READ, },
                                            {.name = NULL}},
    .outputs       = (const AVFilterPad[]) {{
                                            .name           = "default",
                                            .type           = AVMEDIA_TYPE_VIDEO,
                                            .config_props   = config_output,
                                            .poll_frame     = poll_frame,
                                            .request_frame  = request_frame, },
                                           {.name = NULL}},
};
