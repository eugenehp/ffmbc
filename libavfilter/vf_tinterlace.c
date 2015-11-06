/*
 * Copyright (c) 2011 Stefano Sabatini
 * Copyright (c) 2010 Baptiste Coudurier
 * Copyright (c) 2003 Michael Zucchi <notzed@ximian.com>
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with FFmpeg if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

/**
 * @file
 * temporal field interlace filter, ported from MPlayer/libmpcodecs
 */

#include "libavutil/avassert.h"
#include "libavutil/imgutils.h"
#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"
#include "avfilter.h"
#include "internal.h"

enum TInterlaceMode {
    MODE_MERGE = 0,
    MODE_DROP_EVEN,
    MODE_DROP_ODD,
    MODE_PAD,
    MODE_INTERLEAVE_TOP,
    MODE_INTERLEAVE_BOTTOM,
    MODE_INTERLACEX2,
    MODE_NB,
};

typedef struct {
    const AVClass *class;
    enum TInterlaceMode mode;   ///< interlace mode selected
    int flags;                  ///< flags affecting interlacing algorithm
    int frame;                  ///< number of the output frame
    int vsub;                   ///< chroma vertical subsampling
    uint8_t *black_data[4];     ///< buffer used to fill padded lines
    int black_linesize[4];
    AVFilterBufferRef *cur;
    AVFilterBufferRef *next;
} TInterlaceContext;

#define OFFSET(x) offsetof(TInterlaceContext, x)
#define TINTERLACE_FLAG_VLPF 01

static const AVOption tinterlace_options[] = {
    {"mode",              "select interlace mode",           OFFSET(mode),  FF_OPT_TYPE_INT,   {.dbl=MODE_MERGE}, 0, MODE_NB-1 },

    {"flags",             "set flags",                       OFFSET(flags), FF_OPT_TYPE_FLAGS, {.dbl=0}, 0, INT_MAX, 0, "flags" },
    {"low_pass_filter",   "enable vertical low-pass filter", 0,             FF_OPT_TYPE_CONST, {.dbl=TINTERLACE_FLAG_VLPF}, INT_MIN, INT_MAX, 0, "flags" },
    {"vlpf",              "enable vertical low-pass filter", 0,             FF_OPT_TYPE_CONST, {.dbl=TINTERLACE_FLAG_VLPF}, INT_MIN, INT_MAX, 0, "flags" },

    {NULL}
};

static const char *tinterlace_get_name(void *ctx)
{
    return "tinterlace";
}

static const AVClass tinterlace_class = {
    "DrawTextContext",
    tinterlace_get_name,
    tinterlace_options
};

#define FULL_SCALE_YUVJ_FORMATS \
        PIX_FMT_YUVJ420P, PIX_FMT_YUVJ422P, PIX_FMT_YUVJ444P

static enum PixelFormat full_scale_yuvj_pix_fmts[] = {
    FULL_SCALE_YUVJ_FORMATS, PIX_FMT_NONE
};

static av_cold int init(AVFilterContext *ctx, const char *args, void *opaque)
{
    TInterlaceContext *tinterlace = ctx->priv;
    char c;
    int count, err;

    tinterlace->class = &tinterlace_class;
    av_opt_set_defaults2(tinterlace, 0, 0);

    if (args) {
        count = sscanf(args, "%d%c", (int *)&tinterlace->mode, &c);
        if (count >= 1) { // found a number possibly followed by a non-number
            if (tinterlace->mode > MODE_INTERLACEX2) {
                av_log(ctx, AV_LOG_ERROR,
                       "Invalid mode '%s', use an integer between 0 and 6\n", args);
                return AVERROR(EINVAL);
            }
        }
        if (count > 1) { // move args on to the character after the non-number
            if ((args = strchr(args,c))) {
                args++;
            }
        }
        if (args && count != 1) { // parse the rest of the args
            if ((err = (av_set_options_string(tinterlace, args, "=", ":"))) < 0) {
                av_log(ctx, AV_LOG_ERROR, "Error parsing options string: '%s'\n", args);
                return err;
            }
        }
    }

    return 0;
}

static int query_formats(AVFilterContext *ctx)
{
    static const enum PixelFormat pix_fmts[] = {
        PIX_FMT_YUV420P,
        PIX_FMT_YUV422P,
        PIX_FMT_YUV444P,
        FULL_SCALE_YUVJ_FORMATS,
        PIX_FMT_NONE
    };

    avfilter_set_common_pixel_formats(ctx, avfilter_make_format_list(pix_fmts));

    return 0;
}

static av_cold void uninit(AVFilterContext *ctx)
{
    TInterlaceContext *tinterlace = ctx->priv;

    if (tinterlace->cur ) avfilter_unref_buffer(tinterlace->cur );
    if (tinterlace->next) avfilter_unref_buffer(tinterlace->next);
}

static int config_input(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    TInterlaceContext *tinterlace = ctx->priv;
    const AVPixFmtDescriptor *pix_desc = &av_pix_fmt_descriptors[inlink->format];

    tinterlace->vsub = pix_desc->log2_chroma_h;

    return 0;
}

static int config_output(AVFilterLink *outlink)
{
    TInterlaceContext *tinterlace = outlink->src->priv;
    const AVPixFmtDescriptor *desc = &av_pix_fmt_descriptors[outlink->format];
    uint8_t black[4] = { 16, 128, 128, 16 };
    int i, ret;

    switch (tinterlace->mode) {
    case MODE_PAD:
        if (ff_fmt_is_in(outlink->format, full_scale_yuvj_pix_fmts))
            black[0] = black[3] = 0;
        ret = av_image_alloc(tinterlace->black_data, tinterlace->black_linesize,
                             outlink->w, outlink->h, outlink->format, 1);
        if (ret < 0)
            return ret;

        /* fill black picture with black */
        for (i = 0; i < 4 && tinterlace->black_data[i]; i++) {
            int h = i == 1 || i == 2 ? outlink->h >> desc->log2_chroma_h : outlink->h;
            memset(tinterlace->black_data[i], black[i],
                   tinterlace->black_linesize[i] * h);
        }
        // fall
    case MODE_MERGE:
        outlink->h = outlink->src->inputs[0]->h*2;
        break;
    case MODE_DROP_EVEN:        /* odd frames */
    case MODE_DROP_ODD:         /* even frames */
    case MODE_INTERLEAVE_TOP:   /* alternate frame (height-preserving) interlacing */
    case MODE_INTERLEAVE_BOTTOM:
    case MODE_INTERLACEX2:
        outlink->h = outlink->src->inputs[0]->h;
        break;
    }
    if ((tinterlace->flags & TINTERLACE_FLAG_VLPF)
            && !(tinterlace->mode == MODE_INTERLEAVE_TOP
              || tinterlace->mode == MODE_INTERLEAVE_BOTTOM)) {
        av_log(tinterlace, AV_LOG_WARNING, "low_pass_filter flag ignored with mode %d\n",
                tinterlace->mode);
        tinterlace->flags &= !TINTERLACE_FLAG_VLPF;
    }

    outlink->w = outlink->src->inputs[0]->w;

    av_log(tinterlace, AV_LOG_INFO, "mode:%d filter:%s\n",
            tinterlace->mode, (tinterlace->flags & TINTERLACE_FLAG_VLPF) ? "on" : "off");

    return 0;
}

#define FIELD_UPPER           0
#define FIELD_LOWER           1
#define FIELD_UPPER_AND_LOWER 2

/**
 * Copy picture field from src to dst.
 *
 * @param src_field copy from upper, lower field or both
 * @param interleave leave a padding line between each copied line
 * @param dst_field copy to upper or lower field,
 *        only meaningful when interleave is selected
 * @param flags context flags
 */
static inline
void copy_picture_field(uint8_t *dst[4], int dst_linesize[4],
                        const uint8_t *src[4], int src_linesize[4],
                        enum PixelFormat format, int w, int src_h,
                        int src_field, int interleave, int dst_field,
                        int flags)
{
    const AVPixFmtDescriptor *desc = &av_pix_fmt_descriptors[format];
    int plane, vsub = desc->log2_chroma_h;
    int k = src_field == FIELD_UPPER_AND_LOWER ? 1 : 2;
    int h, i;

    for (plane = 0; plane < desc->nb_components; plane++) {
        int lines = plane == 1 || plane == 2 ? src_h >> vsub : src_h;
        int linesize = av_image_get_linesize(format, w, plane);
        uint8_t *dstp = dst[plane];
        const uint8_t *srcp = src[plane];

        if (linesize < 0)
            return;

        lines /= k;
        if (src_field == FIELD_LOWER)
            srcp += src_linesize[plane];
        if (interleave && dst_field == FIELD_LOWER)
            dstp += dst_linesize[plane];
        if (flags & TINTERLACE_FLAG_VLPF) {
            // Low-pass filtering is required when creating an interlaced destination from
            // a progressive source which contains high-frequency vertical detail.
            // Filtering will reduce interlace 'twitter' and Moire patterning.
            int srcp_linesize = src_linesize[plane] * k;
            int dstp_linesize = dst_linesize[plane] * (interleave ? 2 : 1);
            for (h = lines; h > 0; h--) {
                const uint8_t *srcp_above = srcp - src_linesize[plane];
                const uint8_t *srcp_below = srcp + src_linesize[plane];
                if (h == lines) srcp_above = srcp; // there is no line above
                if (h == 1) srcp_below = srcp;     // there is no line below
                for (i = 0; i < linesize; i++) {
                    // this calculation is an integer representation of
                    // '0.5 * current + 0.25 * above + 0.25 + below'
                    // '1 +' is for rounding. */
                    dstp[i] = (1 + srcp[i] + srcp[i] + srcp_above[i] + srcp_below[i]) >> 2;
                }
                dstp += dstp_linesize;
                srcp += srcp_linesize;
            }
        } else {
            av_image_copy_plane(dstp, dst_linesize[plane] * (interleave ? 2 : 1),
                            srcp, src_linesize[plane]*k, linesize, lines);
        }
    }
}

static void start_frame(AVFilterLink *inlink, AVFilterBufferRef *picref)
{
    AVFilterContext *ctx = inlink->dst;
    TInterlaceContext *tinterlace = ctx->priv;

    avfilter_unref_buffer(tinterlace->cur);
    tinterlace->cur  = tinterlace->next;
    tinterlace->next = picref;
}

static void end_frame(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    AVFilterLink *outlink = ctx->outputs[0];
    TInterlaceContext *tinterlace = ctx->priv;
    AVFilterBufferRef *cur  = tinterlace->cur;
    AVFilterBufferRef *next = tinterlace->next;
    AVFilterBufferRef *out  = NULL;
    int field, tff;

    /* we need at least two frames */
    if (!tinterlace->cur)
        return;

    switch (tinterlace->mode) {
    case MODE_MERGE: /* move the odd frame into the upper field of the new image, even into
             * the lower field, generating a double-height video at half framerate */
        out = avfilter_get_video_buffer(outlink, AV_PERM_WRITE, outlink->w, outlink->h);
        avfilter_copy_buffer_ref_props(out, cur);
        out->video->h = outlink->h;
        out->video->interlaced = 1;
        out->video->top_field_first = 1;

        /* write odd frame lines into the upper field of the new frame */
        copy_picture_field(out->data, out->linesize,
                           (const uint8_t **)cur->data, cur->linesize,
                           inlink->format, inlink->w, inlink->h,
                           FIELD_UPPER_AND_LOWER, 1, FIELD_UPPER, tinterlace->flags);
        /* write even frame lines into the lower field of the new frame */
        copy_picture_field(out->data, out->linesize,
                           (const uint8_t **)next->data, next->linesize,
                           inlink->format, inlink->w, inlink->h,
                           FIELD_UPPER_AND_LOWER, 1, FIELD_LOWER, tinterlace->flags);
        avfilter_unref_buffer(tinterlace->next);
        break;

    case MODE_DROP_ODD:  /* only output even frames, odd  frames are dropped; height unchanged, half framerate */
    case MODE_DROP_EVEN: /* only output odd  frames, even frames are dropped; height unchanged, half framerate */
        out = avfilter_ref_buffer(tinterlace->mode == MODE_DROP_EVEN ? cur : next, AV_PERM_READ);
        avfilter_unref_buffer(tinterlace->next);
        tinterlace->next = NULL;
        break;

    case MODE_PAD: /* expand each frame to double height, but pad alternate
                    * lines with black; framerate unchanged */
        out = avfilter_get_video_buffer(outlink, AV_PERM_WRITE, outlink->w, outlink->h);
        avfilter_copy_buffer_ref_props(out, cur);
        out->video->h = outlink->h;

        field = (1 + tinterlace->frame) & 1 ? FIELD_UPPER : FIELD_LOWER;
        /* copy upper and lower fields */
        copy_picture_field(out->data, out->linesize,
                           (const uint8_t **)cur->data, cur->linesize,
                           inlink->format, inlink->w, inlink->h,
                           FIELD_UPPER_AND_LOWER, 1, field, tinterlace->flags);
        /* pad with black the other field */
        copy_picture_field(out->data, out->linesize,
                           (const uint8_t **)tinterlace->black_data, tinterlace->black_linesize,
                           inlink->format, inlink->w, inlink->h,
                           FIELD_UPPER_AND_LOWER, 1, !field, tinterlace->flags);
        break;

        /* interleave upper/lower lines from odd frames with lower/upper lines from even frames,
         * halving the frame rate and preserving image height */
    case MODE_INTERLEAVE_TOP:    /* top    field first */
    case MODE_INTERLEAVE_BOTTOM: /* bottom field first */
        tff = tinterlace->mode == MODE_INTERLEAVE_TOP;
        out = avfilter_get_video_buffer(outlink, AV_PERM_WRITE, outlink->w, outlink->h);
        avfilter_copy_buffer_ref_props(out, cur);
        out->video->interlaced = 1;
        out->video->top_field_first = tff;

        /* copy upper/lower field from cur */
        copy_picture_field(out->data, out->linesize,
                           (const uint8_t **)cur->data, cur->linesize,
                           inlink->format, inlink->w, inlink->h,
                           tff ? FIELD_UPPER : FIELD_LOWER, 1, tff ? FIELD_UPPER : FIELD_LOWER,
                           tinterlace->flags);
        /* copy lower/upper field from next */
        copy_picture_field(out->data, out->linesize,
                           (const uint8_t **)next->data, next->linesize,
                           inlink->format, inlink->w, inlink->h,
                           tff ? FIELD_LOWER : FIELD_UPPER, 1, tff ? FIELD_LOWER : FIELD_UPPER,
                           tinterlace->flags);
        avfilter_unref_buffer(tinterlace->next);
        tinterlace->next = NULL;
        break;
    case MODE_INTERLACEX2: /* re-interlace preserving image height, double frame rate */
        /* output current frame first */
        out = avfilter_ref_buffer(cur, ~AV_PERM_WRITE);
        out->video->interlaced = 1;

        avfilter_start_frame(outlink, out);
        avfilter_draw_slice(outlink, 0, outlink->h, 1);
        avfilter_end_frame(outlink);

        /* output mix of current and next frame */
        tff = next->video->top_field_first;
        out = avfilter_get_video_buffer(outlink, AV_PERM_WRITE, outlink->w, outlink->h);
        avfilter_copy_buffer_ref_props(out, next);
        out->video->interlaced = 1;

        /* write current frame second field lines into the second field of the new frame */
        copy_picture_field(out->data, out->linesize,
                           (const uint8_t **)cur->data, cur->linesize,
                           inlink->format, inlink->w, inlink->h,
                           tff ? FIELD_LOWER : FIELD_UPPER, 1, tff ? FIELD_LOWER : FIELD_UPPER,
                           tinterlace->flags);
        /* write next frame first field lines into the first field of the new frame */
        copy_picture_field(out->data, out->linesize,
                           (const uint8_t **)next->data, next->linesize,
                           inlink->format, inlink->w, inlink->h,
                           tff ? FIELD_UPPER : FIELD_LOWER, 1, tff ? FIELD_UPPER : FIELD_LOWER,
                           tinterlace->flags);
        break;
    default:
        av_assert0(0);
    }

    avfilter_start_frame(outlink, out);
    avfilter_draw_slice(outlink, 0, outlink->h, 1);
    avfilter_end_frame(outlink);

    tinterlace->frame++;
}

static int poll_frame(AVFilterLink *link, int flush)
{
    TInterlaceContext *tinterlace = link->src->priv;
    int ret, val;

    val = avfilter_poll_frame(link->src->inputs[0], flush);

    if (val==1 && !tinterlace->next) { //FIXME change API to not requre this red tape
        if ((ret = avfilter_request_frame(link->src->inputs[0])) < 0)
            return ret;
        val = avfilter_poll_frame(link->src->inputs[0], flush);
    }
    assert(tinterlace->next);

    return val;
}

static void null_draw_slice(AVFilterLink *link, int y, int h, int slice_dir) { }

AVFilter avfilter_vf_tinterlace = {
    .name          = "tinterlace",
    .description   = NULL_IF_CONFIG_SMALL("Temporal field interlacing"),

    .priv_size     = sizeof(TInterlaceContext),
    .init          = init,
    .uninit        = uninit,
    .query_formats = query_formats,

    .inputs    = (AVFilterPad[]) {{ .name             = "default",
                                    .type             = AVMEDIA_TYPE_VIDEO,
                                    .config_props     = config_input,
                                    .start_frame      = start_frame,
                                    .draw_slice       = null_draw_slice,
                                    .end_frame        = end_frame, },
                                  { .name = NULL}},

    .outputs   = (AVFilterPad[]) {{ .name             = "default",
                                    .type             = AVMEDIA_TYPE_VIDEO,
                                    .config_props     = config_output,
                                    .poll_frame       = poll_frame, },
                                  { .name = NULL}},
};
