/*
 * Copyright (c) 2012 Mark Himsley
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

/**
 * @file
 * Repeat the first, or specified, frame in a file, creates pts etc
 */

/* #define DEBUG */

#include "avfilter.h"
#include "libavutil/imgutils.h"
#include "libavutil/parseutils.h"
#include "libavutil/pixdesc.h"

typedef struct {
    AVFilterBufferRef *srce;            ///< buffered source frame

    AVRational requested_frame_rate;    ///< requested output frames per second

    AVRational input_time_base;         ///< timebase of source
    double input_start_pts;             ///< pts of the first frame
    int input_w, input_h;

    AVRational output_time_base;        ///< calculated timebase of output
    double output_frame_rate;           ///< output frames per second as double
    int64_t output_frame_number;        ///< output frame number starting from zero

    char rate_expr[256];
} REPEATContext;

/** public general methods */

static av_cold int init(AVFilterContext *ctx, const char *args, void *opaque)
{
    REPEATContext *priv_ctx = ctx->priv;

    AVRational rate = (AVRational){50, 1};

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG, "init()\n");
#endif

    if (args) {
        if (sscanf(args, "%255[^:]", priv_ctx->rate_expr) != 1) {
            av_log(ctx, AV_LOG_ERROR, "Invalid arguments '%s'.\n", args);
            return AVERROR(EINVAL);
        }
        if (av_parse_video_rate(&rate, priv_ctx->rate_expr) < 0) {
            av_log(ctx, AV_LOG_ERROR, "Invalid input rate '%s'.\n", priv_ctx->rate_expr);
            return AVERROR(EINVAL);
        }
    }

    priv_ctx->requested_frame_rate = rate;
    priv_ctx->output_frame_rate = av_q2d(rate);

    av_log(ctx, AV_LOG_INFO,
           "output fps:%u/%u (%f)\n",
           rate.num, rate.den,
           priv_ctx->output_frame_rate);

    return 0;
}

static av_cold void uninit(AVFilterContext *ctx)
{
    REPEATContext *priv_ctx = ctx->priv;

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG, "uninit()\n");
#endif

    if (priv_ctx->srce) avfilter_unref_buffer(priv_ctx->srce);
}

/** public input methods */

static int config_input(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    REPEATContext *priv_ctx = ctx->priv;

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG, "config_input()\n");
#endif

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG,
           "config_input() input time base:%u/%u (%f)\n",
           inlink->time_base.num,inlink->time_base.den,
           av_q2d(inlink->time_base));
#endif

    priv_ctx->input_time_base = inlink->time_base;
    priv_ctx->input_w = inlink->w;
    priv_ctx->input_h = inlink->h;

    return 0;
}

#define D2TS(d)  (isnan(d) ? AV_NOPTS_VALUE : (int64_t)(d))
#define TS2D(ts) ((ts) == AV_NOPTS_VALUE ? NAN : (double)(ts))

static void create_frame(AVFilterContext *ctx, AVFilterLink *outlink)
{
    REPEATContext *priv_ctx = ctx->priv;

    AVFilterBufferRef *outpicref;
    double out_pts;

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG, "create_frame()\n");
#endif

    avfilter_unref_buffer(outlink->out_buf);
    outpicref = avfilter_ref_buffer(priv_ctx->srce, ~0);
    out_pts = priv_ctx->input_start_pts + priv_ctx->output_frame_number / av_q2d(priv_ctx->output_time_base) / priv_ctx->output_frame_rate;
    outpicref->pts = D2TS(out_pts);

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG,
           "create_frame() n:%"PRId64" output pts:%"PRId64" pts_time:%f tb:%d/%d\n",
           priv_ctx->output_frame_number,
           outpicref->pts,
           outpicref->pts * av_q2d(priv_ctx->output_time_base),
           outlink->time_base.num, outlink->time_base.den);
#endif

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG, "create_frame() call destination's start_frame()\n");
#endif
    avfilter_start_frame(outlink, outpicref);
    priv_ctx->output_frame_number += 1;
}

static void start_frame(AVFilterLink *inlink, AVFilterBufferRef *inpicref)
{
    AVFilterContext *ctx = inlink->dst;
    REPEATContext *priv_ctx = ctx->priv;

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG, "start_frame()\n");
#endif

    if (isnan(priv_ctx->input_start_pts))
        priv_ctx->input_start_pts = TS2D(inpicref->pts);

    if (!priv_ctx->srce)
        priv_ctx->srce = avfilter_ref_buffer(inpicref, ~0);

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG,
           "start_frame() n:%"PRId64" input pts:%"PRId64" t:%f\n",
           priv_ctx->output_frame_number,
           inpicref ->pts,
           inpicref ->pts * av_q2d(inlink->time_base));
#endif

    create_frame(ctx, inlink->dst->outputs[0]);
}

/** public output methods */

static int config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    REPEATContext *priv_ctx = ctx->priv;
    AVRational tb;
    int exact;

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG, "config_output()\n");
#endif

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG,
           "config_output() input time base:%u/%u (%f)\n",
           ctx->inputs[0]->time_base.num,ctx->inputs[0]->time_base.den,
           av_q2d(ctx->inputs[0]->time_base));
#endif

    // make sure timebase is small enough to hold the framerate

    exact = av_reduce(&tb.num, &tb.den,
                      av_gcd((int64_t)priv_ctx->input_time_base.num * priv_ctx->requested_frame_rate.num,
                             (int64_t)priv_ctx->input_time_base.den * priv_ctx->requested_frame_rate.den ),
                      (int64_t)priv_ctx->input_time_base.den * priv_ctx->requested_frame_rate.num, INT_MAX);

    av_log(ctx, AV_LOG_INFO,
           "time base:%u/%u -> %u/%u exact:%d\n",
           priv_ctx->input_time_base.num, priv_ctx->input_time_base.den,
           tb.num, tb.den, exact);
    if (!exact) {
        av_log(ctx, AV_LOG_WARNING, "Timebase conversion is not exact\n");
    }

    outlink->time_base = priv_ctx->output_time_base = tb;
    outlink->w = priv_ctx->input_w;
    outlink->h = priv_ctx->input_h;

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG,
           "config_output() output time base:%u/%u (%f) w:%d h:%d\n",
           outlink->time_base.num, outlink->time_base.den,
           av_q2d(outlink->time_base),
           outlink->w, outlink->h);
#endif

    return 0;
}

static int poll_frame(AVFilterLink *outlink, int flush)
{
    AVFilterContext *ctx = outlink->src;
    REPEATContext *priv_ctx = ctx->priv;

    int val;

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG, "poll_frame()\n");
#endif

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG, "poll_frame() call source's poll_frame()\n");
#endif
    if ((val = avfilter_poll_frame(outlink->src->inputs[0], flush))) {
#ifdef DEBUG
        av_log(ctx, AV_LOG_DEBUG, "poll_frame() source's poll_frame() returned %d\n", val);
#endif
        if (!priv_ctx->srce || (val > 0 && val < INT_MAX)) {
#ifdef DEBUG
            av_log(ctx, AV_LOG_DEBUG, "poll_frame() we return source's poll_frame() value %d\n", val);
#endif
            return val;
        }
    }

    if (priv_ctx->srce) {
#ifdef DEBUG
        av_log(ctx, AV_LOG_DEBUG, "poll_frame() we have a frame so we can repeat it - return 1\n");
#endif
        return 1;
    }
    // otherwise we're cannot return a frame
#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG, "poll_frame() we return 0\n");
#endif

    return 0;
}

static int request_frame(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    REPEATContext *priv_ctx = ctx->priv;

    AVFilterLink *inlink = ctx->inputs[0];

    int val;

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG, "request_frame()\n");
#endif

    // first frame
    if (!priv_ctx->srce) {
#ifdef DEBUG
        av_log(ctx, AV_LOG_DEBUG, "request_frame() no cached frame so call source's request_frame()\n");
#endif
        if ((val = avfilter_request_frame(inlink)) < 0) {
#ifdef DEBUG
            av_log(ctx, AV_LOG_DEBUG, "request_frame() source's request_frame() returned error %d\n", val);
#endif
            return val;
        }
#ifdef DEBUG
        av_log(ctx, AV_LOG_DEBUG, "request_frame() source's request_frame() returned %d\n", val);
#endif
        return 0;
    }

    // else repeated the frame

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG, "request_frame() we are outputing a repeated frame - call our create_frame()\n");
#endif
    create_frame(ctx, outlink);
#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG, "request_frame() call destination's draw_slice()\n");
#endif
    avfilter_draw_slice(outlink, 0, outlink->h, 1);
#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG, "request_frame() call destination's end_frame()\n");
#endif
    avfilter_end_frame(outlink);

    return 0;
}

AVFilter avfilter_vf_repeatframe = {
    .name          = "repeatframe",
    .description   = NULL_IF_CONFIG_SMALL("Repeats the first, or specified, frame in a file"),
    .priv_size     = sizeof(REPEATContext),
    .init          = init,
    .uninit        = uninit,
    .inputs        = (const AVFilterPad[]) {{
                                            .name             = "default",
                                            .type             = AVMEDIA_TYPE_VIDEO,
                                            .config_props     = config_input,
                                            .start_frame      = start_frame,
                                            .min_perms        = AV_PERM_READ|AV_PERM_REUSE,
                                            },
                                            { .name = NULL}},
    .outputs       = (const AVFilterPad[]) {{
                                            .name             = "default",
                                            .type             = AVMEDIA_TYPE_VIDEO,
                                            .config_props     = config_output,
                                            .poll_frame       = poll_frame,
                                            .request_frame    = request_frame,
                                            },
                                            { .name = NULL}},
};
