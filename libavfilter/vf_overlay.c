/*
 * Copyright (c) 2012 Mark Himsley
 * Copyright (c) 2010 Stefano Sabatini
 * Copyright (c) 2010 Baptiste Coudurier
 * Copyright (c) 2007 Bobby Bingham
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation;
 * version 2 of the License.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * @file
 * overlay one video on top of another
 */

#include "avfilter.h"
#include "libavutil/eval.h"
#include "libavutil/avstring.h"
#include "libavutil/pixdesc.h"
#include "libavutil/imgutils.h"
#include "libavutil/mathematics.h"
#include "drawutils.h"
#include "internal.h"

/* #define DEBUG */

static const char *var_names[] = {
    "E",
    "PHI",
    "PI",
    "main_w",    "W", ///< width  of the main    video
    "main_h",    "H", ///< height of the main    video
    "overlay_w", "w", ///< width  of the overlay video
    "overlay_h", "h", ///< height of the overlay video
    "rgb",
    NULL
};

enum var_name {
    VAR_E,
    VAR_PHI,
    VAR_PI,
    VAR_MAIN_W,    VAR_MW,
    VAR_MAIN_H,    VAR_MH,
    VAR_OVERLAY_W, VAR_OW,
    VAR_OVERLAY_H, VAR_OH,
    VAR_RGB,
    VAR_VARS_NB
};

enum { MAIN = 0, OVERLAY };
enum { R = 0, G, B, A };
enum { Y = 0, U, V };

typedef struct {
    int x, y;                   ///< position of overlayed picture

    int allow_packed_rgb;

    uint8_t main_is_packed_rgb;
    uint8_t main_rgba_map[4];
    uint8_t main_has_alpha;
    uint8_t overlay_is_packed_rgb;
    uint8_t overlay_rgba_map[4];
    uint8_t overlay_has_alpha;

    AVFilterBufferRef *overpicref;

    int main_pix_step[4];       ///< steps per pixel for each plane of the main output
    int overlay_pix_step[4];    ///< steps per pixel for each plane of the overlay
    int hsub, vsub;             ///< chroma subsampling values

    char x_expr[256], y_expr[256], rgb_expr[256];
} OverlayContext;

static av_cold int init(AVFilterContext *ctx, const char *args, void *opaque)
{
    OverlayContext *over = ctx->priv;

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG, "init()\n");
#endif

    av_strlcpy(over->x_expr,   "0", sizeof(over->x_expr));
    av_strlcpy(over->y_expr,   "0", sizeof(over->y_expr));
    av_strlcpy(over->rgb_expr, "0", sizeof(over->rgb_expr));

    if (args)
        sscanf(args, "%255[^:]:%255[^:]:%255[^:]", over->x_expr, over->y_expr, over->rgb_expr);

    if (over->rgb_expr && strstr(over->rgb_expr,"rgb=1")) {
#ifdef DEBUG
        av_log(ctx, AV_LOG_DEBUG, "config_input_overlay() RGB\n");
#endif
        over->allow_packed_rgb = 1;
    }

    return 0;
}

static av_cold void uninit(AVFilterContext *ctx)
{
    OverlayContext *over = ctx->priv;

    if (over->overpicref)
        avfilter_unref_buffer(over->overpicref);
}

static int query_formats(AVFilterContext *ctx)
{
    OverlayContext *over = ctx->priv;

    /* overlay formats contains alpha, for avoiding conversion with alpha information loss */
    const enum PixelFormat main_pix_fmts_yuv[] = { PIX_FMT_YUV420P, PIX_FMT_YUVA420P, PIX_FMT_NONE };
    const enum PixelFormat overlay_pix_fmts_yuv[] = { PIX_FMT_YUVA420P, PIX_FMT_NONE };
    const enum PixelFormat main_pix_fmts_rgb[] = {
        PIX_FMT_ARGB,  PIX_FMT_RGBA,
        PIX_FMT_ABGR,  PIX_FMT_BGRA,
        PIX_FMT_RGB24, PIX_FMT_BGR24,
        PIX_FMT_NONE
    };
    const enum PixelFormat overlay_pix_fmts_rgb[] = {
        PIX_FMT_ARGB,  PIX_FMT_RGBA,
        PIX_FMT_ABGR,  PIX_FMT_BGRA,
        PIX_FMT_NONE
    };

    AVFilterFormats *main_formats;
    AVFilterFormats *overlay_formats;

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG, "query_formats()\n");
#endif

    if (over->allow_packed_rgb) {
        main_formats    = avfilter_make_format_list(main_pix_fmts_rgb);
        overlay_formats = avfilter_make_format_list(overlay_pix_fmts_rgb);
    } else {
        main_formats    = avfilter_make_format_list(main_pix_fmts_yuv);
        overlay_formats = avfilter_make_format_list(overlay_pix_fmts_yuv);
    }

    avfilter_formats_ref(main_formats,    &ctx->inputs [MAIN   ]->out_formats);
    avfilter_formats_ref(overlay_formats, &ctx->inputs [OVERLAY]->out_formats);
    avfilter_formats_ref(main_formats,    &ctx->outputs[MAIN   ]->in_formats );

    return 0;
}

static const enum PixelFormat alpha_pix_fmts[] = {
    PIX_FMT_YUVA420P, PIX_FMT_ARGB, PIX_FMT_ABGR, PIX_FMT_RGBA,
    PIX_FMT_BGRA, PIX_FMT_NONE
};

static int config_input_main(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    OverlayContext *over = inlink->dst->priv;
    const AVPixFmtDescriptor *pix_desc = &av_pix_fmt_descriptors[inlink->format];

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG, "config_input_main()\n");
#endif

    av_image_fill_max_pixsteps(over->main_pix_step, NULL, pix_desc);

    over->hsub = pix_desc->log2_chroma_w;
    over->vsub = pix_desc->log2_chroma_h;

    over->main_is_packed_rgb =
        ff_fill_rgba_map(over->main_rgba_map, inlink->format) >= 0;
    over->main_has_alpha = ff_fmt_is_in(inlink->format, alpha_pix_fmts);

    return 0;
}

static int config_input_overlay(AVFilterLink *inlink)
{
    AVFilterContext *ctx  = inlink->dst;
    OverlayContext  *over = inlink->dst->priv;
    char *expr;
    double var_values[VAR_VARS_NB], res;
    int ret;
    const AVPixFmtDescriptor *pix_desc = &av_pix_fmt_descriptors[inlink->format];

#ifdef DEBUG
    av_log(ctx, AV_LOG_DEBUG, "config_input_overlay()\n");
#endif

    av_image_fill_max_pixsteps(over->overlay_pix_step, NULL, pix_desc);

    /* Finish the configuration by evaluating the expressions
       now when both inputs are configured. */
    var_values[VAR_E  ] = M_E;
    var_values[VAR_PHI] = M_PHI;
    var_values[VAR_PI ] = M_PI;

    var_values[VAR_MAIN_W   ] = var_values[VAR_MW] = ctx->inputs[MAIN   ]->w;
    var_values[VAR_MAIN_H   ] = var_values[VAR_MH] = ctx->inputs[MAIN   ]->h;
    var_values[VAR_OVERLAY_W] = var_values[VAR_OW] = ctx->inputs[OVERLAY]->w;
    var_values[VAR_OVERLAY_H] = var_values[VAR_OH] = ctx->inputs[OVERLAY]->h;

    if ((ret = av_expr_parse_and_eval(&res, (expr = over->x_expr), var_names, var_values,
                                      NULL, NULL, NULL, NULL, NULL, 0, ctx)) < 0)
        goto fail;
    over->x = res;
    if ((ret = av_expr_parse_and_eval(&res, (expr = over->y_expr), var_names, var_values,
                                      NULL, NULL, NULL, NULL, NULL, 0, ctx)))
        goto fail;
    over->y = res;
    /* x may depend on y */
    if ((ret = av_expr_parse_and_eval(&res, (expr = over->x_expr), var_names, var_values,
                                      NULL, NULL, NULL, NULL, NULL, 0, ctx)) < 0)
        goto fail;
    over->x = res;

    over->overlay_is_packed_rgb =
        ff_fill_rgba_map(over->overlay_rgba_map, inlink->format) >= 0;
    over->overlay_has_alpha = ff_fmt_is_in(inlink->format, alpha_pix_fmts);

    av_log(ctx, AV_LOG_INFO,
           "main w:%d h:%d fmt:%s overlay x:%d y:%d w:%d h:%d fmt:%s\n",
           ctx->inputs[MAIN]->w, ctx->inputs[MAIN]->h,
           av_pix_fmt_descriptors[ctx->inputs[MAIN]->format].name,
           over->x, over->y,
           ctx->inputs[OVERLAY]->w, ctx->inputs[OVERLAY]->h,
           av_pix_fmt_descriptors[ctx->inputs[OVERLAY]->format].name);

    if (over->x < 0 || over->y < 0 ||
        over->x + var_values[VAR_OVERLAY_W] > var_values[VAR_MAIN_W] ||
        over->y + var_values[VAR_OVERLAY_H] > var_values[VAR_MAIN_H]) {
        av_log(ctx, AV_LOG_ERROR,
               "Overlay area (%d,%d)<->(%d,%d) not within the main area (0,0)<->(%d,%d) or zero-sized\n",
               over->x, over->y,
               (int)(over->x + var_values[VAR_OVERLAY_W]),
               (int)(over->y + var_values[VAR_OVERLAY_H]),
               (int)var_values[VAR_MAIN_W], (int)var_values[VAR_MAIN_H]);
        return AVERROR(EINVAL);
    }
    return 0;

fail:
    av_log(NULL, AV_LOG_ERROR,
           "Error when evaluating the expression '%s'\n", expr);
    return ret;
}

static int config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;

    outlink->w = ctx->inputs[MAIN]->w;
    outlink->h = ctx->inputs[MAIN]->h;
    outlink->time_base = ctx->inputs[MAIN]->time_base;

    return 0;
}

static AVFilterBufferRef *get_video_buffer(AVFilterLink *link, int perms, int w, int h)
{
    return avfilter_get_video_buffer(link->dst->outputs[0], perms, w, h);
}

static void start_frame(AVFilterLink *inlink, AVFilterBufferRef *inpicref)
{
    AVFilterBufferRef *outpicref = avfilter_ref_buffer(inpicref, ~0);
    AVFilterContext *ctx = inlink->dst;
    OverlayContext *over = ctx->priv;

    inlink->dst->outputs[0]->out_buf = outpicref;
    if (!over->overpicref ||
        av_compare_ts(over->overpicref->pts, inlink->time_base,
                      outpicref->pts, ctx->inputs[OVERLAY]->time_base) < 0) {
        AVFilterBufferRef *old = over->overpicref;
        over->overpicref = NULL;
        avfilter_request_frame(ctx->inputs[OVERLAY]);
        if (over->overpicref) {
            if (old)
                avfilter_unref_buffer(old);
        } else
            over->overpicref = old;
    }

    avfilter_start_frame(inlink->dst->outputs[0], outpicref);
}

static void start_frame_overlay(AVFilterLink *inlink, AVFilterBufferRef *inpicref)
{
    AVFilterContext *ctx = inlink->dst;
    OverlayContext *over = ctx->priv;

    over->overpicref = inpicref;
}

// divide by 255 and round to nearest
// apply a fast variant: (X+127)/255 = ((X+127)*257+257)>>16 = ((X+128)*257)>>16
#define FAST_DIV255(x) ((((x) + 128) * 257) >> 16)

// calculate the unpremultiplied alpha, applying the general equation:
// alpha = alpha_overlay / ( (alpha_main + alpha_overlay) - (alpha_main * alpha_overlay) )
// (((x) << 16) - ((x) << 9) + (x)) is a faster version of: 255 * 255 * x
// ((((x) + (y)) << 8) - ((x) + (y)) - (y) * (x)) is a faster version of: 255 * (x + y)
#define UNPREMULTIPLY_ALPHA(x, y) ((((x) << 16) - ((x) << 9) + (x)) / ((((x) + (y)) << 8) - ((x) + (y)) - (y) * (x)))

static void blend_slice(AVFilterContext *ctx,
                        AVFilterBufferRef *dst, AVFilterBufferRef *src,
                        int x, int y, int w, int h,
                        int slice_y, int slice_w, int slice_h)
{
    OverlayContext *over = ctx->priv;
    int i, j, k;
    int width, height;
    int overlay_end_y = y+h;
    int slice_end_y = slice_y+slice_h;
    int end_y, start_y;

    width = FFMIN(slice_w - x, w);
    end_y = FFMIN(slice_end_y, overlay_end_y);
    start_y = FFMAX(y, slice_y);
    height = end_y - start_y;

    if (over->main_is_packed_rgb) {
        uint8_t *dp = dst->data[0] + x * over->main_pix_step[0] +
                      start_y * dst->linesize[0];
        uint8_t *sp = src->data[0];
        uint8_t alpha;          ///< the amount of overlay to blend on to main
        const int dr = over->main_rgba_map[R];
        const int dg = over->main_rgba_map[G];
        const int db = over->main_rgba_map[B];
        const int da = over->main_rgba_map[A];
        const int dstep = over->main_pix_step[0];
        const int sr = over->overlay_rgba_map[R];
        const int sg = over->overlay_rgba_map[G];
        const int sb = over->overlay_rgba_map[B];
        const int sa = over->overlay_rgba_map[A];
        const int sstep = over->overlay_pix_step[0];
        const int main_has_alpha = over->main_has_alpha;
        if (slice_y > y)
            sp += (slice_y - y) * src->linesize[0];
        for (i = 0; i < height; i++) {
            uint8_t *d = dp, *s = sp;
            for (j = 0; j < width; j++) {
                alpha = s[sa];

                // if the main channel has an alpha channel, alpha has to be calculated
                // to create an un-premultiplied (straight) alpha value
                if (main_has_alpha && alpha != 0 && alpha != 255) {
                    uint8_t alpha_d = d[da];
                    alpha = UNPREMULTIPLY_ALPHA(alpha, alpha_d);
                }

                switch (alpha) {
                case 0:
                    break;
                case 255:
                    d[dr] = s[sr];
                    d[dg] = s[sg];
                    d[db] = s[sb];
                    break;
                default:
                    // main_value = main_value * (1 - alpha) + overlay_value * alpha
                    // since alpha is in the range 0-255, the result must divided by 255
                    d[dr] = FAST_DIV255(d[dr] * (255 - alpha) + s[sr] * alpha);
                    d[dg] = FAST_DIV255(d[dg] * (255 - alpha) + s[sg] * alpha);
                    d[db] = FAST_DIV255(d[db] * (255 - alpha) + s[sb] * alpha);
                }
                if (main_has_alpha) {
                    switch (alpha) {
                    case 0:
                        break;
                    case 255:
                        d[da] = s[sa];
                        break;
                    default:
                        // apply alpha compositing: main_alpha += (1-main_alpha) * overlay_alpha
                        d[da] += FAST_DIV255((255 - d[da]) * s[sa]);
                    }
                }
                d += dstep;
                s += sstep;
            }
            dp += dst->linesize[0];
            sp += src->linesize[0];
        }
    } else {
        const int main_has_alpha = over->main_has_alpha;
        if (main_has_alpha) {
            uint8_t *da = dst->data[3] + x * over->main_pix_step[3] +
                          start_y * dst->linesize[3];
            uint8_t *sa = src->data[3];
            uint8_t alpha;          ///< the amount of overlay to blend on to main
            if (slice_y > y)
                sa += (slice_y - y) * src->linesize[3];
            for (i = 0; i < height; i++) {
                uint8_t *d = da, *s = sa;
                for (j = 0; j < width; j++) {
                    alpha = *s;
                    if (alpha != 0 && alpha != 255) {
                        uint8_t alpha_d = *d;
                        alpha = UNPREMULTIPLY_ALPHA(alpha, alpha_d);
                    }
                    switch (alpha) {
                    case 0:
                        break;
                    case 255:
                        *d = *s;
                        break;
                    default:
                        // apply alpha compositing: main_alpha += (1-main_alpha) * overlay_alpha
                        *d += FAST_DIV255((255 - *d) * *s);
                    }
                    d += 1;
                    s += 1;
                }
                da += dst->linesize[3];
                sa += src->linesize[3];
            }
        }
        for (i = 0; i < 3; i++) {
            int hsub = i ? over->hsub : 0;
            int vsub = i ? over->vsub : 0;
            uint8_t *dp = dst->data[i] + (x >> hsub) +
                (start_y >> vsub) * dst->linesize[i];
            uint8_t *sp = src->data[i];
            uint8_t *ap = src->data[3];
            int wp = FFALIGN(width, 1<<hsub) >> hsub;
            int hp = FFALIGN(height, 1<<vsub) >> vsub;
            if (slice_y > y) {
                sp += ((slice_y - y) >> vsub) * src->linesize[i];
                ap += (slice_y - y) * src->linesize[3];
            }
            for (j = 0; j < hp; j++) {
                uint8_t *d = dp, *s = sp, *a = ap;
                for (k = 0; k < wp; k++) {
                    // average alpha for color components, improve quality
                    uint8_t alpha_v, alpha_h, alpha;
                    if (hsub && vsub && j+1 < hp && k+1 < wp) {
                        alpha = (a[0] + a[src->linesize[3]] +
                                 a[1] + a[src->linesize[3]+1]) >> 2;
                    } else if (hsub || vsub) {
                        alpha_h = hsub && k+1 < wp ?
                            (a[0] + a[1]) >> 1 : a[0];
                        alpha_v = vsub && j+1 < hp ?
                            (a[0] + a[src->linesize[3]]) >> 1 : a[0];
                        alpha = (alpha_v + alpha_h) >> 1;
                    } else
                        alpha = a[0];
                    // if the main channel has an alpha channel, alpha has to be calculated
                    // to create an un-premultiplied (straight) alpha value
                    if (main_has_alpha && alpha != 0 && alpha != 255) {
                        // average alpha for color components, improve quality
                        uint8_t alpha_d;
                        if (hsub && vsub && j+1 < hp && k+1 < wp) {
                            alpha_d = (d[0] + d[src->linesize[3]] +
                                       d[1] + d[src->linesize[3]+1]) >> 2;
                        } else if (hsub || vsub) {
                            alpha_h = hsub && k+1 < wp ?
                                (d[0] + d[1]) >> 1 : d[0];
                            alpha_v = vsub && j+1 < hp ?
                                (d[0] + d[src->linesize[3]]) >> 1 : d[0];
                            alpha_d = (alpha_v + alpha_h) >> 1;
                        } else
                            alpha_d = d[0];
                        alpha = UNPREMULTIPLY_ALPHA(alpha, alpha_d);
                    }
                    *d = FAST_DIV255(*d * (255 - alpha) + *s * alpha);
                    s++;
                    d++;
                    a += 1 << hsub;
                }
                dp += dst->linesize[i];
                sp += src->linesize[i];
                ap += (1 << vsub) * src->linesize[3];
            }
        }
    }
}

static void draw_slice(AVFilterLink *inlink, int y, int h, int slice_dir)
{
    AVFilterContext *ctx = inlink->dst;
    AVFilterLink *outlink = ctx->outputs[0];
    AVFilterBufferRef *outpicref = outlink->out_buf;
    OverlayContext *over = ctx->priv;

    if (over->overpicref &&
        !(over->x >= outpicref->video->w || over->y >= outpicref->video->h ||
          y+h < over->y || y >= over->y + over->overpicref->video->h)) {
        blend_slice(ctx, outpicref, over->overpicref, over->x, over->y,
                    over->overpicref->video->w, over->overpicref->video->h,
                    y, outpicref->video->w, h);
    }
    avfilter_draw_slice(outlink, y, h, slice_dir);
}

static void end_frame(AVFilterLink *inlink)
{
    avfilter_end_frame(inlink->dst->outputs[0]);
    avfilter_unref_buffer(inlink->cur_buf);
}

static void null_draw_slice(AVFilterLink *inlink, int y, int h, int slice_dir) { }

static void null_end_frame(AVFilterLink *inlink) { }

AVFilter avfilter_vf_overlay = {
    .name      = "overlay",
    .description = NULL_IF_CONFIG_SMALL("Overlay a video source on top of the input."),

    .init      = init,
    .uninit    = uninit,

    .priv_size = sizeof(OverlayContext),

    .query_formats = query_formats,

    .inputs    = (AVFilterPad[]) {{ .name            = "main",
                                    .type            = AVMEDIA_TYPE_VIDEO,
                                    .start_frame     = start_frame,
                                    .get_video_buffer= get_video_buffer,
                                    .config_props    = config_input_main,
                                    .draw_slice      = draw_slice,
                                    .end_frame       = end_frame,
                                    .min_perms       = AV_PERM_READ,
                                    .rej_perms       = AV_PERM_REUSE2|AV_PERM_PRESERVE, },
                                  { .name            = "overlay",
                                    .type            = AVMEDIA_TYPE_VIDEO,
                                    .start_frame     = start_frame_overlay,
                                    .config_props    = config_input_overlay,
                                    .draw_slice      = null_draw_slice,
                                    .end_frame       = null_end_frame,
                                    .min_perms       = AV_PERM_READ,
                                    .rej_perms       = AV_PERM_REUSE2, },
                                  { .name = NULL}},
    .outputs   = (AVFilterPad[]) {{ .name            = "default",
                                    .type            = AVMEDIA_TYPE_VIDEO,
                                    .config_props    = config_output, },
                                  { .name = NULL}},
};
