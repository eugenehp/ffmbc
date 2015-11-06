/*
 * Copyright (c) 2013 Mark Himsley
 * Copyright (c) 2010 Brandon Mintern
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
 * video fade filter
 * based heavily on vf_negate.c by Bobby Bingham
 */

#include "libavutil/avstring.h"
#include "libavutil/eval.h"
#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"
#include "libavutil/parseutils.h"
#include "libavutil/colorspace.h"
#include "avfilter.h"
#include "drawutils.h"
#include "internal.h"

typedef struct {
    const AVClass *class;
    FFDrawContext dc;
    FFDrawColor color;
    double factor, fade_per_frame;
    unsigned int frame_index, start_frame, stop_frame, nb_frames;
    int alpha;
    char type[4];
    char *color_str;
    uint8_t is_packed_rgb;
    uint8_t rgba_map[4];
} FadeContext;

enum { RED = 0, GREEN, BLUE, ALPHA };

#define OFFSET(x) offsetof(FadeContext, x)

static const AVOption fade_options[] = {
    {"alpha", "fade alpha if present",        OFFSET(alpha),     FF_OPT_TYPE_INT,    {.dbl=0}, 0, 1 },
    {"color", "set color to fade to or from", OFFSET(color_str), FF_OPT_TYPE_STRING, {.str="black"}, CHAR_MIN, CHAR_MAX },
    {NULL},
};

static const char *fade_get_name(void *ctx)
{
    return "fade";
}

static const AVClass fade_class = {
    "FadeContext",
    fade_get_name,
    fade_options
};

static av_cold int init(AVFilterContext *ctx, const char *args, void *opaque)
{
    FadeContext *fade = ctx->priv;
    int len, ret = 0;

    fade->class = &fade_class;
    av_opt_set_defaults(fade);

    if (!args ||
        sscanf(args, " %3[^:]:%u:%u%n", fade->type, &fade->start_frame, &fade->nb_frames, &len) != 3) {
        av_log(ctx, AV_LOG_ERROR,
               "Expected 3 arguments '(in|out):#:#':'%s'\n", args);
        return AVERROR(EINVAL);
    }

    if (args[len] == ':') {
        if ((ret = av_set_options_string(fade, args+len+1, "=", ":")) < 0)
            return ret;
    }

    if ((ret = av_parse_color(fade->color.rgba, fade->color_str, -1, ctx))) {
        av_log(ctx, AV_LOG_ERROR, "Invalid text color '%s'\n", fade->color_str);
        return ret;
    }

    fade->nb_frames = FFMAX(1, fade->nb_frames);
    fade->fade_per_frame = 255.0 / fade->nb_frames;
    if (!strcmp(fade->type, "in")) {
        fade->fade_per_frame = -fade->fade_per_frame;
        fade->factor = 255;
    } else if (!strcmp(fade->type, "out")) {
        fade->factor = 0;
    } else {
        av_log(ctx, AV_LOG_ERROR,
               "Type argument must be 'in' or 'out' but '%s' was specified\n", fade->type);
        return AVERROR(EINVAL);
    }
    fade->stop_frame = fade->start_frame + fade->nb_frames;

    av_log(ctx, AV_LOG_INFO,
           "type:%s start_frame:%d nb_frames:%d alpha:%d color:%s\n",
           fade->type, fade->start_frame, fade->nb_frames, fade->alpha, fade->color_str);

    return 0;
}

static int query_formats(AVFilterContext *ctx)
{
    avfilter_set_common_pixel_formats(ctx, ff_draw_supported_pixel_formats(0));
    return 0;
}

static enum PixelFormat alpha_pix_fmts[] = {
    PIX_FMT_YUVA420P,
    PIX_FMT_ARGB, PIX_FMT_ABGR,
    PIX_FMT_RGBA, PIX_FMT_BGRA,
    PIX_FMT_NONE
};

static int config_props(AVFilterLink *inlink)
{
    FadeContext *fade = inlink->dst->priv;

    ff_draw_init(&fade->dc, inlink->format, 0);
    ff_draw_color(&fade->dc, &fade->color, fade->color.rgba);

    fade->alpha = fade->alpha ? ff_fmt_is_in(inlink->format, alpha_pix_fmts) : 0;
    fade->is_packed_rgb = ff_fill_rgba_map(fade->rgba_map, inlink->format) >= 0;

    return 0;
}

static void fade_alpha(int y, int h, int w,
                       double fade_factor,
                       uint8_t offset, uint8_t step, int bytes_per_plane,
                       uint8_t *data, int line_size)
{
    uint8_t *p;
    int i, j, m;

    m = 255 - lrint(fade_factor);

    for (i = 0; i < h; i++) {
        p = data + offset + (y+i) * line_size;
        for (j = 0; j < w * bytes_per_plane; j++) {
            *p = (*p * m) >> 8;
            p += step;
        }
    }
}
static void draw_slice(AVFilterLink *inlink, int y, int h, int slice_dir)
{
    FadeContext *fade = inlink->dst->priv;
    AVFilterBufferRef *picref = inlink->cur_buf;

    if (fade->factor > 0) {
        if (fade->alpha) {
            int plane;
            // alpha only
            plane = fade->is_packed_rgb ? 0 : ALPHA; // alpha is on plane 0 for packed formats
                                                     // or plane 3 for planar formats
            fade_alpha(y, h, inlink->w,
                       fade->factor,
                       fade->is_packed_rgb ? fade->rgba_map[ALPHA] : 0, // alpha offset
                       fade->dc.pixelstep[plane],
                       1, picref->data[plane], picref->linesize[plane]);
        } else {
            fade->color.rgba[3] = lrint(fade->factor);
            ff_draw_color(&fade->dc, &fade->color, fade->color.rgba);
            ff_blend_rectangle(&fade->dc, &fade->color,
                               picref->data, picref->linesize,
                               picref->video->w, picref->video->h,
                               0, y, picref->video->w, h);
        }
    }

    avfilter_draw_slice(inlink->dst->outputs[0], y, h, slice_dir);
}

static void end_frame(AVFilterLink *inlink)
{
    FadeContext *fade = inlink->dst->priv;

    avfilter_end_frame(inlink->dst->outputs[0]);

    if (fade->frame_index >= fade->start_frame &&
        fade->frame_index <= fade->stop_frame)
        fade->factor += fade->fade_per_frame;
    fade->factor = av_clipf(fade->factor, 0, 255);
    fade->frame_index++;
}

AVFilter avfilter_vf_fade = {
    .name          = "fade",
    .description   = NULL_IF_CONFIG_SMALL("Fade in/out input video"),
    .init          = init,
    .priv_size     = sizeof(FadeContext),
    .query_formats = query_formats,

    .inputs    = (AVFilterPad[]) {{ .name            = "default",
                                    .type            = AVMEDIA_TYPE_VIDEO,
                                    .config_props    = config_props,
                                    .get_video_buffer = avfilter_null_get_video_buffer,
                                    .start_frame      = avfilter_null_start_frame,
                                    .draw_slice      = draw_slice,
                                    .end_frame       = end_frame,
                                    .min_perms       = AV_PERM_READ | AV_PERM_WRITE,
                                    .rej_perms       = AV_PERM_PRESERVE, },
                                  { .name = NULL}},
    .outputs   = (AVFilterPad[]) {{ .name            = "default",
                                    .type            = AVMEDIA_TYPE_VIDEO, },
                                  { .name = NULL}},
};
