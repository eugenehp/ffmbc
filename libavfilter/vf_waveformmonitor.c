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
 * video waveform monitor filter
 */

/* #define DEBUG */

#include "libavutil/colorspace.h"
#include "libavutil/pixdesc.h"
#include "libavutil/parseutils.h"
#include "avfilter.h"

typedef struct {
    int w, h;
    int hsub, vsub;         ///< chroma subsampling values
    uint8_t rgba_color[4];
    uint8_t yuva_color[4];
    uint16_t *luma_data;    ///< pointer to array of pic-width * 256 to store incoming luma value buckets
    int brightness;         ///< brightness, contrast and gama for waveform display
    float gain, gamma;
    uint8_t *y_lut;         ///< transform to make waveform monitor brightness look right
    uint8_t *a_lut;         ///< transform for alpha
} WFMContext;

static av_cold int init(AVFilterContext *ctx, const char *args, void *opaque)
{
    WFMContext *wfm = ctx->priv;

    int ret;

    //TODO: make brightness, gain and gamma parameters
    wfm->brightness = 40;
    wfm->gain       = 4.0;
    wfm->gamma      = 3.0;

    //TODO: make colour a parameter
    if ((ret = av_parse_color(wfm->rgba_color, "0x00000080", -1, ctx)) < 0)
        return ret;

    wfm->yuva_color[0] = RGB_TO_Y_CCIR(wfm->rgba_color[0], wfm->rgba_color[1], wfm->rgba_color[2]);
    wfm->yuva_color[1] = RGB_TO_U_CCIR(wfm->rgba_color[0], wfm->rgba_color[1], wfm->rgba_color[2], 0);
    wfm->yuva_color[2] = RGB_TO_V_CCIR(wfm->rgba_color[0], wfm->rgba_color[1], wfm->rgba_color[2], 0);
    wfm->yuva_color[3] = wfm->rgba_color[3];

    return 0;
}

static av_cold void uninit(AVFilterContext *ctx)
{
    WFMContext *wfm = ctx->priv;

    av_freep(&wfm->luma_data);
    av_freep(&wfm->y_lut);
    av_freep(&wfm->a_lut);
}

static int query_formats(AVFilterContext *ctx)
{
    const enum PixelFormat in_pix_fmts[] = {
            PIX_FMT_YUV422P,
            PIX_FMT_YUV444P,
            PIX_FMT_YUV420P,
            PIX_FMT_NONE };
    const enum PixelFormat out_pix_fmts[] = {
            PIX_FMT_YUVA420P,
            PIX_FMT_NONE };

    if (ctx->inputs[0]) {
        AVFilterFormats *in_formats = avfilter_make_format_list(in_pix_fmts);
        AVFilterFormats *out_formats = avfilter_make_format_list(out_pix_fmts);

        avfilter_formats_ref(in_formats, &ctx->inputs [0]->out_formats);
        avfilter_formats_ref(out_formats, &ctx->outputs[0]->in_formats );
    }

    return 0;
}

static int config_input_luma(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    WFMContext *wfm = ctx->priv;

    int h;

    av_log(ctx, AV_LOG_INFO, "config_input_luma\n");

    wfm->w = inlink->w;
    wfm->h = 256;

    if (!(wfm->luma_data = av_malloc(wfm->w * wfm->h * sizeof(wfm->luma_data))) ||
        !(wfm->y_lut = av_malloc((1 + inlink->h) * sizeof(wfm->y_lut))) ||
        !(wfm->a_lut = av_malloc((1 + inlink->h) * sizeof(wfm->a_lut)))) {
        uninit(ctx);
        return AVERROR(ENOMEM);
    }

    /* calculate 8 bit luminance LUT emulating a waveform monitor display
     * within CCIR-601 space
     * the calculation gives a fast rising start to a knee (dependent on gamma)
     * then flattens off rising (dependent on gain) to a clip point */
    wfm->y_lut[0] = wfm->yuva_color[0];
    for (h = 0; h < inlink->h; h++) {
        wfm->y_lut[h+1] = FFMIN(235,
                wfm->yuva_color[0] + wfm->brightness +
                pow(wfm->gain*h, 1/wfm->gamma) / pow(inlink->h, 1/wfm->gamma) *
                (219 - wfm->yuva_color[0] - wfm->brightness));
    }
    /* calculate an 8 bit alpha LUT for keying the above luminance */
    wfm->a_lut[0] = wfm->yuva_color[3];
    for (h = 0; h <= inlink->h; h++) {
        wfm->a_lut[h+1] = FFMIN(255,
                wfm->yuva_color[3] + wfm->brightness +
                pow(wfm->gain*h,1/wfm->gamma) / pow(inlink->h, 1/wfm->gamma) *
                (255 - wfm->yuva_color[3] - wfm->brightness));
//        av_log(ctx, AV_LOG_DEBUG, "%d\tA = %d\tY = %d\n",h,wfm->a_lut[h],wfm->y_lut[h]);
    }

    return 0;
}

static int config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    WFMContext *wfm = ctx->priv;

    const AVPixFmtDescriptor *pix_desc = &av_pix_fmt_descriptors[outlink->format];

    av_log(ctx, AV_LOG_INFO, "config_output\n");

    outlink->w = wfm->w;
    outlink->h = wfm->h;

    wfm->hsub = pix_desc->log2_chroma_w;
    wfm->vsub = pix_desc->log2_chroma_h;

    return 0;
}

static void start_frame_luma(AVFilterLink *inlink, AVFilterBufferRef *inpicref)
{
    AVFilterContext *ctx = inlink->dst;
    WFMContext *wfm = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];

    outlink->out_buf = avfilter_get_video_buffer(
            outlink,
            AV_PERM_WRITE,
            FFALIGN(wfm->w,32),
            FFALIGN(wfm->h,32));
    avfilter_copy_buffer_ref_props(outlink->out_buf, inpicref);
    outlink->out_buf->video->sample_aspect_ratio = (AVRational){1,1};

    /** clear waveform monitor cache data */
    memset(wfm->luma_data, 0, wfm->w * wfm->h * sizeof(wfm->luma_data));

    avfilter_start_frame(outlink, outlink->out_buf);
}

static void draw_slice_luma(AVFilterLink *inlink, int y, int h, int slice_dir)
{
    AVFilterContext *ctx = inlink->dst;
    WFMContext *wfm = ctx->priv;

    AVFilterBufferRef *inpicref = inlink->cur_buf;

    int dy, dx;     ///< delta x and y within the input slice
    uint8_t *p;     ///< pointer to picture data

    for (dy = 0; dy < h; dy++) {
        p = inpicref->data[0] + (y+dy) * inpicref->linesize[0];
        for (dx = 0; dx < inpicref->video->w && dx < wfm->w; dx++) {
            wfm->luma_data[dx + *p * wfm->w]++;
            p++;
        }
    }
    // do not call next avfilter_draw_slice() because the outpic
    //  is not created until end_frame()
}

static void end_frame_luma(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    WFMContext *wfm = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];

    AVFilterBufferRef *outpicref = outlink->out_buf;

    int dy, dx;     ///< delta x and y within the input slice
    uint8_t *y, *a; ///< pointer to picture data and alpha data

    for (dy = 0; dy < wfm->h; dy++) {
        y = outpicref->data[0] + (wfm->h - dy - 1) * outpicref->linesize[0];
        a = outpicref->data[3] + (wfm->h - dy - 1) * outpicref->linesize[0];
        for (dx = 0; dx < wfm->w; dx++) {
            *y++ = wfm->y_lut[wfm->luma_data[dx + dy * wfm->w]];
            *a++ = wfm->a_lut[wfm->luma_data[dx + dy * wfm->w]];
        }
    }

    avfilter_draw_slice(outlink, 0, wfm->h, 1);
    avfilter_end_frame(outlink);
    avfilter_unref_buffer(inlink->cur_buf);
}

AVFilter avfilter_vf_wfm_luma = {
    .name          = "wfm_luma",
    .description   = NULL_IF_CONFIG_SMALL("Luma waveform monitor."),
    .priv_size     = sizeof(WFMContext),
    .init          = init,
    .uninit        = uninit,
    .query_formats = query_formats,
    .inputs        = (const AVFilterPad[]) {{
                                            .name             = "default",
                                            .type             = AVMEDIA_TYPE_VIDEO,
                                            .config_props     = config_input_luma,
                                            .get_video_buffer = avfilter_null_get_video_buffer,
                                            .start_frame      = start_frame_luma,
                                            .draw_slice       = draw_slice_luma,
                                            .end_frame        = end_frame_luma,
                                            .min_perms        = AV_PERM_READ,
                                            .rej_perms        = AV_PERM_REUSE2|AV_PERM_PRESERVE,},
                                            {.name = NULL}},
    .outputs       = (const AVFilterPad[]) {{
                                            .name             = "default",
                                            .type             = AVMEDIA_TYPE_VIDEO,
                                            .config_props     = config_output, },
                                            {.name = NULL}},
};
