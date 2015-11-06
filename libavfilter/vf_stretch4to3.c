/*
 * Copyright (c) 2012 Mark Himsley for BBC Broadcast Systems Development
 *
 * With thanks to Tim Borer from BBC R&D for the filter coefficients and
 * original implementation, which this implementation looks nothing like.
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
 * Stretch or squeeze video to four thirds the width
 */

/* #define DEBUG */

#include "avfilter.h"
#include "libavutil/imgutils.h"
#include "libavutil/pixdesc.h"

typedef struct {
    int w, h;                       ///< width and height of output image
    AVRational sar;                 ///< sample aspect ratio of the output image
    int chroma_w_sub, chroma_h_sub; ///< chroma sub-sampling in horizontal (width) and vertical (height) dimension
    int output_linesize[4];         ///< bytes of pixel data per line for each output plane
    int up, down;                   ///< image size change scale factor
} STRETCHContext;

// in stretch there are actually 4 phases, but one has been optimised out into a loop
#define STRETCH_N_PHASE 3
#define STRETCH_N_COEF 12
static const int stretch_up = 4;
static const int stretch_down = 3;
static const int stretch_n_phase = STRETCH_N_PHASE;
static const int stretch_n_coef = STRETCH_N_COEF;
static const int stretch_tap_offset = (STRETCH_N_COEF-2)/2;
static const int32_t stretch_coeff[STRETCH_N_PHASE][STRETCH_N_COEF] = {
        { -13,  31, -85, 197,-429,1187,3682,-666, 290,-132,  53,  -19},
        { -21,  58,-151, 338,-750,2574,2574,-750, 338,-151,  58,  -21},
        { -19,  53,-132, 290,-666,3682,1187,-429, 197, -85,  31,  -13}};

#define SQUEEZE_N_PHASE 3
#define SQUEEZE_N_COEF 12
static const int squeeze_up = 3;
static const int squeeze_down = 4;
static const int squeeze_n_phase = SQUEEZE_N_PHASE;
static const int squeeze_n_coef = SQUEEZE_N_COEF;
static const int squeeze_tap_offset = (SQUEEZE_N_COEF-2)/2;
static const int32_t squeeze_coeff[SQUEEZE_N_PHASE][SQUEEZE_N_COEF] = {
        { -23,   0, 160,-496, 865,3084, 865,-496, 160,   0, -23,   0},
        {   0, -47, 171,-269,   0,2744,1898,-457,   0,  90, -47,  13},
        {  13, -47,  90,   0,-457,1898,2744,   0,-269, 171, -47,   0}};

#define NORMALISATION 12
static const int normalisation = NORMALISATION;
static const int rounding = (1<<(NORMALISATION-1));
static const int32_t maximum = 255*4096;

static int query_formats(AVFilterContext *ctx)
{
    const enum PixelFormat pix_fmts[] = {
            PIX_FMT_YUV422P,
            PIX_FMT_YUV444P,
            PIX_FMT_YUV420P,
            PIX_FMT_YUV411P,
            PIX_FMT_YUV410P,
            PIX_FMT_YUVJ422P,
            PIX_FMT_YUVJ444P,
            PIX_FMT_NONE };

#ifdef DEBUG
    av_dlog(ctx, "query_formats()\n");
#endif

    avfilter_set_common_pixel_formats(ctx, avfilter_make_format_list(pix_fmts));

    return 0;
}

static int config_inputs(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    STRETCHContext *priv_ctx = ctx->priv;

    const AVPixFmtDescriptor *pix_desc = &av_pix_fmt_descriptors[inlink->format];
    int plane;
    AVRational dar;

    priv_ctx->chroma_h_sub = pix_desc->log2_chroma_h;
    priv_ctx->chroma_w_sub = pix_desc->log2_chroma_w;

    priv_ctx->w = FFALIGN(inlink->w*priv_ctx->up/priv_ctx->down, (int)pow(2,priv_ctx->chroma_w_sub));
    priv_ctx->h = inlink->h;

    av_log(ctx, AV_LOG_DEBUG, "fmt:%s chroma_w_sub:%d chroma_h_sub:%d w:%d -> w:%d\n",
            av_pix_fmt_descriptors[inlink->format].name,priv_ctx->chroma_w_sub,priv_ctx->chroma_h_sub,inlink->w,priv_ctx->w);

    dar = av_mul_q(inlink->sample_aspect_ratio,
                   (AVRational){inlink->w, inlink->h});
    av_reduce(&priv_ctx->sar.num,
              &priv_ctx->sar.den,
              dar.num * priv_ctx->h,
              dar.den * priv_ctx->w,
              INT_MAX);

    // full an array with the number of bytes that the video
    // data occupies per line for each plane of the input video
    for (plane = 0; plane < 4; plane++) {
        priv_ctx->output_linesize[plane] = av_image_get_linesize(
                inlink->format,
                priv_ctx->w,
                plane);
    }

    return 0;
}

static int config_input_stretch(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    STRETCHContext *priv_ctx = ctx->priv;

    priv_ctx->up = stretch_up;
    priv_ctx->down = stretch_down;

    //   av_log(ctx, AV_LOG_INFO, "config_input_stretch\n");

    return config_inputs(inlink);
}

static int config_input_squeeze(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    STRETCHContext *priv_ctx = ctx->priv;

    priv_ctx->up = squeeze_up;
    priv_ctx->down = squeeze_down;

 //   av_log(ctx, AV_LOG_INFO, "config_input_squeeze\n");

    return config_inputs(inlink);
}

static int config_output(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    STRETCHContext *priv_ctx = ctx->priv;

//    av_log(ctx, AV_LOG_INFO, "config_output\n");

    outlink->w = priv_ctx->w;
    outlink->h = priv_ctx->h;
    outlink->sample_aspect_ratio = priv_ctx->sar;

    return 0;
}

static void start_frame(AVFilterLink *inlink, AVFilterBufferRef *inpicref)
{
    AVFilterContext *ctx = inlink->dst;
    STRETCHContext *priv_ctx = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];

#ifdef DEBUG
    av_dlog(ctx, "start_frame()\n");
#endif
    outlink->out_buf = avfilter_get_video_buffer(
            outlink,
            AV_PERM_WRITE,
            FFALIGN(priv_ctx->w,32),
            FFALIGN(priv_ctx->h,32));
    avfilter_copy_buffer_ref_props(outlink->out_buf, inpicref);
    outlink->out_buf->video->w = priv_ctx->w;
    outlink->out_buf->video->h = priv_ctx->h;
    outlink->out_buf->video->sample_aspect_ratio = priv_ctx->sar;

    avfilter_start_frame(outlink, avfilter_ref_buffer(outlink->out_buf, ~0));
}

static void draw_slice_stretch(AVFilterLink *inlink, int slice_y, int slice_h, int slice_dir)
{
    AVFilterContext *ctx = inlink->dst;
    STRETCHContext *priv_ctx = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];
    AVFilterBufferRef *inpicref   = inlink->cur_buf;
    AVFilterBufferRef *outpicref  = outlink->cur_buf;

    uint8_t *input_line_start, *input_line_end; ///< pointers to the first and last pixels of picture data for the input line
    uint8_t *output_line_start;                 ///< pointer to the first pixel of picture data for the output line
    uint8_t *input_pixel_pointer;     ///< pointer to the current pixel of picture data for the input line
    uint8_t *output_pixel_pointer;    ///< pointer to the current pixel of picture data for the output line
    int slice_delta_y;        ///< delta y within the input slice
    int input_y;              ///< actual y within the input picture
    int output_x;             ///< actual x within the output picture
    int plane;                ///< plane of picture
    int plane_y_sub;
    int coef_tap, coef_phase; ///< coefficient column and row
    int32_t accumulator;      ///< accumulator for current pixel calculation

#ifdef DEBUG
    av_dlog(ctx, "draw_slice_stretch()\n");
#endif

    for (plane = 0; plane < 4 && inpicref->data[plane] && outpicref->data[plane]; plane++) {
        plane_y_sub = (plane == 1 || plane == 2) ? priv_ctx->chroma_h_sub : 0;
#ifdef DEBUG
        av_dlog(ctx, "plane: %d slice_h >> plane_y_sub:%d\n",plane, slice_h >> plane_y_sub);
#endif
        for (slice_delta_y = 0; slice_delta_y < (slice_h >> plane_y_sub); slice_delta_y++) {
            input_y = (slice_y >> plane_y_sub) + slice_delta_y;
            input_pixel_pointer = input_line_start = inpicref->data[plane] + input_y * inpicref->linesize[plane];
            input_line_end = input_line_start + inpicref->linesize[plane];
            output_pixel_pointer = output_line_start = outpicref->data[plane] + input_y * outpicref->linesize[plane];
            // walk along the input line in groups of 'stretch_down' pixels
            // walk along the output line in groups of 'stretch_up' pixels
            // one set of coefficients said 'just copy the original pixel'
            // so output pixel 1 from the group of 4 is a direct copy
            for (output_x = 0; output_x < priv_ctx->output_linesize[plane]; output_x += stretch_up) {
                // output pixel 1 from the group of 4 maps directly from input pixel 1
                *output_pixel_pointer = *input_pixel_pointer;
                output_pixel_pointer +=4; input_pixel_pointer += 3;
            }
            // output pixels 2, 3 and 4 from the group of 4 are calculated from many input pixels
            input_pixel_pointer = input_line_start - stretch_tap_offset;
            output_pixel_pointer = output_line_start + 1;
            // calculate the leading edge of the output image
            for (output_x = 1; output_x < 8; output_x += stretch_up) {
//                av_log(ctx, AV_LOG_INFO, "line:%d\n",output_x);
                for (coef_phase = 0; coef_phase < stretch_n_phase; coef_phase++) {
                    accumulator = rounding;
                    for (coef_tap = 0; coef_tap < stretch_n_coef; coef_tap++) {
                        if (input_pixel_pointer+coef_tap < input_line_start)
                            accumulator += *input_line_start * stretch_coeff[coef_phase][coef_tap];
                        else
                            accumulator += *(input_pixel_pointer+coef_tap) * stretch_coeff[coef_phase][coef_tap];
                    }
                    *output_pixel_pointer++ = (FFMAX(FFMIN(accumulator,maximum),0)) >> normalisation;
                    input_pixel_pointer++;
                }
                output_pixel_pointer++; // skip the output pixel 1
            }
            // calculate the centre of the output image
            for (output_x = 9; output_x < priv_ctx->output_linesize[plane] - 8; output_x += stretch_up) {
                for (coef_phase = 0; coef_phase < stretch_n_phase; coef_phase++) {
                    accumulator = rounding;
                    for (coef_tap = 0; coef_tap < stretch_n_coef; coef_tap++) {
                        accumulator += *(input_pixel_pointer+coef_tap) * stretch_coeff[coef_phase][coef_tap];
                    }
                    *output_pixel_pointer++ = (FFMAX(FFMIN(accumulator,maximum),0)) >> normalisation;
                    input_pixel_pointer++;
                }
                output_pixel_pointer++; // skip the output pixel 1
            }
            // calculate the lagging edge of the output image
            for (output_x = priv_ctx->output_linesize[plane] - 7; output_x < priv_ctx->output_linesize[plane]; output_x += stretch_up) {
                for (coef_phase = 0; coef_phase < stretch_n_phase; coef_phase++) {
                    accumulator = rounding;
                    for (coef_tap = 0; coef_tap < stretch_n_coef; coef_tap++) {
                        if (input_pixel_pointer+coef_tap > input_line_end)
                            accumulator += *input_line_end * stretch_coeff[coef_phase][coef_tap];
                        else
                            accumulator += *(input_pixel_pointer+coef_tap) * stretch_coeff[coef_phase][coef_tap];
                    }
                    *output_pixel_pointer++ = (FFMAX(FFMIN(accumulator,maximum),0)) >> normalisation;
                    input_pixel_pointer++;
                }
                output_pixel_pointer++; // skip the output pixel 1
            }
        }
    }

    avfilter_draw_slice(outlink, slice_y, slice_h, slice_dir);
}

static void draw_slice_squeeze(AVFilterLink *inlink, int slice_y, int slice_h, int slice_dir)
{
    AVFilterContext *ctx = inlink->dst;
    STRETCHContext *priv_ctx = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];
    AVFilterBufferRef *inpicref   = inlink->cur_buf;
    AVFilterBufferRef *outpicref  = outlink->cur_buf;

    uint8_t *input_line_start, *input_line_end; ///< pointers to the first and last pixels of picture data for the input line
    uint8_t *input_pixel_pointer;     ///< pointer to the current pixel of picture data for the input line
    uint8_t *output_pixel_pointer;    ///< pointer to the current pixel of picture data for the output line
    int slice_delta_y;        ///< delta y within the input slice
    int input_y;              ///< actual y within the input picture
    int output_x;             ///< actual x within the output picture
    int plane;                ///< plane of picture
    int plane_y_sub;
    int coef_tap, coef_phase; ///< coefficient column and row
    int32_t accumulator;      ///< accumulator for current pixel calculation

#ifdef DEBUG
    av_dlog(ctx, "draw_slice_squeeze()\n");
#endif

    for (plane = 0; plane < 4 && inpicref->data[plane] && outpicref->data[plane]; plane++) {
        plane_y_sub = (plane == 1 || plane == 2) ? priv_ctx->chroma_h_sub : 0;
#ifdef DEBUG
        av_dlog(ctx, "plane: %d slice_h >> plane_y_sub:%d\n",plane, slice_h >> plane_y_sub);
#endif
        for (slice_delta_y = 0; slice_delta_y < (slice_h >> plane_y_sub); slice_delta_y++) {
            input_y = (slice_y >> plane_y_sub) + slice_delta_y;
            input_line_start = inpicref->data[plane] + input_y * inpicref->linesize[plane];
            input_line_end = input_line_start + inpicref->linesize[plane];
            input_pixel_pointer = input_line_start - squeeze_tap_offset;
            output_pixel_pointer = outpicref->data[plane] + input_y * outpicref->linesize[plane];
            // walk along the input line in groups of 4 pixels
            // walk along the output line in groups of 3 pixels
            // calculate the leading edge of the output image
            for (output_x = 0; output_x < 6; output_x += squeeze_up) {
                for (coef_phase = 0; coef_phase < squeeze_n_phase; coef_phase++) {
                    accumulator = rounding;
                    for (coef_tap = 0; coef_tap < squeeze_n_coef; coef_tap++) {
                        if (squeeze_coeff[coef_phase][coef_tap] == 0) continue;
                        if (input_pixel_pointer+coef_tap < input_line_start)
                            accumulator += *input_line_start * squeeze_coeff[coef_phase][coef_tap];
                        else
                            accumulator += *(input_pixel_pointer+coef_tap) * squeeze_coeff[coef_phase][coef_tap];
                    }
                    *output_pixel_pointer++ = (FFMAX(FFMIN(accumulator,maximum),0)) >> normalisation;
                    input_pixel_pointer++;
                }
                input_pixel_pointer++;
            }
            // calculate the centre of the output image
            for (output_x = 6; output_x < priv_ctx->output_linesize[plane] - 6; output_x += squeeze_up) {
                for (coef_phase = 0; coef_phase < squeeze_n_phase; coef_phase++) {
                    accumulator = rounding;
                    for (coef_tap = 0; coef_tap < squeeze_n_coef; coef_tap++) {
                        if (squeeze_coeff[coef_phase][coef_tap] == 0) continue;
                        accumulator += *(input_pixel_pointer+coef_tap) * squeeze_coeff[coef_phase][coef_tap];
                    }
                    *output_pixel_pointer++ = (FFMAX(FFMIN(accumulator,maximum),0)) >> normalisation;
                    input_pixel_pointer++;
                }
                input_pixel_pointer++;
            }
            // calculate the lagging edge of the output image
            for (output_x = priv_ctx->output_linesize[plane] - 6; output_x < priv_ctx->output_linesize[plane]; output_x += squeeze_up) {
                for (coef_phase = 0; coef_phase < squeeze_n_phase; coef_phase++) {
                    accumulator = rounding;
                    for (coef_tap = 0; coef_tap < squeeze_n_coef; coef_tap++) {
                        if (squeeze_coeff[coef_phase][coef_tap] == 0) continue;
                        if (input_pixel_pointer+coef_tap > input_line_end) accumulator += *input_line_end * squeeze_coeff[coef_phase][coef_tap];
                        else accumulator += *(input_pixel_pointer+coef_tap) * squeeze_coeff[coef_phase][coef_tap];
                    }
                    *output_pixel_pointer++ = (FFMAX(FFMIN(accumulator,maximum),0)) >> normalisation;
                    input_pixel_pointer++;
                }
                input_pixel_pointer++;
            }
        }
    }

    avfilter_draw_slice(outlink, slice_y, slice_h, slice_dir);
}

static void end_frame(AVFilterLink *inlink)
{
#ifdef DEBUG
    AVFilterContext *ctx = inlink->dst;
    av_dlog(ctx, "end_frame()\n");
#endif

    avfilter_unref_buffer(inlink->cur_buf);
    avfilter_end_frame(inlink->dst->outputs[0]);
    avfilter_unref_buffer(inlink->dst->outputs[0]->out_buf);
}

AVFilter avfilter_vf_stretch4to3 = {
    .name          = "stretch4to3",
    .description   = NULL_IF_CONFIG_SMALL("Stretch video to four thirds the width"),
    .priv_size     = sizeof(STRETCHContext),
    .query_formats = query_formats,
    .inputs        = (const AVFilterPad[]) {{
                                            .name             = "default",
                                            .type             = AVMEDIA_TYPE_VIDEO,
                                            .config_props     = config_input_stretch,
                                            .start_frame      = start_frame,
                                            .draw_slice       = draw_slice_stretch,
                                            .end_frame        = end_frame,
                                            .min_perms        = AV_PERM_READ,
                                            .rej_perms        = AV_PERM_REUSE2|AV_PERM_PRESERVE,},
                                            { .name = NULL}},
    .outputs       = (const AVFilterPad[]) {{
                                            .name             = "default",
                                            .type             = AVMEDIA_TYPE_VIDEO,
                                            .config_props     = config_output,},
                                            { .name = NULL}},
};

AVFilter avfilter_vf_squeeze4to3 = {
    .name          = "squeeze4to3",
    .description   = NULL_IF_CONFIG_SMALL("Squeeze video to three quarters the width"),
    .priv_size     = sizeof(STRETCHContext),
    .query_formats = query_formats,
    .inputs        = (const AVFilterPad[]) {{
                                            .name             = "default",
                                            .type             = AVMEDIA_TYPE_VIDEO,
                                            .config_props     = config_input_squeeze,
                                            .start_frame      = start_frame,
                                            .draw_slice       = draw_slice_squeeze,
                                            .end_frame        = end_frame,
                                            .min_perms        = AV_PERM_READ,
                                            .rej_perms        = AV_PERM_REUSE2|AV_PERM_PRESERVE,},
                                            { .name = NULL}},
    .outputs       = (const AVFilterPad[]) {{
                                            .name             = "default",
                                            .type             = AVMEDIA_TYPE_VIDEO,
                                            .config_props     = config_output,},
                                            { .name = NULL}},
};
