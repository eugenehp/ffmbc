/*
 * Copyright (C) 2012 British Broadcasting Corporation, All Rights Reserved
 * Author of de-interlace algorithm: Jim Easterbrook for BBC R&D
 * Based on the process described by Martin Weston for BBC R&D
 * Author of FFmpeg filter: Mark Himsley for BBC Broadcast Systems Development
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include "libavutil/cpu.h"
#include "libavutil/common.h"
#include "libavutil/imgutils.h"
#include "libavutil/pixdesc.h"
#include "avfilter.h"

/* #define DEBUG */

#undef NDEBUG
#include <assert.h>

typedef struct {
    int line_size[4];     ///< bytes of pixel data per line for each plane
    int pending;          ///< how many fields are still waiting to be sent to next filter
    int field;            ///< which field are we on, 0 or 1
    int flush;            ///< are we flushing final frames
    int filter;           ///< 0 is simple, 1 is more complex
    int pts_delta;        ///< accumulator of pts deltas, add this when we can't calculate one

    AVFilterBufferRef *prev, *crnt, *next;  ///< previous, current, next frames
    AVFilterBufferRef *work;                ///< frame we are working on
    int32_t* work_line;   ///< line we are calculating

} W3FDIFContext;

/** Martin Weston deinterlace filter */

/** filter coefficients from PH-2071, scaled by 256*256
 *
 *  each set of coefficients have a sets for low-frequencies and high-frequencies
 *  n_coef_lf[] and n_coef_hf[] are the number of coefs for simple and more-complex
 *  it is important for later that n_coef_lf[] is even and n_coef_hf[] is odd
 *  coef_lf[][] and coef_hf[][] are the coefficients for low-frequencies and high-
 *                              frequencies for simple and more-complex mode */
static const int     n_coef_lf[2]    = {2, 4};
static const int32_t   coef_lf[2][4] = {{ 32768, 32768,     0,      0},
                                        { -1704, 34472, 34472,  -1704}};
static const int     n_coef_hf[2]    = {3, 5};
static const int32_t   coef_hf[2][5] = {{ -4096,  8192, -4096,     0,     0},
                                        {  2032, -7602, 11140, -7602,  2032}};

static int deinterlace_component(AVFilterContext *ctx,
        const AVFilterBufferRef *cur, const AVFilterBufferRef *adj,
        const int filter, const int plane)
{
    W3FDIFContext *w3fdif = ctx->priv;

    uint8_t *in_line, *out_line;
    uint8_t *in_lines_cur[5];
    uint8_t *in_lines_adj[5];
    uint8_t *out_pixel;
    int32_t* work_line;
    int32_t *work_pixel;
    int j, y_in, y_out;
    int cur_line_stride, adj_line_stride, dst_line_stride, line_size;
    uint8_t *cur_data, *adj_data, *dst_data;

    cur_line_stride = cur->linesize[plane];
    adj_line_stride = adj->linesize[plane];
    dst_line_stride = w3fdif->work->linesize[plane];

    cur_data = cur->data[plane];
    adj_data = adj->data[plane];
    dst_data = w3fdif->work->data[plane];

    line_size = w3fdif->line_size[plane];

    /** copy unchanged the lines of the field */
    if (w3fdif->field != cur->video->top_field_first) {
        y_out = 0;
    } else {
        y_out = 1;
    }

    in_line  = cur_data + (y_out * cur_line_stride);
    out_line = dst_data + (y_out * dst_line_stride);

    while (y_out < cur->video->h) {
        memcpy(out_line, in_line, line_size);
        y_out += 2;
        in_line  += cur_line_stride * 2;
        out_line += dst_line_stride * 2;
    }

    /** interpolate other other lines of the field */
    if (w3fdif->field==cur->video->top_field_first) {
        y_out = 0;
    } else {
        y_out = 1;
    }

    out_line = dst_data + (y_out * dst_line_stride);

    while (y_out < w3fdif->crnt->video->h) {
        /** clear workspace */
        memset(w3fdif->work_line, 0, sizeof(uint32_t) * line_size);
        /** get low vertical frequencies from current field */
        for (j = 0; j < n_coef_lf[filter]; j++) {
            y_in = (y_out + 1) + (j * 2) - n_coef_lf[filter];
            while (y_in < 0) y_in += 2;
            while (y_in >= cur->video->h) y_in -= 2;
            in_lines_cur[j] = cur_data + (y_in * cur_line_stride);
        }
        work_line = w3fdif->work_line;
        // TODO: set pixel stride for in
        // these have been unrolled from an function with loops for speed
        switch (n_coef_lf[filter]) {
        case 4:
            for (int i = 0; i < line_size; i++) {
                *work_line   += *in_lines_cur[0]++ * coef_lf[filter][0];
                *work_line   += *in_lines_cur[1]++ * coef_lf[filter][1];
                *work_line   += *in_lines_cur[2]++ * coef_lf[filter][2];
                *work_line++ += *in_lines_cur[3]++ * coef_lf[filter][3];
            }
            break;
        case 2:
            for (int i = 0; i < line_size; i++) {
                *work_line   += *in_lines_cur[0]++ * coef_lf[filter][0];
                *work_line++ += *in_lines_cur[1]++ * coef_lf[filter][1];
            }
            break;
        default:
            assert(0);
        }
        /** get high vertical frequencies from adjacent fields */
        for (j = 0; j < n_coef_hf[filter]; j++) {
            y_in = (y_out + 1) + (j * 2) - n_coef_hf[filter];
            while (y_in < 0) y_in += 2;
            while (y_in >= cur->video->h) y_in -= 2;
            in_lines_cur[j] = cur_data + (y_in * cur_line_stride);
            in_lines_adj[j] = adj_data + (y_in * adj_line_stride);
        }
        work_line = w3fdif->work_line;
        // TODO: set pixel stride for in
        // these have been unrolled from an function with loops for speed
        switch (n_coef_hf[filter]) {
        case 5:
            for (int i = 0; i < line_size; i++) {
                *work_line   += *in_lines_cur[0]++ * coef_hf[filter][0];
                *work_line   += *in_lines_adj[0]++ * coef_hf[filter][0];
                *work_line   += *in_lines_cur[1]++ * coef_hf[filter][1];
                *work_line   += *in_lines_adj[1]++ * coef_hf[filter][1];
                *work_line   += *in_lines_cur[2]++ * coef_hf[filter][2];
                *work_line   += *in_lines_adj[2]++ * coef_hf[filter][2];
                *work_line   += *in_lines_cur[3]++ * coef_hf[filter][3];
                *work_line   += *in_lines_adj[3]++ * coef_hf[filter][3];
                *work_line   += *in_lines_cur[4]++ * coef_hf[filter][4];
                *work_line++ += *in_lines_adj[4]++ * coef_hf[filter][4];
            }
            break;
        case 3:
            for (int i = 0; i < line_size; i++) {
                *work_line   += *in_lines_cur[0]++ * coef_hf[filter][0];
                *work_line   += *in_lines_adj[0]++ * coef_hf[filter][0];
                *work_line   += *in_lines_cur[1]++ * coef_hf[filter][1];
                *work_line   += *in_lines_adj[1]++ * coef_hf[filter][1];
                *work_line   += *in_lines_cur[2]++ * coef_hf[filter][2];
                *work_line++ += *in_lines_adj[2]++ * coef_hf[filter][2];
            }
            break;
        default:
            assert(0);
        }
        /** save scaled result to the output frame, scaling down by 256 * 256 */
        //TODO: set pixel stride for out
        work_pixel = w3fdif->work_line;
        out_pixel = out_line;
        for (j = 0; j < line_size; j++) {
            *out_pixel =  (*work_pixel>(255*256*256)?(255*256*256):(*work_pixel<0?0:*work_pixel))>>16;
            out_pixel++;
            work_pixel++;
        }
        /** move on to next line */
        y_out += 2;
        out_line += dst_line_stride * 2;
    }
    return 0;
}

/** FFmpeg filter integration */

static void set_frame_pts(AVFilterContext *ctx)
{
    W3FDIFContext *w3fdif = ctx->priv;

    int pts;

    if (w3fdif->field) {
        /** calculate the PTS of the new frame */
         if ((pts = ((w3fdif->next->pts - w3fdif->crnt->pts)>>1))) {
             w3fdif->pts_delta = w3fdif->pts_delta?((w3fdif->pts_delta+pts)>>1):pts;
             w3fdif->work->pts = w3fdif->crnt->pts + pts;
         } else if ((pts = ((w3fdif->crnt->pts - w3fdif->prev->pts)>>1))) {
             w3fdif->pts_delta = w3fdif->pts_delta?((w3fdif->pts_delta+pts)>>1):pts;
             w3fdif->work->pts = w3fdif->crnt->pts + pts;
         } else if (w3fdif->pts_delta) {
             w3fdif->work->pts = w3fdif->crnt->pts + w3fdif->pts_delta;
         } else {
             w3fdif->work->pts = AV_NOPTS_VALUE;
         }
    }
}

static void process_frame(AVFilterContext *ctx)
{
    W3FDIFContext *w3fdif = ctx->priv;

    int plane;

    w3fdif->work_line = av_malloc(w3fdif->crnt->linesize[0] * sizeof(uint32_t));

    if (!w3fdif->field) {
        /** do the deinterlacing for field 0 */
        for (plane = 0; plane < 4 && w3fdif->crnt->data[plane]; plane++) {
            deinterlace_component(ctx, w3fdif->crnt, w3fdif->prev, w3fdif->filter, plane);
        }

        /** prev is not neede after this point*/
        if (w3fdif->prev && w3fdif->prev != w3fdif->crnt) {
            avfilter_unref_buffer(w3fdif->prev);
        }
        w3fdif->prev = NULL;

        /** at the end of the first field we _always_ require a new frame
         *  setting next to null is the indicator that a new next frame is required */
        w3fdif->next = NULL;
    } else {
        /** do the deinterlacing for field 1 */
        for (plane = 0; plane < 4 && w3fdif->crnt->data[plane]; plane++) {
            deinterlace_component(ctx, w3fdif->crnt, w3fdif->next, w3fdif->filter, plane);
        }

        /** at the end of the second field we _always_ copy current to previous
         *  and copy next to current */
        w3fdif->prev = w3fdif->crnt;
        w3fdif->crnt = w3fdif->next;
    }

    av_free(w3fdif->work_line);

    /** swap field */
    w3fdif->field = !w3fdif->field;
}

/** public input methods */

static int config_input(AVFilterLink *link)
{
    AVFilterContext *ctx = link->dst;
    W3FDIFContext *w3fdif = ctx->priv;

    int plane;

    /** full an array with the number of bytes that the video
     *  data occupies per line for each plane of the input video */
    for (plane = 0; plane < 4; plane++) {
        w3fdif->line_size[plane] = av_image_get_linesize(
                link->format,
                link->w,
                plane);
    }
    return 0;
}

static void start_frame(AVFilterLink *link, AVFilterBufferRef *picref)
{
    AVFilterContext *ctx = link->dst;
    W3FDIFContext *w3fdif = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];
    int width, height;

    /** we have one new frame so we will be outputting two */
    w3fdif->pending += 2;

    /** store the pointer to the new frame */
    w3fdif->next = avfilter_ref_buffer(picref, ~0);

    /** ensure previous, current and next all contain pointers to frames */
    if (!w3fdif->crnt) w3fdif->crnt = w3fdif->next;
    if (!w3fdif->prev) w3fdif->prev = w3fdif->crnt;

    /** get work-space for output frame */
    width  = FFALIGN(link->w,   32);
    height = FFALIGN(link->h+2, 32);
    w3fdif->work = avfilter_get_video_buffer(
            link,
            AV_PERM_WRITE,
            width,
            height);
    avfilter_copy_buffer_ref_props(w3fdif->work, w3fdif->crnt);
    w3fdif->work->video->interlaced = 0;
    set_frame_pts(ctx);

    avfilter_start_frame(outlink, w3fdif->work);
}

static void null_draw_slice(AVFilterLink *link, int y, int h, int slice_dir) { }

static void end_frame(AVFilterLink *link)
{
    AVFilterContext *ctx = link->dst;
    W3FDIFContext *w3fdif = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];
    AVFilterBufferRef *inpicref = link->cur_buf;

    if (!w3fdif->crnt) {
        return;
    }

    process_frame(ctx);

    avfilter_draw_slice(outlink, 0, outlink->h, 1);
    avfilter_end_frame(outlink);
    avfilter_unref_buffer(inpicref);
}

/** public output methods */

static int poll_frame(AVFilterLink *link, int flush)
{
    W3FDIFContext *w3fdif = link->src->priv;
    int val;

    /** the destination filter is polling us for how many frames we have
     *  if we have pending fields to deinterlace AND we have enough
     *  data to do the deinterlacing then we say so */
    if ((!w3fdif->field && w3fdif->pending) || (w3fdif->field && w3fdif->pending && w3fdif->next)) {
        return 1;
    }

    /**  otherwise we need a frame, so poll our source */
    if ((val=avfilter_poll_frame(link->src->inputs[0], flush))) {
        return 1;
    }

    /**  otherwise check if we are pending and flush is enabled */
    if (w3fdif->pending && flush) {
        w3fdif->flush = flush;
        return 1;
    }

    /** otherwise we're cannot return a frame */
    return 0;
}

static int request_frame(AVFilterLink *link)
{
    AVFilterContext *ctx = link->src;
    W3FDIFContext *w3fdif = ctx->priv;
    int ret, width, height;

    /** if there is no "next" frame AND we are not in flush then get one from our input filter */
    if (!w3fdif->next && !w3fdif->flush) {
        if ((ret = avfilter_request_frame(link->src->inputs[0])) < 0) return ret;
        w3fdif->pending--;
        return 0;
    }

    /** otherwise, make brand-new frame and pass to our output filter */

    if (!w3fdif->crnt && w3fdif->next) w3fdif->crnt = w3fdif->next;
    if (!w3fdif->prev)                 w3fdif->prev = w3fdif->crnt;
    if (!w3fdif->next)                 w3fdif->next = w3fdif->crnt;

    /** get work-space for output frame */
    width  = FFALIGN(link->w,   32);
    height = FFALIGN(link->h+2, 32);
    w3fdif->work = avfilter_get_video_buffer(
            link,
            AV_PERM_WRITE,
            width,
            height);
    avfilter_copy_buffer_ref_props(w3fdif->work, w3fdif->crnt);
    w3fdif->work->video->interlaced = 0;
    set_frame_pts(ctx);

    avfilter_start_frame(link, w3fdif->work);

    process_frame(ctx);

    avfilter_draw_slice(link, 0, link->h, 1);
    avfilter_end_frame(link);
    w3fdif->pending--;
    return 0;
}

/** public general methods */

static av_cold int init(AVFilterContext *ctx, const char *args, void *opaque)
{
    W3FDIFContext *w3fdif = ctx->priv;

    if (!args) {
        w3fdif->filter = 1;
    } else if (sscanf(args, "%u", &w3fdif->filter) == 1) {
        w3fdif->filter = !!w3fdif->filter;
    } else {
        av_log(ctx, AV_LOG_ERROR, "Invalid argument '%s'.\n", args);
        return AVERROR(EINVAL);
    }

    av_log(ctx, AV_LOG_INFO, "using %s filter\n",
            w3fdif->filter ? "more complex" : "simple");

    return 0;
}

static av_cold void uninit(AVFilterContext *ctx)
{
    W3FDIFContext *w3fdif = ctx->priv;

    if (w3fdif->prev && (w3fdif->prev != w3fdif->crnt)) avfilter_unref_buffer(w3fdif->prev);
    if (w3fdif->next && (w3fdif->next != w3fdif->crnt)) avfilter_unref_buffer(w3fdif->next);
    if (w3fdif->crnt) avfilter_unref_buffer(w3fdif->crnt);
}

static int query_formats(AVFilterContext *ctx)
{
    static const enum PixelFormat pix_fmts[] = {
//        PIX_FMT_YUV420P,
        PIX_FMT_YUV422P,
//        PIX_FMT_YUV444P,
//        PIX_FMT_YUV410P,
//        PIX_FMT_YUV411P,
//        PIX_FMT_GRAY8,
//        PIX_FMT_YUVJ420P,
//        PIX_FMT_YUVJ422P,
//        PIX_FMT_YUVJ444P,
//        AV_NE( PIX_FMT_GRAY16BE, PIX_FMT_GRAY16LE ),
//        PIX_FMT_YUV440P,
//        PIX_FMT_YUVJ440P,
//        AV_NE( PIX_FMT_YUV420P16BE, PIX_FMT_YUV420P16LE ),
//        AV_NE( PIX_FMT_YUV422P16BE, PIX_FMT_YUV422P16LE ),
//        AV_NE( PIX_FMT_YUV444P16BE, PIX_FMT_YUV444P16LE ),
        PIX_FMT_NONE
    };

    avfilter_set_common_pixel_formats(ctx, avfilter_make_format_list(pix_fmts));

    return 0;
}

AVFilter avfilter_vf_w3fdif = {
    .name          = "w3fdif",
    .description   = NULL_IF_CONFIG_SMALL("Martin Weston three field deinterlace"),
    .priv_size     = sizeof(W3FDIFContext),
    .init          = init,
    .uninit        = uninit,
    .query_formats = query_formats,
    .inputs        = (const AVFilterPad[]) {{
                                            .name           = "default",
                                            .type           = AVMEDIA_TYPE_VIDEO,
                                            .config_props   = config_input,
                                            .start_frame    = start_frame,
                                            .draw_slice     = null_draw_slice,
                                            .end_frame      = end_frame, },
                                            {.name = NULL}},
    .outputs       = (const AVFilterPad[]) {{
                                            .name           = "default",
                                            .type           = AVMEDIA_TYPE_VIDEO,
                                            .poll_frame     = poll_frame,
                                            .request_frame  = request_frame, },
                                           {.name = NULL}},
};
