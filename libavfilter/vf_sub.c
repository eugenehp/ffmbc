/*
 * Copyright (c) 2011 Baptiste Coudurier
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
 * Subtitles burning filter.
 */

#include <fcntl.h>
#include <unistd.h>
#include <strings.h>
#include <ass/ass.h>
#include "libavutil/parseutils.h"
#include "libavutil/intreadwrite.h"
#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"
#include "libavutil/colorspace.h"
#include "libavutil/mathematics.h"
#include "libavutil/avstring.h"
#include "avfilter.h"
#include "drawutils.h"

typedef struct {
    int64_t start;
    int64_t end;
    char *text;
} Subtitle;

typedef struct {
    const AVClass *class;
    FFDrawContext dc;
    char *text;
    char *fontpath;
    char *filepath;
    char *textcolor_string;
    int drawbox;
    float fontsize;
    char *boxcolor_string;
    FFDrawColor boxcolor;
    FFDrawColor textcolor;
    Subtitle *subtitles;
    unsigned subtitles_count;
    ASS_Library *ass_library;
    ASS_Renderer *ass_renderer;
    ASS_Track *ass_track;
    int hsub, vsub;
    int vmargin;
    int font_encoding;
} SubContext;

static void message_callback(int level, const char *format, va_list va, void *ctx)
{
    int n;
    char *str;
    va_list dst;

    if (level >= 6 && av_log_get_level() < AV_LOG_DEBUG)
        return;
    va_copy(dst, va);
    n = vsnprintf(NULL, 0, format, va);
    if (n > 0 && (str = av_malloc(n + 1))) {
        vsnprintf(str, n + 1, format, dst);
        av_log(ctx, AV_LOG_INFO, "[ass] %s\n", str);
        av_free(str);
    }
}

static ASS_Library *ass_init(AVFilterContext *ctx)
{
    ASS_Library *priv;
    priv = ass_library_init();
    ass_set_message_cb(priv, message_callback, ctx);
    return priv;
}

static ASS_Track *ass_default_track(AVFilterContext *ctx)
{
    SubContext *sub = ctx->priv;
    ASS_Track *track = ass_new_track(sub->ass_library);
    ASS_Style *style;
    int sid;

    track->track_type = TRACK_TYPE_ASS;
    track->PlayResX = 1920;
    track->PlayResY = 1080;
    track->WrapStyle = 0;
    track->Kerning = 1;

    sid = ass_alloc_style(track);
    style = track->styles + sid;

    style->treat_fontname_as_pattern = 1;
    style->Name = av_strdup("Default");
    style->FontName = sub->fontpath ? sub->fontpath : av_strdup("Arial");
    style->FontSize = sub->fontsize;
    style->Encoding = sub->font_encoding;

    style->PrimaryColour = (AV_RB24(sub->textcolor.rgba) << 8) | (255 - sub->textcolor.rgba[3]);
    style->OutlineColour = (AV_RB24(sub->boxcolor.rgba)  << 8) | (255 - sub->boxcolor.rgba[3]);
    style->BorderStyle = 1 + 2*sub->drawbox;
    style->Alignment = 2;
    style->Outline = 5;
    style->Spacing = 5 - 2*sub->drawbox;
    style->Shadow = 0;
    style->MarginL = 10;
    style->MarginR = 10;
    style->MarginV = sub->vmargin;
    style->ScaleX = 1.0;
    style->ScaleY = 1.0;

    return track;
}

static int ass_create_event(ASS_Track *track, Subtitle *s)
{
    int eid = ass_alloc_event(track);
    ASS_Event *event = track->events + eid;

    event->Start = s->start;
    event->Duration = s->end - s->start;
    event->Text = s->text;

    return 0;
}

static int html_color_parse(void *log_ctx, const char *str)
{
    uint8_t rgba[4];
    if (av_parse_color(rgba, str, strcspn(str, "\" >"), log_ctx) < 0)
        return -1;
    return rgba[0] | rgba[1] << 8 | rgba[2] << 16;
}

enum {
    PARAM_UNKNOWN = -1,
    PARAM_SIZE,
    PARAM_COLOR,
    PARAM_FACE,
    PARAM_NUMBER
};

typedef struct {
    char tag[128];
    char param[PARAM_NUMBER][128];
} SrtStack;

static const char *srt_to_ass(void *log_ctx, char *out, char *out_end,
                              const char *in, int x1, int y1, int x2, int y2)
{
    char c, *param, buffer[128], tmp[128];
    int len, tag_close, sptr = 1, line_start = 1, an = 0, end = 0;
    SrtStack stack[16];
    char *start = out;

    stack[0].tag[0] = 0;
    strcpy(stack[0].param[PARAM_SIZE],  "{\\fs}");
    strcpy(stack[0].param[PARAM_COLOR], "{\\c}");
    strcpy(stack[0].param[PARAM_FACE],  "{\\fn}");

    if (x1 >= 0 && y1 >= 0) {
        if (x2 >= 0 && y2 >= 0 && (x2 != x1 || y2 != y1))
            out += snprintf(out, out_end-out,
                            "{\\an1}{\\move(%d,%d,%d,%d)}", x1, y1, x2, y2);
        else
            out += snprintf(out, out_end-out, "{\\an1}{\\pos(%d,%d)}", x1, y1);
    }

    for (; out < out_end && !end && *in; in++) {
        switch (*in) {
        case '\r':
            break;
        case '\n':
            if (line_start) {
                end = 1;
                break;
            }
            while (out[-1] == ' ')
                out--;
            out += snprintf(out, out_end-out, "\\N");
            line_start = 1;
            break;
        case ' ':
            if (!line_start)
                *out++ = *in;
            break;
        case '{':    /* skip all {\xxx} substrings except for {\an%d}
                        and all microdvd like styles such as {Y:xxx} */
            an += sscanf(in, "{\\an%*1u}%c", &c) == 1;
            if ((an != 1 && sscanf(in, "{\\%*[^}]}%n%c", &len, &c) > 0) ||
                sscanf(in, "{%*1[CcFfoPSsYy]:%*[^}]}%n%c", &len, &c) > 0) {
                in += len - 1;
            } else
                *out++ = *in;
            break;
        case '<':
            tag_close = in[1] == '/';
            if (sscanf(in+tag_close+1, "%127[^>]>%n%c", buffer, &len,&c) >= 2) {
                if ((param = strchr(buffer, ' ')))
                    *param++ = 0;
                if ((!tag_close && sptr < FF_ARRAY_ELEMS(stack)) ||
                    ( tag_close && sptr > 0 && !strcmp(stack[sptr-1].tag, buffer))) {
                    int i, j, unknown = 0;
                    in += len + tag_close;
                    if (!tag_close)
                        memset(stack+sptr, 0, sizeof(*stack));
                    if (!strcmp(buffer, "font")) {
                        if (tag_close) {
                            for (i=PARAM_NUMBER-1; i>=0; i--)
                                if (stack[sptr-1].param[i][0])
                                    for (j=sptr-2; j>=0; j--)
                                        if (stack[j].param[i][0]) {
                                            out += snprintf(out, out_end-out,
                                                            stack[j].param[i]);
                                            break;
                                        }
                        } else {
                            while (param) {
                                if (!strncmp(param, "size=", 5)) {
                                    unsigned font_size;
                                    param += 5 + (param[5] == '"');
                                    if (sscanf(param, "%u", &font_size) == 1) {
                                        snprintf(stack[sptr].param[PARAM_SIZE],
                                             sizeof(stack[0].param[PARAM_SIZE]),
                                             "{\\fs%u}", font_size);
                                    }
                                } else if (!strncmp(param, "color=", 6)) {
                                    param += 6 + (param[6] == '"');
                                    snprintf(stack[sptr].param[PARAM_COLOR],
                                         sizeof(stack[0].param[PARAM_COLOR]),
                                         "{\\c&H%X&}",
                                         html_color_parse(log_ctx, param));
                                } else if (!strncmp(param, "face=", 5)) {
                                    param += 5 + (param[5] == '"');
                                    len = strcspn(param,
                                                  param[-1] == '"' ? "\"" :" ");
                                    av_strlcpy(tmp, param,
                                               FFMIN(sizeof(tmp), len+1));
                                    param += len;
                                    snprintf(stack[sptr].param[PARAM_FACE],
                                             sizeof(stack[0].param[PARAM_FACE]),
                                             "{\\fn%s}", tmp);
                                }
                                if ((param = strchr(param, ' ')))
                                    param++;
                            }
                            for (i=0; i<PARAM_NUMBER; i++)
                                if (stack[sptr].param[i][0])
                                    out += snprintf(out, out_end-out,
                                                    stack[sptr].param[i]);
                        }
                    } else if (!buffer[1] && strspn(buffer, "bisu") == 1) {
                        out += snprintf(out, out_end-out,
                                        "{\\%c%d}", buffer[0], !tag_close);
                    } else {
                        unknown = 1;
                        snprintf(tmp, sizeof(tmp), "</%s>", buffer);
                    }
                    if (tag_close) {
                        sptr--;
                    } else if (unknown && !strstr(in, tmp)) {
                        in -= len + tag_close;
                        *out++ = *in;
                    } else
                        av_strlcpy(stack[sptr++].tag, buffer,
                                   sizeof(stack[0].tag));
                    break;
                }
            }
        default:
            *out++ = *in;
            break;
        }
        if (*in != ' ' && *in != '\r' && *in != '\n')
            line_start = 0;
    }

    while (out > start+1 && !strncmp(out-2, "\\N", 2))
        out -= 2;
    while (out > start && out[-1] == ' ')
        out--;
    *out = 0;
    return in;
}

static const char *srt_read_ts(const char *buf, int64_t *ts_start, int64_t *ts_end,
                               int *x1, int *y1, int *x2, int *y2)
{
    int i, hs, ms, ss, he, me, se;

    for (i=0; i<2; i++) {
        /* try to read timestamps in either the first or second line */
        int c = sscanf(buf, "%d:%2d:%2d%*1[,.]%3"PRId64" --> %d:%2d:%2d%*1[,.]%3"PRId64
                       "%*[ ]X1:%u X2:%u Y1:%u Y2:%u",
                       &hs, &ms, &ss, ts_start, &he, &me, &se, ts_end,
                       x1, x2, y1, y2);
        buf += strcspn(buf, "\n") + 1;
        if (c >= 8) {
            *ts_start = 1000*(ss + 60*(ms + 60*hs)) + *ts_start;
            *ts_end   = 1000*(se + 60*(me + 60*he)) + *ts_end;
            return buf;
        }
    }
    return NULL;
}

static void detect_font_encoding(AVFilterContext *ctx, uint8_t *text)
{
    SubContext *sub = ctx->priv;
    uint8_t *p = text;
    int i;
    for (i = 0; i < 100 && *p; i++) {
        uint32_t code;
        GET_UTF8(code, *p++, continue;);
        if (code >= 0x0590 && code <= 0x05FF) {
            sub->font_encoding = 177; // hebrew
            return;
        }
    }
    sub->font_encoding = 1; // auto
}

static int sub_parse_srt(AVFilterContext *ctx)
{
    SubContext *sub = ctx->priv;
    int fd = open(sub->filepath, 0);
    int file_size, ret, num, count = 0;
    char *buf, *end;
    const char *p;

    if (fd < 0) {
        av_log(ctx, AV_LOG_ERROR, "Error opening subtitle file '%s'\n", sub->filepath);
        return AVERROR(EIO);
    }

    file_size = lseek(fd, 0, SEEK_END);
    if (file_size <= 0) {
        av_log(ctx, AV_LOG_ERROR, "Error getting subtitle file size\n");
        return AVERROR(EIO);
    }
    lseek(fd, 0, SEEK_SET);

    buf = av_malloc(file_size+1);
    if (!buf)
        return AVERROR(ENOMEM);
    if (read(fd, buf, file_size) != file_size) {
        av_log(ctx, AV_LOG_ERROR, "Error reading subtitle file\n");
        av_free(buf);
        return AVERROR(EIO);
    }
    buf[file_size] = 0;
    end = buf+file_size;

    detect_font_encoding(ctx, buf);

    sub->ass_track = ass_default_track(ctx);

    for (p = buf; p < end && *p;) {
        Subtitle s;
        char buffer[2048];
        int x1 = -1, y1 = -1, x2 = -1, y2 = -1;
        while (*p == '\n' || *p == '\r')
            p++;
        if (!sscanf(p, "%d", &num)) {
            av_log(ctx, AV_LOG_ERROR, "Error parsing subtitle number: last %d\n", num);
            return -1;
        }
        p += strcspn(p, "\n") + 1;
        p = srt_read_ts(p, &s.start, &s.end, &x1, &y1, &x2, &y2);
        if (!p)
            break;
        p = srt_to_ass(ctx, buffer, buffer+sizeof(buffer), p,
                       x1, y1, x2, y2);
        s.text = av_strdup(buffer);
        //av_log(ctx, AV_LOG_INFO, "n: %d s: %lld e:%lld text '%s'\n", num, s.start, s.end, s.text);
        ret = ass_create_event(sub->ass_track, &s);
        if (ret < 0)
            return -1;
        count++;
    }

    av_free(buf);
    close(fd);

    av_log(ctx, AV_LOG_INFO, "loaded %d subtitles\n", count);

    return 0;
}

#define OFFSET(x) offsetof(SubContext, x)

static const AVOption sub_options[]= {
    {"file",     "set subtitles file",   OFFSET(filepath),           FF_OPT_TYPE_STRING, {.str=NULL},  CHAR_MIN, CHAR_MAX },
    {"font",     "set font file",        OFFSET(fontpath),           FF_OPT_TYPE_STRING, {.str=NULL},  CHAR_MIN, CHAR_MAX },
    {"textcolor","set foreground color", OFFSET(textcolor_string),   FF_OPT_TYPE_STRING, {.str=NULL},  CHAR_MIN, CHAR_MAX },
    {"boxcolor", "set box color",        OFFSET(boxcolor_string),    FF_OPT_TYPE_STRING, {.str=NULL},  CHAR_MIN, CHAR_MAX },
    {"box",      "set box",              OFFSET(drawbox),            FF_OPT_TYPE_INT,    {.dbl=0},     0,        1        },
    {"fontsize", "set font size",        OFFSET(fontsize),           FF_OPT_TYPE_FLOAT,  {.dbl=62},    1,        65536    },
    {"text",     "set osd text",         OFFSET(text),               FF_OPT_TYPE_STRING, {.str=NULL},  CHAR_MIN, CHAR_MAX },
    {"vmargin",  "set vertical margin",  OFFSET(vmargin),            FF_OPT_TYPE_INT,    {.dbl=100},   0,        65536    },
    {0},
};

static const char *sub_get_name(void *ctx)
{
    return "sub";
}

static const AVClass sub_class = {
    "SubContext",
    sub_get_name,
    sub_options
};

static av_cold int init(AVFilterContext *ctx, const char *args, void *opaque)
{
    SubContext *sub = ctx->priv;
    int ret;

    sub->class = &sub_class;
    av_opt_set_defaults2(sub, 0, 0);

    sub->textcolor_string = av_strdup("white");
    sub->boxcolor_string = av_strdup("black");

    if ((ret = (av_set_options_string(sub, args, "=", ":"))) < 0) {
        av_log(ctx, AV_LOG_ERROR, "Error parsing options string: '%s'\n", args);
        return ret;
    }

    if (!sub->filepath && !sub->text) {
        av_log(ctx, AV_LOG_ERROR, "Please supply a subtitles file path");
        return -1;
    }

    if ((ret = av_parse_color(sub->textcolor.rgba, sub->textcolor_string, -1, ctx))) {
        av_log(ctx, AV_LOG_ERROR, "Invalid text color '%s'\n", sub->textcolor_string);
        return ret;
    }

    if ((ret = av_parse_color(sub->boxcolor.rgba, sub->boxcolor_string, -1, ctx))) {
        av_log(ctx, AV_LOG_ERROR, "Invalid box color '%s'\n", sub->boxcolor_string);
        return ret;
    }

    sub->ass_library = ass_init(ctx);
    if (!sub->ass_library)
        return AVERROR(ENOMEM);
    sub->ass_renderer = ass_renderer_init(sub->ass_library);
    if (!sub->ass_renderer)
        return AVERROR(ENOMEM);

    if (sub->fontpath) {
        FILE *f = fopen(sub->fontpath, "rb");
        uint8_t *buf;
        int size;
        if (!f) {
            av_log(ctx, AV_LOG_ERROR, "Error opening font path '%s'\n", sub->fontpath);
            return -1;
        }
        fseek(f, 0, SEEK_END);
        size = ftell(f);
        fseek(f, 0, SEEK_SET);
        if (size <= 0) {
            av_log(ctx, AV_LOG_ERROR, "Error getting font file size\n");
            return -1;
        }
        buf = av_malloc(size);
        if (!buf)
            return AVERROR(ENOMEM);
        if (fread(buf, size, 1, f) != 1) {
            av_log(ctx, AV_LOG_ERROR, "Error reading font file\n");
            return -1;
        }
        ass_add_font(sub->ass_library, sub->fontpath, buf, size);
    }

    if (sub->text) {
        Subtitle s;
        s.start = 0;
        s.end = INT64_MAX;
        s.text = sub->text;
        sub->ass_track = ass_default_track(ctx);
        ass_create_event(sub->ass_track, &s);
    } else if (strstr(sub->filepath, ".ass")) {
        sub->ass_track = ass_read_file(sub->ass_library, sub->filepath, NULL);
        if (!sub->ass_track)
            return -1;
    } else if (strstr(sub->filepath, ".srt")) {
        if (sub_parse_srt(ctx) < 0)
            return -1;
    } else {
        av_log(ctx, AV_LOG_ERROR, "please supply either a srt or ass file "
               "with correct filename extension\n");
        return -1;
    }

    ass_set_fonts(sub->ass_renderer, sub->fontpath, NULL, !sub->fontpath, NULL, 1);

    return 0;
}

static av_cold void uninit(AVFilterContext *ctx)
{
    SubContext *sub = ctx->priv;

    if (sub->ass_track)
        ass_free_track(sub->ass_track);
    if (sub->ass_renderer)
        ass_renderer_done(sub->ass_renderer);
    if (sub->ass_library)
        ass_library_done(sub->ass_library);

    av_freep(&sub->filepath);
    av_freep(&sub->textcolor_string);
    av_freep(&sub->boxcolor_string);
}

static int query_formats(AVFilterContext *ctx)
{
    avfilter_set_common_pixel_formats(ctx, ff_draw_supported_pixel_formats(0));
    return 0;
}

static int config_input(AVFilterLink *link)
{
    AVFilterContext *ctx = link->dst;
    SubContext *sub = ctx->priv;
    AVRational dar;
    AVRational sar;

    ff_draw_init(&sub->dc, link->format, 0);
    ff_draw_color(&sub->dc, &sub->textcolor, sub->textcolor.rgba);
    ff_draw_color(&sub->dc, &sub->boxcolor, sub->boxcolor.rgba);

    dar.num = link->w*link->sample_aspect_ratio.num;
    dar.den = link->h*link->sample_aspect_ratio.den;
    sar.num = link->w;
    sar.den = link->h;

    ass_set_aspect_ratio(sub->ass_renderer, av_q2d(sar), av_q2d(dar));
    ass_set_frame_size(sub->ass_renderer, link->w, link->h);

    return 0;
}

static AVFilterBufferRef *get_video_buffer(AVFilterLink *link, int perms, int w, int h)
{
    AVFilterBufferRef *picref = avfilter_get_video_buffer(link->dst->outputs[0],
                                                           perms, w, h);
    return picref;
}

static void start_frame(AVFilterLink *link, AVFilterBufferRef *picref)
{
    AVFilterBufferRef *outpicref = avfilter_ref_buffer(picref, ~0);

    link->dst->outputs[0]->out_buf = outpicref;

    avfilter_start_frame(link->dst->outputs[0], outpicref);
}

static void null_draw_slice(AVFilterLink *link, int y, int h, int slice_dir)
{
}

#define R(c)  ((c)>>24)
#define G(c)  (((c)>>16)&0xFF)
#define B(c)  (((c)>>8)&0xFF)
#define A(c)  ((c)&0xFF)

static void blend_ass_image(AVFilterContext *ctx, AVFilterBufferRef *ref, ASS_Image *img)
{
    SubContext *sub = ctx->priv;
    int opacity = 255 - A(img->color);
    FFDrawColor color;
    uint8_t rgba_color[] = { R(img->color), G(img->color), B(img->color), opacity };

    ff_draw_color(&sub->dc, &color, rgba_color);
    ff_blend_mask(&sub->dc, &color,
                  ref->data, ref->linesize,
                  ref->video->w, ref->video->h,
                  img->bitmap, img->stride, img->w, img->h,
                  3, 0, img->dst_x, img->dst_y);
}

static void end_frame(AVFilterLink *link)
{
    AVFilterContext *ctx = link->dst;
    SubContext *sub = link->dst->priv;
    int64_t pts = av_rescale_q(link->cur_buf->pts, link->time_base,
                               (AVRational){ 1, 1000 });
    int detect_change;
    ASS_Image *img = ass_render_frame(sub->ass_renderer, sub->ass_track,
                                      pts, &detect_change);

    if (sub->drawbox == 1 && img) {
        ASS_Image box, *i = img;
        int min_x, min_y, max_x, max_y, prev_y, prev_x, last_line_y = 0;
    compute_box:
        min_x = link->w;
        min_y = link->h;
        max_x = max_y = 0;
        prev_y = link->h;
        prev_x = 0;
        for (; i; i = i->next) {
            if (i->color == sub->ass_track->styles[0].OutlineColour) {
                if (i->dst_x < prev_x && i->dst_y > prev_y) // new line
                    break;
                min_x = FFMIN(i->dst_x, min_x);
                min_y = FFMIN(i->dst_y, min_y);
                max_x = FFMAX(i->dst_x + i->w, max_x);
                max_y = FFMAX(i->dst_y + i->h, max_y);
                prev_y = i->dst_y+i->h/2;
                prev_x = i->dst_x;
            }
        }
        min_y = FFMAX(last_line_y, min_y);
        box.stride = box.w = max_x - min_x;
        box.h = max_y - min_y;
        box.dst_x = min_x;
        box.dst_y = min_y;
        box.bitmap = av_malloc(box.w * box.h);
        box.color = sub->ass_track->styles[0].OutlineColour;
        memset(box.bitmap, 0xff, box.w * box.h);
        blend_ass_image(ctx, link->cur_buf, &box);
        last_line_y = max_y;
        av_free(box.bitmap);
        if (i)
            goto compute_box;
    }

    for (; img; img = img->next) {
        // skip box outline pictures
        if (sub->drawbox == 1 &&
            img->color == sub->ass_track->styles[0].OutlineColour)
            continue;
        blend_ass_image(ctx, link->cur_buf, img);
    }

    avfilter_draw_slice(link->dst->outputs[0], 0, link->cur_buf->video->h, 1);
    avfilter_end_frame(link->dst->outputs[0]);
    avfilter_unref_buffer(link->cur_buf);
}

AVFilter avfilter_vf_sub = {
    .name          = "sub",
    .description   = NULL_IF_CONFIG_SMALL("Burn subtitles"),
    .priv_size     = sizeof(SubContext),
    .query_formats = query_formats,
    .init          = init,
    .uninit        = uninit,
    .inputs        = (AVFilterPad[]) {{ .name             = "default",
                                        .type             = AVMEDIA_TYPE_VIDEO,
                                        .get_video_buffer = get_video_buffer,
                                        .start_frame      = start_frame,
                                        .draw_slice       = null_draw_slice,
                                        .end_frame        = end_frame,
                                        .config_props     = config_input,
                                        .min_perms        = AV_PERM_WRITE | AV_PERM_READ,
                                        .rej_perms        = AV_PERM_PRESERVE },
                                      { .name = NULL}},
    .outputs       = (AVFilterPad[]) {{ .name             = "default",
                                        .type             = AVMEDIA_TYPE_VIDEO, },
                                      { .name = NULL}},
};
