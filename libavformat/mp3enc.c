/*
 * MP3 muxer
 * Copyright (c) 2003 Fabrice Bellard
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

#include <strings.h>
#include "avformat.h"
#include "avio_internal.h"
#include "id3v1.h"
#include "id3v2.h"
#include "rawenc.h"
#include "libavutil/avstring.h"
#include "libavcodec/mpegaudio.h"
#include "libavcodec/mpegaudiodata.h"
#include "libavcodec/mpegaudiodecheader.h"
#include "libavutil/intreadwrite.h"
#include "libavutil/opt.h"
#include "libavcodec/mpegaudio.h"
#include "libavcodec/mpegaudiodata.h"
#include "libavcodec/mpegaudiodecheader.h"
#include "libavformat/avio_internal.h"
#include "libavutil/dict.h"
#include "libavutil/avstring.h"

/* simple formats */

static int len_put_str16(const char *str)
{
    const uint8_t *q = str;
    int ret = 0;
    uint32_t ch;

    do {
        uint16_t tmp;
        tmp = tmp;
        GET_UTF8(ch, *q++, break;);
        PUT_UTF16(ch, tmp, ret += 2;);
    } while (ch);
    return ret;
}

static void id3v2_put_size(AVFormatContext *s, int size)
{
    avio_w8(s->pb, size >> 21 & 0x7f);
    avio_w8(s->pb, size >> 14 & 0x7f);
    avio_w8(s->pb, size >> 7  & 0x7f);
    avio_w8(s->pb, size       & 0x7f);
}

static int string_is_ascii(const uint8_t *str)
{
    while (*str && *str < 128) str++;
    return !*str;
}

#define VBR_NUM_BAGS 400
#define VBR_TOC_SIZE 100
typedef struct MP3Context {
    const AVClass *class;
    int id3v2_version;
    int64_t frames_offset;
    int32_t frames;
    int32_t size;
    uint32_t want;
    uint32_t seen;
    uint32_t pos;
    uint64_t bag[VBR_NUM_BAGS];
    int initial_bitrate;
    int has_variable_bitrate;
} MP3Context;

static int id3v2_put_tag(AVFormatContext *s, const char *key, const char *value, int version)
{
    int len, encoding;

    if (version == 4)
        encoding = 3; // utf-8
    else if (string_is_ascii(value))
        encoding = 0; // iso-8859-1
    else
        encoding = 1; // utf-16be

    if (encoding == 1)
        len = 2+len_put_str16(value);
    else
        len = strlen(value)+1;

    avio_wtag(s->pb, key);
    if (version == 4)
        id3v2_put_size(s, 1+len);
    else
        avio_wb32(s->pb, 1+len);
    avio_wb16(s->pb, 0);
    avio_w8(s->pb, encoding);
    if (encoding == 1) {
        avio_wb16(s->pb, 0xfffe); // BOM
        avio_put_str16le(s->pb, value);
    } else {
        avio_write(s->pb, value, len);
    }
    return 4+4+2+1+len;
}

static int id3v2_put_apic(AVFormatContext *s, AVDictionaryEntry *tag, int version)
{
    const char *mime = av_metadata_get_attribute(tag, "mime");
    int len;
    if (!mime) {
        av_log(s, AV_LOG_ERROR, "error, no mime type set for cover\n");
        return 0;
    }
    avio_wtag(s->pb, "APIC");
    len = 1+strlen(mime)+1+1+1+tag->len;
    if (version == 4)
        id3v2_put_size(s, len);
    else
        avio_wb32(s->pb, len);
    avio_wb16(s->pb, 0); // flags
    avio_w8(s->pb, 0); // encoding
    avio_put_str(s->pb, mime);
    avio_w8(s->pb, 3); // type, cover front
    avio_w8(s->pb, 0); // description
    avio_write(s->pb, tag->value, tag->len);
    return 4+4+2+len;
}

static int id3v2_put_uslt(AVFormatContext *s, AVDictionaryEntry *tag, int version)
{
    const char *lang = av_metadata_get_attribute(tag, "language");
    int data_len, len, encoding;

    if (version == 4)
        encoding = 3; // utf-8
    else if (string_is_ascii(tag->value))
        encoding = 0; // iso-8859-1
    else
        encoding = 1; // utf-16be

    if (encoding == 1)
        data_len = 2+2 + 2+len_put_str16(tag->value);
    else
        data_len = 1 + strlen(tag->value)+1;
    len = 1+3+data_len;

    avio_wtag(s->pb, "USLT");
    if (version == 4)
        id3v2_put_size(s, len);
    else
        avio_wb32(s->pb, len);
    avio_wb16(s->pb, 0); // flags
    avio_w8(s->pb, encoding);
    if (!lang)
        lang = "eng";
    avio_w8(s->pb, lang[0]);
    avio_w8(s->pb, lang[1]);
    avio_w8(s->pb, lang[2]);
    if (encoding != 1) {
        avio_w8(s->pb, 0); // description
        avio_put_str(s->pb, tag->value);
    } else {
        avio_wb16(s->pb, 0xfffe); // BOM
        avio_wb16(s->pb, 0); // description
        avio_wb16(s->pb, 0xfffe);
        avio_put_str16le(s->pb, tag->value);
    }
    return 4+4+2+len;
}

#if CONFIG_MP2_MUXER
AVOutputFormat ff_mp2_muxer = {
    .name              = "mp2",
    .long_name         = NULL_IF_CONFIG_SMALL("MPEG audio layer 2"),
    .mime_type         = "audio/x-mpeg",
    .extensions        = "mp2,m2a",
    .audio_codec       = CODEC_ID_MP2,
    .video_codec       = CODEC_ID_NONE,
    .write_packet      = ff_raw_write_packet,
};
#endif

#if CONFIG_MP3_MUXER

static const AVOption options[] = {
    { "id3v2_version", "Select ID3v2 version to write. Currently 3 and 4 are supported.",
      offsetof(MP3Context, id3v2_version), FF_OPT_TYPE_INT, {.dbl = 3}, 3, 4, AV_OPT_FLAG_ENCODING_PARAM},
    { NULL },
};

static const AVClass mp3_muxer_class = {
    .class_name     = "MP3 muxer",
    .item_name      = av_default_item_name,
    .option         = options,
    .version        = LIBAVUTIL_VERSION_INT,
};

static int id3v2_write_tag(AVFormatContext *s, AVDictionaryEntry *t)
{
    MP3Context *mp3 = s->priv_data;
    int version = mp3->id3v2_version;
    int version_flags, i;

    if (!strcmp(t->key, "cover"))
        return id3v2_put_apic(s, t, version);
    else if (!strcmp(t->key, "lyrics"))
        return id3v2_put_uslt(s, t, version);

    version_flags = version == 4 ? 2 : 1;

    for (i = 0; ff_id3v2_tags[i].tag; i++) {
        if (!strcmp(t->key, ff_id3v2_tags[i].name) &&
            ff_id3v2_tags[i].valid_version & version_flags) {
            return id3v2_put_tag(s, ff_id3v2_tags[i].tag, t->value, version);
        }
    }
    return 0;
}

static const int64_t xing_offtbl[2][2] = {{32, 17}, {17,9}};

/*
 * Write an empty XING header and initialize respective data.
 */
static int mp3_write_xing(AVFormatContext *s)
{
    AVCodecContext   *codec = s->streams[0]->codec;
    MP3Context       *mp3 = s->priv_data;
    int              bitrate_idx;
    int              best_bitrate_idx = 0;
    int              best_bitrate_error= INT_MAX;
    int64_t          xing_offset;
    int32_t          mask, header;
    MPADecodeHeader  c;
    int              srate_idx, i, channels;
    int              needed;

    for (i = 0; i < FF_ARRAY_ELEMS(ff_mpa_freq_tab); i++)
        if (ff_mpa_freq_tab[i] == codec->sample_rate) {
            srate_idx = i;
            break;
        }
    if (i == FF_ARRAY_ELEMS(ff_mpa_freq_tab)) {
        av_log(s, AV_LOG_ERROR, "Unsupported sample rate.\n");
        return -1;
    }

    switch (codec->channels) {
    case 1:  channels = MPA_MONO;                                          break;
    case 2:  channels = MPA_STEREO;                                        break;
    default: av_log(s, AV_LOG_ERROR, "Unsupported number of channels.\n"); return -1;
    }

    /* dummy MPEG audio header */
    header  =  0xff                                  << 24; // sync
    header |= (0x7 << 5 | 0x3 << 3 | 0x1 << 1 | 0x1) << 16; // sync/mpeg-1/layer 3/no crc*/
    header |= (srate_idx << 2) <<  8;
    header |= channels << 6;

    for (bitrate_idx=1; bitrate_idx<15; bitrate_idx++) {
        int error;
        ff_mpegaudio_decode_header(&c, header | (bitrate_idx << (4+8)));
        error= FFABS(c.bit_rate - codec->bit_rate);
        if(error < best_bitrate_error){
            best_bitrate_error= error;
            best_bitrate_idx  = bitrate_idx;
        }
    }

    for (bitrate_idx= best_bitrate_idx;; bitrate_idx++) {
        if (15 == bitrate_idx)
            return -1;

        mask = (bitrate_idx << 4) <<  8;
        header |= mask;
        ff_mpegaudio_decode_header(&c, header);
        xing_offset=xing_offtbl[c.lsf == 1][c.nb_channels == 1];
        needed = 4              // header
               + xing_offset
               + 4              // xing tag
               + 4              // frames/size/toc flags
               + 4              // frames
               + 4              // size
               + VBR_TOC_SIZE;  // toc

        if (needed <= c.frame_size)
            break;

        header &= ~mask;
        ++bitrate_idx;
    }

    avio_wb32(s->pb, header);
    avio_fill(s->pb, 0, xing_offset);
    avio_wb32(s->pb, MKBETAG('X', 'i', 'n', 'g'));
    avio_wb32(s->pb, 0x01 | 0x02 | 0x04);  // frames/size/toc

    mp3->frames_offset = avio_tell(s->pb);
    mp3->size = c.frame_size;
    mp3->want=1;
    mp3->seen=0;
    mp3->pos=0;

    avio_wb32(s->pb, 0);  // frames
    avio_wb32(s->pb, 0);  // size

    // toc
    for (i = 0; i < VBR_TOC_SIZE; ++i)
        avio_w8(s->pb, (uint8_t)(255 * i / VBR_TOC_SIZE));

    avio_fill(s->pb, 0, c.frame_size - needed);
    avio_flush(s->pb);

    return 0;
}

/*
 * Add a frame to XING data.
 * Following lame's "VbrTag.c".
 */
static void mp3_xing_add_frame(AVFormatContext *s, AVPacket *pkt)
{
    MP3Context  *mp3 = s->priv_data;
    int i;

    ++mp3->frames;
    mp3->size += pkt->size;

    if (mp3->want == ++mp3->seen) {
        mp3->bag[mp3->pos] = mp3->size;

        if (VBR_NUM_BAGS == ++mp3->pos) {
            /* shrink table to half size by throwing away each second bag. */
            for (i = 1; i < VBR_NUM_BAGS; i += 2)
                mp3->bag[i >> 1] = mp3->bag[i];

            /* double wanted amount per bag. */
            mp3->want <<= 1;
            /* adjust current position to half of table size. */
            mp3->pos >>= 1;
        }

        mp3->seen = 0;
    }
}

static void mp3_fix_xing(AVFormatContext *s)
{
    MP3Context  *mp3 = s->priv_data;
    int i;

    avio_flush(s->pb);

    /* replace "Xing" identification string with "Info" for CBR files. */
    if (!mp3->has_variable_bitrate) {
        int64_t tag_offset = mp3->frames_offset
            - 4   // frames/size/toc flags
            - 4;  // xing tag
        avio_seek(s->pb, tag_offset, SEEK_SET);
        avio_wb32(s->pb, MKBETAG('I', 'n', 'f', 'o'));
    }

    avio_seek(s->pb, mp3->frames_offset, SEEK_SET);
    avio_wb32(s->pb, mp3->frames);
    avio_wb32(s->pb, mp3->size);

    avio_w8(s->pb, 0);  // first toc entry has to be zero.

    for (i = 1; i < VBR_TOC_SIZE; ++i) {
        int j = i * mp3->pos / VBR_TOC_SIZE;
        int seek_point = 256LL * mp3->bag[j] / mp3->size;
        avio_w8(s->pb, FFMIN(seek_point, 255));
    }

    avio_flush(s->pb);
    avio_seek(s->pb, 0, SEEK_END);
}

/**
 * Write an ID3v2 header at beginning of stream
 */

static int mp3_write_header(struct AVFormatContext *s)
{
    MP3Context  *mp3 = s->priv_data;
    AVDictionaryEntry *t;
    int totlen = 0;
    int64_t size_pos, cur_pos;

    if (s->nb_streams != 1) {
        av_log(s, AV_LOG_ERROR, "mp3 only supports one audio stream\n");
        return -1;
    }

    if (s->streams[0]->codec->codec_id != CODEC_ID_MP3) {
        av_log(s, AV_LOG_ERROR, "error, codec is not mp3\n");
        return -1;
    }

    avio_wb32(s->pb, MKBETAG('I', 'D', '3', mp3->id3v2_version));
    avio_w8(s->pb, 0);
    avio_w8(s->pb, 0); /* flags */

    /* reserve space for size */
    size_pos = avio_tell(s->pb);
    avio_wb32(s->pb, 0);

    // translate year / date tag
    if ((t = av_dict_get(s->metadata, "date", NULL, 0)) && mp3->id3v2_version == 3 &&
        !av_dict_get(s->metadata, "year", NULL, 0) && strlen(t->value) >= 4) {
        t->value[5] = 0;
        strcpy(t->key, "year");
    }

    // translate year / date tag
    if ((t = av_dict_get(s->metadata, "year", NULL, 0)) && mp3->id3v2_version == 4 &&
        !av_dict_get(s->metadata, "date", NULL, 0)) {
        strcpy(t->key, "date");
    }

    for (t = NULL; t = av_dict_get(s->metadata, "", t, AV_DICT_IGNORE_SUFFIX);)
        totlen += id3v2_write_tag(s, t);

    cur_pos = avio_tell(s->pb);
    avio_seek(s->pb, size_pos, SEEK_SET);
    id3v2_put_size(s, totlen);
    avio_seek(s->pb, cur_pos, SEEK_SET);

    if (s->pb->seekable)
        mp3_write_xing(s);

    return 0;
}

static int mp3_write_packet(AVFormatContext *s, AVPacket *pkt)
{
    if (! pkt || ! pkt->data || pkt->size < 4)
        return ff_raw_write_packet(s, pkt);
    else {
        MP3Context  *mp3 = s->priv_data;
        MPADecodeHeader c;
#ifdef FILTER_VBR_HEADERS
        int base;
#endif

        ff_mpegaudio_decode_header(&c, AV_RB32(pkt->data));

        if (!mp3->initial_bitrate)
            mp3->initial_bitrate = c.bit_rate;
        if (!mp3->has_variable_bitrate) {
            if ((c.bit_rate == 0) || (mp3->initial_bitrate != c.bit_rate))
                mp3->has_variable_bitrate = 1;
        }

#ifdef FILTER_VBR_HEADERS
        /* filter out XING and INFO headers. */
        base = 4 + xing_offtbl[c.lsf == 1][c.nb_channels == 1];

        if (base + 4 <= pkt->size) {
            uint32_t v = AV_RB32(pkt->data + base);

            if (MKBETAG('X','i','n','g') == v || MKBETAG('I','n','f','o') == v)
                return 0;
        }

        /* filter out VBRI headers. */
        base = 4 + 32;

        if (base + 4 <= pkt->size && MKBETAG('V','B','R','I') == AV_RB32(pkt->data + base))
            return 0;
#endif

        if (mp3->frames_offset)
            mp3_xing_add_frame(s, pkt);

        return ff_raw_write_packet(s, pkt);
    }
}

static int mp3_write_trailer(AVFormatContext *s)
{
    MP3Context  *mp3 = s->priv_data;

    if (mp3->frames_offset)
        mp3_fix_xing(s);

    return 0;
}

static const AVMetadataConv ff_mp3_metadata_conv[] = {
    { "APIC", "cover" },
    { "TALB", "album" },
    { "TCOM", "composer" },
    { "TCON", "genre" },
    { "TCOP", "copyright" },
    { "TENC", "encoded_by" },
    { "TIT2", "title" },
    { "TLAN", "language" },
    { "TPE1", "artist" },
    { "TPE2", "album_artist" },
    { "TPE3", "performer" },
    { "TPOS", "disc" },
    { "TPUB", "publisher" },
    { "TRCK", "track" },
    { "TSSE", "encoder" },
    { "TYER", "year" },
    { "USLT", "lyrics" },
    { "TDRC", "date" },
    { "TDRL", "release_date" },
    { "TDEN", "creation_time" },
    { "TSOA", "album-sort" },
    { "TSOP", "artist-sort" },
    { "TSOT", "title-sort" },
    { 0 },
};

AVOutputFormat ff_mp3_muxer = {
    .name              = "mp3",
    .long_name         = NULL_IF_CONFIG_SMALL("MPEG audio layer 3"),
    .mime_type         = "audio/x-mpeg",
    .extensions        = "mp3",
    .priv_data_size    = sizeof(MP3Context),
    .audio_codec       = CODEC_ID_MP3,
    .video_codec       = CODEC_ID_NONE,
    .write_header      = mp3_write_header,
    .write_packet      = mp3_write_packet,
    .write_trailer     = mp3_write_trailer,
    .flags             = AVFMT_NOTIMESTAMPS,
    .metadata_conv     = ff_mp3_metadata_conv,
    .priv_class = &mp3_muxer_class,
};
#endif
