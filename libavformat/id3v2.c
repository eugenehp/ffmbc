/*
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

/**
 * @file
 * ID3v2 header parser
 *
 * Specifications available at:
 * http://id3.org/Developer_Information
 */

#include "id3v2.h"
#include "id3v1.h"
#include "libavutil/avstring.h"
#include "libavutil/intreadwrite.h"
#include "libavutil/dict.h"
#include "avio_internal.h"

int ff_id3v2_match(const uint8_t *buf, const char * magic)
{
    return  buf[0]         == magic[0] &&
            buf[1]         == magic[1] &&
            buf[2]         == magic[2] &&
            buf[3]         != 0xff &&
            buf[4]         != 0xff &&
           (buf[6] & 0x80) ==    0 &&
           (buf[7] & 0x80) ==    0 &&
           (buf[8] & 0x80) ==    0 &&
           (buf[9] & 0x80) ==    0;
}

int ff_id3v2_tag_len(const uint8_t * buf)
{
    int len = ((buf[6] & 0x7f) << 21) +
              ((buf[7] & 0x7f) << 14) +
              ((buf[8] & 0x7f) << 7) +
               (buf[9] & 0x7f) +
              ID3v2_HEADER_SIZE;
    if (buf[5] & 0x10)
        len += ID3v2_HEADER_SIZE;
    return len;
}

static unsigned int get_size(AVIOContext *s, int len)
{
    int v = 0;
    while (len--)
        v = (v << 7) + (avio_r8(s) & 0x7F);
    return v;
}

static int read_id3v2_string(AVFormatContext *s, AVIOContext *pb,
                             const char *key, int taglen,
                             int encoding, char *dst, int dstlen)
{
    unsigned int (*get)(AVIOContext*) = avio_rb16;
    char *q = dst;
    uint8_t tmp;
    uint32_t ch;
    int bom;

    switch (encoding) {
    case ID3v2_ENCODING_ISO8859:
        while (taglen) {
            uint32_t val = avio_r8(pb); taglen--;
            if (!val)
                break;
            PUT_UTF8(val, tmp, if (q - dst < dstlen) *q++ = tmp;);
        }
        *q = 0;
        break;
    case ID3v2_ENCODING_UTF16BOM:
        bom = avio_rb16(pb); taglen -= 2;
        switch (bom) {
        case 0xfffe:
            get = avio_rl16;
        case 0xfeff:
            break;
        default:
            av_log(s, AV_LOG_ERROR, "Incorrect BOM value: %x in tag %s\n", bom, key);
            return taglen;
        }
        // fall-through
    case ID3v2_ENCODING_UTF16BE:
        while (taglen > 1) {
            uint16_t val = get(pb); taglen -= 2;
            if (!val)
                break;
            GET_UTF16(ch, val, break;);
            PUT_UTF8(ch, tmp, if (q - dst < dstlen) *q++ = tmp;);
        }
        *q = 0;
        break;
    case ID3v2_ENCODING_UTF8:
        while (taglen) {
            uint32_t val = avio_r8(pb); taglen--;
            if (!val)
                break;
            GET_UTF8(ch, val, break;);
            PUT_UTF8(ch, tmp, if (q - dst < dstlen) *q++ = tmp;);
        }
        *q = 0;
        break;
    default:
        av_log(s, AV_LOG_WARNING, "Unknown encoding in tag %s\n", key);
    }
    return taglen;
}

static void read_ttag(AVFormatContext *s, AVIOContext *pb, int taglen, const char *key)
{
    char dst[512];
    const char *val = NULL;
    int len, dstlen = sizeof(dst) - 1;
    unsigned genre, encoding;

    if (taglen < 1)
        return;

    taglen--; /* account for encoding type byte */
    encoding = avio_r8(pb);
    taglen = read_id3v2_string(s, pb, key, taglen, encoding, dst, dstlen);

    if (!(strcmp(key, "TCON") && strcmp(key, "TCO"))
        && (sscanf(dst, "(%d)", &genre) == 1 || sscanf(dst, "%d", &genre) == 1)
        && genre <= ID3v1_GENRE_MAX)
        val = ff_id3v1_genre_str[genre];
    else if (!(strcmp(key, "TXXX") && strcmp(key, "TXX"))) {
        /* dst now contains two 0-terminated strings */
        len = strlen(dst);
        read_id3v2_string(s, pb, key, taglen, encoding, dst+len, dstlen-len);
        val = dst+len;
    } else if (!strcmp(key, "TDAT")) {
        /* date in the form DDMM, change to DD/MM */
        dst[5] = 0;
        dst[4] = dst[3];
        dst[3] = dst[2];
        dst[2] = '/';
        val = dst;
    } else if (*dst)
        val = dst;

    if (val) {
        int i;
        const char *name = key;
        for (i = 0; ff_id3v2_tags[i].tag; i++) {
            if (!strcmp(ff_id3v2_tags[i].tag, key)) {
                name = ff_id3v2_tags[i].name;
                break;
            }
        }
        av_dict_set(&s->metadata, name, val, 0);
    }
}

static int read_uslt(AVFormatContext *s, int taglen, const char *key)
{
    AVDictionaryEntry *tag;
    uint8_t *data;
    char lang[4];
    int encoding;

    encoding = avio_r8(s->pb); // encoding
    avio_read(s->pb, lang, 3);
    taglen -= 4;
    taglen = read_id3v2_string(s, s->pb, key, taglen, encoding, lang+3, 0); // description
    data = av_malloc(taglen);
    if (!data)
        return AVERROR(ENOMEM);
    read_id3v2_string(s, s->pb, key, taglen, encoding, data, taglen-1);

    if (av_dict_set_custom(&s->metadata, &tag, METADATA_STRING, "lyrics", data,
                           strlen(data), AV_DICT_DONT_STRDUP_VAL) < 0)
        return -1;
    av_metadata_set_attribute(tag, "language", lang);

    return 0;
}

static int read_apic(AVFormatContext *s, int taglen, const char *key)
{
    AVDictionaryEntry *tag;
    char mime[64];
    int64_t pos = avio_tell(s->pb);
    int len;
    uint8_t *data;

    mime[0] = 0;
    avio_r8(s->pb); // encoding
    if (!strcmp(key, "PIC")) {
        char type[4] = {0};
        avio_read(s->pb, type, 3);
        if (!strcmp(type, "PNG"))
            strcpy(mime, "image/png");
        else if (!strcmp(type, "JPG"))
            strcpy(mime, "image/jpeg");
        else if (!strcmp(type, "BMP"))
            strcpy(mime, "image/bmp");
    } else {
        avio_get_str(s->pb, sizeof(mime), mime, sizeof(mime));
    }
    avio_r8(s->pb); // type
    while (avio_r8(s->pb)); // description

    len = taglen - (avio_tell(s->pb) - pos);
    if (len <= 0)
        return -1;
    data = av_malloc(len);
    if (!data)
        return AVERROR(ENOMEM);
    avio_read(s->pb, data, len);

    if (av_dict_set_custom(&s->metadata, &tag, METADATA_BYTEARRAY, "cover",
                           data, len, AV_DICT_DONT_STRDUP_VAL) < 0)
        return -1;
    av_metadata_set_attribute(tag, "mime", mime);

    return 0;
}

static void ff_id3v2_parse(AVFormatContext *s, int len, uint8_t version, uint8_t flags)
{
    int isv34, unsync;
    unsigned tlen;
    char tag[5];
    int64_t next, end = avio_tell(s->pb) + len;
    int taghdrlen;
    const char *reason = NULL;
    AVIOContext pb;
    unsigned char *buffer = NULL;
    int buffer_size = 0;

    switch (version) {
    case 2:
        if (flags & 0x40) {
            reason = "compression";
            goto error;
        }
        isv34 = 0;
        taghdrlen = 6;
        break;

    case 3:
    case 4:
        isv34 = 1;
        taghdrlen = 10;
        break;

    default:
        reason = "version";
        goto error;
    }

    snprintf(tag, 5, "2.%d", version);
    av_dict_set(&s->metadata, "id3_version", tag, 0);

    unsync = flags & 0x80;

    if (isv34 && flags & 0x40) /* Extended header present, just skip over it */
        avio_skip(s->pb, get_size(s->pb, 4));

    while (len >= taghdrlen) {
        unsigned int tflags = 0;
        int tunsync = 0;

        if (isv34) {
            avio_read(s->pb, tag, 4);
            tag[4] = 0;
            if(version==3){
                tlen = avio_rb32(s->pb);
            }else
                tlen = get_size(s->pb, 4);
            tflags = avio_rb16(s->pb);
            tunsync = tflags & ID3v2_FLAG_UNSYNCH;
        } else {
            avio_read(s->pb, tag, 3);
            tag[3] = 0;
            tlen = avio_rb24(s->pb);
        }
        if (tlen > (1<<28) || !tlen)
            break;
        len -= taghdrlen + tlen;

        if (len < 0)
            break;

        next = avio_tell(s->pb) + tlen;

        if (tflags & ID3v2_FLAG_DATALEN) {
            if (tlen < 4)
                break;
            avio_rb32(s->pb);
            tlen -= 4;
        }

        av_log(s, AV_LOG_DEBUG, "tag %s len %d\n", tag, tlen);

        if (tflags & (ID3v2_FLAG_ENCRYPTION | ID3v2_FLAG_COMPRESSION)) {
            av_log(s, AV_LOG_WARNING, "Skipping encrypted/compressed ID3v2 frame %s.\n", tag);
            avio_skip(s->pb, tlen);
        } else if (tag[0] == 'T') {
            if (unsync || tunsync) {
                int i, j;
                av_fast_malloc(&buffer, &buffer_size, tlen);
                if (!buffer) {
                    av_log(s, AV_LOG_ERROR, "Failed to alloc %d bytes\n", tlen);
                    goto seek;
                }
                for (i = 0, j = 0; i < tlen; i++, j++) {
                    buffer[j] = avio_r8(s->pb);
                    if (j > 0 && !buffer[j] && buffer[j - 1] == 0xff) {
                        /* Unsynchronised byte, skip it */
                        j--;
                    }
                }
                ffio_init_context(&pb, buffer, j, 0, NULL, NULL, NULL, NULL);
                read_ttag(s, &pb, j, tag);
            } else {
                read_ttag(s, s->pb, tlen, tag);
            }
        } else if (!strcmp(tag, "APIC") || !strcmp(tag, "PIC")) {
            read_apic(s, tlen, tag);
        } else if (!strcmp(tag, "USLT") || !strcmp(tag, "ULT")) {
            read_uslt(s, tlen, tag);
        } else if (!tag[0]) {
            if (tag[1])
                av_log(s, AV_LOG_WARNING, "invalid frame id, assuming padding");
            avio_skip(s->pb, tlen);
            break;
        }
        /* Skip to end of tag */
seek:
        avio_seek(s->pb, next, SEEK_SET);
    }

    if (version == 4 && flags & 0x10) /* Footer preset, always 10 bytes, skip over it */
        end += 10;

  error:
    if (reason)
        av_log(s, AV_LOG_INFO, "ID3v2.%d tag skipped, cannot handle %s\n", version, reason);
    avio_seek(s->pb, end, SEEK_SET);
    av_free(buffer);
    return;
}

void ff_id3v2_read(AVFormatContext *s, const char *magic)
{
    int len, ret;
    uint8_t buf[ID3v2_HEADER_SIZE];
    int     found_header;
    int64_t off;

    do {
        /* save the current offset in case there's nothing to read/skip */
        off = avio_tell(s->pb);
        ret = avio_read(s->pb, buf, ID3v2_HEADER_SIZE);
        if (ret != ID3v2_HEADER_SIZE)
            break;
            found_header = ff_id3v2_match(buf, magic);
            if (found_header) {
            /* parse ID3v2 header */
            len = ((buf[6] & 0x7f) << 21) |
                  ((buf[7] & 0x7f) << 14) |
                  ((buf[8] & 0x7f) << 7) |
                   (buf[9] & 0x7f);
            ff_id3v2_parse(s, len, buf[3], buf[5]);
        } else {
            avio_seek(s->pb, off, SEEK_SET);
        }
    } while (found_header);
}

/*
 * 0 - v2.2
 * 1 - v2.3
 * 2 - v2.4
*/
const ID3v2Tag ff_id3v2_tags[] = {
    { "APIC", "cover", 3},
    { "TALB", "album", 3},
    { "TCOM", "composer", 3},
    { "TCON", "genre", 3},
    { "TCOP", "copyright", 3},
    { "TENC", "encoded_by", 3},
    { "TIT2", "title", 3},
    { "TLAN", "language", 3},
    { "TPE1", "artist", 3},
    { "TPE2", "album_artist", 3},
    { "TPE3", "performer", 3},
    { "TPOS", "disc", 3},
    { "TPUB", "publisher", 3},
    { "TRCK", "track", 3},
    { "TSSE", "encoder", 3},
    { "TYER", "year", 1},
    { "USLT", "lyrics", 3},
    { "TDRC", "date", 2},
    { "TDRL", "release_date", 2},
    { "TDEN", "creation_time", 2},
    { "TSOA", "album-sort", 2},
    { "TSOP", "artist-sort", 2},
    { "TSOT", "title-sort", 2},
    { "TAL",  "album", 0},
    { "TCM",  "composer", 0},
    { "TCO",  "genre", 0},
    { "TT2",  "title", 0},
    { "TEN",  "encoded_by", 0},
    { "TP1",  "artist", 0},
    { "TP2",  "album_artist", 0},
    { "TP3",  "performer", 0},
    { "TPA",  "disc", 0},
    { "TRK",  "track", 0},
    { "ULT",  "lyrics", 0},
    { "TYE",  "year", 0},
    { 0 }
};
