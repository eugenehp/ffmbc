/*
 * MXF demuxer.
 * Copyright (c) 2006 SmartJog S.A., Baptiste Coudurier <baptiste dot coudurier at smartjog dot com>
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

/*
 * References
 * SMPTE 336M KLV Data Encoding Protocol Using Key-Length-Value
 * SMPTE 377M MXF File Format Specifications
 * SMPTE 378M Operational Pattern 1a
 * SMPTE 379M MXF Generic Container
 * SMPTE 381M Mapping MPEG Streams into the MXF Generic Container
 * SMPTE 382M Mapping AES3 and Broadcast Wave Audio into the MXF Generic Container
 * SMPTE 383M Mapping DV-DIF Data to the MXF Generic Container
 *
 * Principle
 * Search for Track numbers which will identify essence element KLV packets.
 * Search for SourcePackage which define tracks which contains Track numbers.
 * Material Package contains tracks with reference to SourcePackage tracks.
 * Search for Descriptors (Picture, Sound) which contains codec info and parameters.
 * Assign Descriptors to correct Tracks.
 *
 * Metadata reading functions read Local Tags, get InstanceUID(0x3C0A) then add MetaDataSet to MXFContext.
 * Metadata parsing resolves Strong References to objects.
 *
 * Simple demuxer, only OP1A supported and some files might not work at all.
 * Only tracks with associated descriptors will be decoded. "Highly Desirable" SMPTE 377M D.1
 */

//#define DEBUG

#include "libavutil/aes.h"
#include "libavutil/mathematics.h"
#include "libavcodec/bytestream.h"
#include "libavcodec/timecode.h"
#include "avformat.h"
#include "mxf.h"

typedef struct {
    UID uid;
    enum MXFMetadataSetType type;
    UID source_container_ul;
} MXFCryptoContext;

typedef struct {
    UID uid;
    enum MXFMetadataSetType type;
    UID source_package_uid;
    UID data_definition_ul;
    int64_t duration;
    int64_t start_position;
    int source_track_id;
} MXFStructuralComponent;

typedef struct {
    UID uid;
    enum MXFMetadataSetType type;
    int64_t start;
    int base;
    int drop_frame;
} MXFTimecodeComponent;

typedef struct {
    UID uid;
    enum MXFMetadataSetType type;
    UID data_definition_ul;
    UID *structural_components_refs;
    int structural_components_count;
    int64_t duration;
} MXFSequence;

typedef struct {
    UID uid;
    enum MXFMetadataSetType type;
    UID essence_container_ul;
    UID essence_codec_ul;
    AVRational sample_rate;
    AVRational aspect_ratio;
    int width;
    int height;
    int frame_layout;
    int channels;
    int bits_per_sample;
    unsigned int component_depth;
    unsigned int horiz_subsampling;
    unsigned int vert_subsampling;
    UID *sub_descriptors_refs;
    int sub_descriptors_count;
    int linked_track_id;
    uint8_t *extradata;
    int extradata_size;
    enum PixelFormat pix_fmt;
    unsigned padding_size;   ///< avid specific
    unsigned edit_unit_size; ///< avid specific
} MXFDescriptor;

typedef struct {
    UID uid;
    enum MXFMetadataSetType type;
    MXFSequence *sequence; /* mandatory, and only one */
    UID sequence_ref;
    int track_id;
    uint8_t track_number[4];
    AVRational edit_rate;
    KLVPacket klv;                ///< used for clip wrapping
    unsigned edit_unit_bytecount; ///< used for clip wrapping
    MXFDescriptor *descriptor;
} MXFTrack;

typedef struct {
    UID uid;
    enum MXFMetadataSetType type;
    unsigned edit_unit_bytecount;
    unsigned body_sid;
    unsigned index_sid;
    uint64_t start;
    uint64_t duration;
} MXFIndexTableSegment;

typedef struct {
    UID uid;
    enum MXFMetadataSetType type;
    UID package_uid;
    UID *tracks_refs;
    int tracks_count;
    MXFDescriptor *descriptor; /* only one */
    UID descriptor_ref;
} MXFPackage;

typedef struct {
    UID uid;
    enum MXFMetadataSetType type;
} MXFMetadataSet;

typedef enum {
    Op1a,
    Op1b,
    Op1c,
    Op2a,
    Op2b,
    Op2c,
    Op3a,
    Op3b,
    Op3c,
    OpAtom,
} MXFOpValue;

static const struct {
    MXFOpValue val;
    const char *str;
} mxf_operational_patterns[] = {
    { Op1a, "Op1a" },
    { Op1b, "Op1b" },
    { Op1c, "Op1c" },
    { Op2a, "Op2a" },
    { Op2b, "Op2b" },
    { Op2c, "Op2c" },
    { Op3a, "Op3a" },
    { Op3b, "Op3b" },
    { Op3c, "Op3c" },
    { OpAtom, "OpAtom" },
};

typedef struct {
    UID *packages_refs;
    int packages_count;
    MXFMetadataSet **metadata_sets;
    int metadata_sets_count;
    AVFormatContext *fc;
    struct AVAES *aesc;
    uint8_t *local_tags;
    int local_tags_count;
    uint64_t footer_partition; ///< offset of footer partition
    MXFOpValue op; ///< operational pattern
} MXFContext;

enum MXFWrappingScheme {
    Frame,
    Clip,
};

typedef int MXFMetadataReadFunc(AVFormatContext *s, void *arg, int tag, int size, UID uid);

typedef struct {
    const UID key;
    MXFMetadataReadFunc *read;
    int ctx_size;
    enum MXFMetadataSetType type;
} MXFMetadataReadTableEntry;

/* partial keys to match */
static const uint8_t mxf_header_partition_pack_key[]       = { 0x06,0x0e,0x2b,0x34,0x02,0x05,0x01,0x01,0x0d,0x01,0x02,0x01,0x01,0x02 };
static const uint8_t mxf_footer_partition_key[]            = { 0x06,0x0e,0x2b,0x34,0x02,0x05,0x01,0x01,0x0d,0x01,0x02,0x01,0x01,0x04 };
static const uint8_t mxf_essence_element_key[]             = { 0x06,0x0e,0x2b,0x34,0x01,0x02,0x01,0x01,0x0d,0x01,0x03,0x01 };
static const uint8_t mxf_system_metadata_pack_key[]        = { 0x06,0x0e,0x2b,0x34,0x02,0x05,0x01,0x01,0x0d,0x01,0x03,0x01,0x04,0x01,0x01,0x00 };
static const uint8_t mxf_avid_essence_element_key[]        = { 0x06,0x0e,0x2b,0x34,0x01,0x02,0x01,0x01,0x0e,0x04,0x03,0x01 }; //0x15,0x01,0x06,0x01 };
static const uint8_t mxf_klv_key[]                         = { 0x06,0x0e,0x2b,0x34 };
/* complete keys to match */
static const uint8_t mxf_random_index_pack_key[]           = { 0x06,0x0e,0x2b,0x34,0x02,0x05,0x01,0x01,0x0d,0x01,0x02,0x01,0x01,0x11,0x01,0x00 };
static const uint8_t mxf_crypto_source_container_ul[]      = { 0x06,0x0e,0x2b,0x34,0x01,0x01,0x01,0x09,0x06,0x01,0x01,0x02,0x02,0x00,0x00,0x00 };
static const uint8_t mxf_encrypted_triplet_key[]           = { 0x06,0x0e,0x2b,0x34,0x02,0x04,0x01,0x07,0x0d,0x01,0x03,0x01,0x02,0x7e,0x01,0x00 };
static const uint8_t mxf_encrypted_essence_container[]     = { 0x06,0x0e,0x2b,0x34,0x04,0x01,0x01,0x07,0x0d,0x01,0x03,0x01,0x02,0x0b,0x01,0x00 };
static const uint8_t mxf_sony_mpeg4_extradata[]            = { 0x06,0x0e,0x2b,0x34,0x04,0x01,0x01,0x01,0x0e,0x06,0x06,0x02,0x02,0x01,0x00,0x00 };
static const uint8_t mxf_avid_padding_size_uid[]           = { 0xa0,0x24,0x00,0x60,0x94,0xeb,0x75,0xcb,0xce,0x2a,0xca,0x4e,0x51,0xab,0x11,0xd3 };
static const uint8_t mxf_avid_edit_unit_size_uid[]         = { 0xa0,0x24,0x00,0x60,0x94,0xeb,0x75,0xcb,0xce,0x2a,0xca,0x50,0x51,0xab,0x11,0xd3 };

#define IS_KLV_KEY(x, y) (!memcmp(x, y, sizeof(y)))

static int64_t klv_decode_ber_length(AVIOContext *pb)
{
    uint64_t size = avio_r8(pb);
    if (size & 0x80) { /* long form */
        int bytes_num = size & 0x7f;
        /* SMPTE 379M 5.3.4 guarantee that bytes_num must not exceed 8 bytes */
        if (bytes_num > 8)
            return -1;
        size = 0;
        while (bytes_num--)
            size = size << 8 | avio_r8(pb);
    }
    return size;
}

static int mxf_read_sync(AVIOContext *pb, const uint8_t *key, unsigned size)
{
    int i, b;
    for (i = 0; i < size && !url_feof(pb); i++) {
        b = avio_r8(pb);
        if (b == key[0])
            i = 0;
        else if (b != key[i])
            i = -1;
    }
    return i == size;
}

static int klv_read_packet(KLVPacket *klv, AVIOContext *pb)
{
    if (!mxf_read_sync(pb, mxf_klv_key, 4))
        return -1;
    klv->offset = avio_tell(pb) - 4;
    memcpy(klv->key, mxf_klv_key, 4);
    avio_read(pb, klv->key + 4, 12);
    klv->length = klv_decode_ber_length(pb);
    return klv->length == -1 ? -1 : 0;
}

static int mxf_get_stream_index(AVFormatContext *s, KLVPacket *klv)
{
    int i;

    for (i = 0; i < s->nb_streams; i++) {
        MXFTrack *track = s->streams[i]->priv_data;
        /* SMPTE 379M 7.3 */
        if (!memcmp(klv->key + sizeof(mxf_essence_element_key),
                    track->track_number, sizeof(track->track_number)) ||
            !memcmp(klv->key + sizeof(mxf_avid_essence_element_key),
                    track->track_number, sizeof(track->track_number)))
            return i;
    }
    /* return 0 if only one stream, for OP Atom files with 0 as track number */
    return s->nb_streams == 1 ? 0 : -1;
}

/* XXX: use AVBitStreamFilter */
static int mxf_get_d10_aes3_packet(AVIOContext *pb, AVStream *st, AVPacket *pkt, int64_t length)
{
    const uint8_t *buf_ptr, *end_ptr;
    uint8_t *data_ptr;
    int i;

    if (length > 61444) /* worst case PAL 1920 samples 8 channels */
        return -1;
    av_new_packet(pkt, length);
    avio_read(pb, pkt->data, length);
    data_ptr = pkt->data;
    end_ptr = pkt->data + length;
    buf_ptr = pkt->data + 4; /* skip SMPTE 331M header */
    for (; buf_ptr < end_ptr; ) {
        for (i = 0; i < st->codec->channels; i++) {
            uint32_t sample = bytestream_get_le32(&buf_ptr);
            if (st->codec->bits_per_coded_sample == 24)
                bytestream_put_le24(&data_ptr, (sample >> 4) & 0xffffff);
            else
                bytestream_put_le16(&data_ptr, (sample >> 12) & 0xffff);
        }
        buf_ptr += 32 - st->codec->channels*4; // always 8 channels stored SMPTE 331M
    }
    pkt->size = data_ptr - pkt->data;
    return 0;
}

static int mxf_decrypt_triplet(AVFormatContext *s, AVPacket *pkt, KLVPacket *klv)
{
    static const uint8_t checkv[16] = {0x43, 0x48, 0x55, 0x4b, 0x43, 0x48, 0x55, 0x4b, 0x43, 0x48, 0x55, 0x4b, 0x43, 0x48, 0x55, 0x4b};
    MXFContext *mxf = s->priv_data;
    AVIOContext *pb = s->pb;
    int64_t end = avio_tell(pb) + klv->length;
    uint64_t size;
    uint64_t orig_size;
    uint64_t plaintext_size;
    uint8_t ivec[16];
    uint8_t tmpbuf[16];
    int index;

    if (!mxf->aesc && s->key && s->keylen == 16) {
        mxf->aesc = av_malloc(av_aes_size);
        if (!mxf->aesc)
            return -1;
        av_aes_init(mxf->aesc, s->key, 128, 1);
    }
    // crypto context
    avio_skip(pb, klv_decode_ber_length(pb));
    // plaintext offset
    klv_decode_ber_length(pb);
    plaintext_size = avio_rb64(pb);
    // source klv key
    klv_decode_ber_length(pb);
    avio_read(pb, klv->key, 16);
    if (!IS_KLV_KEY(klv, mxf_essence_element_key))
        return -1;
    index = mxf_get_stream_index(s, klv);
    if (index < 0)
        return -1;
    // source size
    klv_decode_ber_length(pb);
    orig_size = avio_rb64(pb);
    if (orig_size < plaintext_size)
        return -1;
    // enc. code
    size = klv_decode_ber_length(pb);
    if (size < 32 || size - 32 < orig_size)
        return -1;
    avio_read(pb, ivec, 16);
    avio_read(pb, tmpbuf, 16);
    if (mxf->aesc)
        av_aes_crypt(mxf->aesc, tmpbuf, tmpbuf, 1, ivec, 1);
    if (memcmp(tmpbuf, checkv, 16))
        av_log(s, AV_LOG_ERROR, "probably incorrect decryption key\n");
    size -= 32;
    av_get_packet(pb, pkt, size);
    size -= plaintext_size;
    if (mxf->aesc)
        av_aes_crypt(mxf->aesc, &pkt->data[plaintext_size],
                     &pkt->data[plaintext_size], size >> 4, ivec, 1);
    pkt->size = orig_size;
    pkt->stream_index = index;
    avio_skip(pb, end - avio_tell(pb));
    return 0;
}

static int mxf_read_opatom(AVFormatContext *s, AVPacket *pkt)
{
    MXFIndexTableSegment *index_segment = NULL;
    MXFContext *mxf = s->priv_data;
    MXFTrack *track;
    AVStream *st;
    int i, index, ret;
    unsigned size = 0;

    if (s->nb_streams > 1) {
        av_log(s, AV_LOG_ERROR, "more than one stream is not currently supported\n");
        return -1;
    }

    st = s->streams[0];
    track = st->priv_data;
    if (!track->klv.offset) {
        if (klv_read_packet(&track->klv, s->pb) < 0)
            return -1;
        PRINT_KEY(s, "read clip", track->klv.key);
        av_dlog(s, "size %"PRId64" offset %#"PRIx64"\n", track->klv.length, track->klv.offset);

        s->data_offset = avio_tell(s->pb);

        index = mxf_get_stream_index(s, &track->klv);
        if (index < 0) {
            av_log(s, AV_LOG_ERROR, "error getting stream index %d\n", AV_RB32(track->klv.key+12));
            return -1;
        }
        if (index != s->streams[0]->index) {
            av_log(s, AV_LOG_ERROR, "klv packet does not match stream index\n");
            return -1;
        }

        if (track->descriptor->edit_unit_size) {
            track->edit_unit_bytecount = track->descriptor->edit_unit_size;
        } else {
            for (i = 0; i < mxf->metadata_sets_count; i++) {
                if (mxf->metadata_sets[i]->type == IndexTableSegment) {
                    index_segment = (MXFIndexTableSegment*)mxf->metadata_sets[i];
                    // P2 files have the first frame bigger
                    if (index_segment->start == 0 && index_segment->duration == 1) {
                        size = index_segment->edit_unit_bytecount;
                        continue;
                    }
                    break;
                }
            }
            if (!index_segment) {
                av_log(s, AV_LOG_ERROR, "clip wrapping without index is not currently supported\n");
                return -1;
            }
            if (!index_segment->edit_unit_bytecount) {
                if (st->codec->codec_type == AVMEDIA_TYPE_AUDIO)
                    track->edit_unit_bytecount =
                        av_get_bits_per_sample(st->codec->codec_id)*1000 / 8;
            } else if (st->codec->codec_type == AVMEDIA_TYPE_AUDIO && st->time_base.num == 1) {
                track->edit_unit_bytecount = index_segment->edit_unit_bytecount*1000;
            } else {
                track->edit_unit_bytecount = index_segment->edit_unit_bytecount;
            }

            if (!track->edit_unit_bytecount) {
                av_log(s, AV_LOG_ERROR, "clip wrapping with variable "
                       "byte per unit is not currently supported\n");
                return -1;
            }
        }

        if (track->descriptor->padding_size) {
            av_log(s, AV_LOG_DEBUG, "padding size %d\n", track->descriptor->padding_size);
            avio_skip(s->pb, track->descriptor->padding_size);
        }
    }

    if (avio_tell(s->pb) + track->edit_unit_bytecount >
        s->data_offset + track->klv.length)
        return AVERROR_EOF;

    ret = av_get_packet(s->pb, pkt, size ? size : track->edit_unit_bytecount);
    if (ret < 0) {
        av_log(s, AV_LOG_ERROR, "error reading data\n");
        return ret;
    }

    return 0;
}

static void mxf_parse_12m_timecode(AVFormatContext *ctx, uint32_t buf)
{
    int drop = (buf >> 30) & 1;
    int f = ((buf >> 28) & 0x3) * 10 + ((buf >> 24) & 0xf);
    int s = ((buf >> 20) & 0x7) * 10 + ((buf >> 16) & 0xf);
    int m = ((buf >> 12) & 0x7) * 10 + ((buf >> 8) & 0xf);
    int h = ((buf >> 4) & 0x3) * 10 + (buf & 0xf);
    if (!av_dict_get(ctx->metadata, "system_timecode", NULL, 0)) {
        char timecode[32];
        snprintf(timecode, 32, "%02d:%02d:%02d%c%02d",
                 h, m, s, drop ? ';' : ':', f);
        av_dict_set(&ctx->metadata, "system_timecode", timecode, 0);
    }
    av_dlog(ctx, "%02d:%02d:%02d%c%02d\n", h, m, s, drop ? ';' : ':', f);
}

static void mxf_parse_system_metadata_pack(AVFormatContext *s, KLVPacket *klv)
{
    AVIOContext *pb = s->pb;
    int64_t vl, pos = avio_tell(pb);
    int v;

    v = avio_r8(pb);
    av_dlog(s, "present %#x\n", v);
    v = avio_r8(pb);
    av_dlog(s, "content package rate %#x\n", v);
    v = avio_r8(pb);
    av_dlog(s, "content package type %#x\n", v);
    v = avio_rb16(pb);
    av_dlog(s, "channel handle %#x\n", v);
    v = avio_rb16(pb);
    av_dlog(s, "continuity count %d\n", v);
    avio_skip(pb, 16); // container ul
    v = avio_r8(pb);
    av_dlog(s, "creation date %#x\n", v);
    vl = avio_rb64(pb);
    av_dlog(s, "creation date %#"PRIx64"\n", vl);
    vl = avio_rb64(pb);
    av_dlog(s, "creation date %#"PRIx64"\n", vl);
    v = avio_r8(pb);
    av_dlog(s, "type %#x\n", v);
    v = avio_rb32(pb);
    av_dlog(s, "timecode %#x\n", v);
    mxf_parse_12m_timecode(s, v);
    v = avio_rb32(pb);
    av_dlog(s, "binary group data %#x\n", v);
    vl = avio_rb64(pb);
    av_dlog(s, "binary group data %#"PRIx64"\n", vl);

    avio_skip(pb, klv->length - (avio_tell(pb) - pos));
}

static int mxf_read_packet(AVFormatContext *s, AVPacket *pkt)
{
    MXFContext *mxf = s->priv_data;
    KLVPacket klv;

    if (mxf->op == OpAtom)
        return mxf_read_opatom(s, pkt);

    while (!url_feof(s->pb)) {
        if (klv_read_packet(&klv, s->pb) < 0)
            return -1;
        if (klv.length > 50*1024*1024) {
            av_log(s, AV_LOG_ERROR, "klv packet too big\n");
            return AVERROR(EINVAL);
        }
        PRINT_KEY(s, "read packet", klv.key);
        av_dlog(s, "size %"PRIu64" offset %#"PRIx64"\n", klv.length, klv.offset);
#ifdef DEBUG
        if (IS_KLV_KEY(klv.key, mxf_system_metadata_pack_key)) {
            mxf_parse_system_metadata_pack(s, &klv);
            continue;
        }
#endif
        if (IS_KLV_KEY(klv.key, mxf_encrypted_triplet_key)) {
            int res = mxf_decrypt_triplet(s, pkt, &klv);
            if (res < 0) {
                av_log(s, AV_LOG_ERROR, "invalid encoded triplet\n");
                return -1;
            }
            return 0;
        }
        if (IS_KLV_KEY(klv.key, mxf_essence_element_key) ||
            IS_KLV_KEY(klv.key, mxf_avid_essence_element_key)) {
            int index = mxf_get_stream_index(s, &klv);
            if (index < 0) {
                av_log(s, AV_LOG_ERROR, "error getting stream index %d\n", AV_RB32(klv.key+12));
                goto skip;
            }
            if (s->streams[index]->discard == AVDISCARD_ALL)
                goto skip;
            /* check for 8 channels AES3 element */
            if (klv.key[12] == 0x06 && klv.key[13] == 0x01 && klv.key[14] == 0x10) {
                if (mxf_get_d10_aes3_packet(s->pb, s->streams[index], pkt, klv.length) < 0) {
                    av_log(s, AV_LOG_ERROR, "error reading D-10 aes3 frame\n");
                    return -1;
                }
            } else
                av_get_packet(s->pb, pkt, klv.length);
            pkt->stream_index = index;
            pkt->pos = klv.offset;
            return 0;
        } else
        skip:
            avio_skip(s->pb, klv.length);
    }
    return AVERROR_EOF;
}

static int mxf_read_primer_pack(AVFormatContext *s, void *arg, int tag, int size, UID uid)
{
    MXFContext *mxf = arg;
    int item_num = avio_rb32(s->pb);
    int item_len = avio_rb32(s->pb);

    if (item_len != 18) {
        av_log(s, AV_LOG_ERROR, "unsupported primer pack item length\n");
        return -1;
    }
    if (item_num > UINT_MAX / item_len)
        return -1;
    mxf->local_tags_count = item_num;
    mxf->local_tags = av_malloc(item_num*item_len);
    if (!mxf->local_tags)
        return -1;
    avio_read(s->pb, mxf->local_tags, item_num*item_len);
    return 0;
}

static int mxf_read_partition(AVFormatContext *s, void *arg, int tag, int size, UID uid)
{
    MXFContext *mxf = arg;
    unsigned count;
    UID op;

    avio_rb16(s->pb); // major version;
    avio_rb16(s->pb); // minor version;

    avio_rb32(s->pb); // kag size
    avio_rb64(s->pb); // offset of this partition
    avio_rb64(s->pb); // offset of previous partition
    mxf->footer_partition = avio_rb64(s->pb); // offset of footer partition

    avio_rb64(s->pb); // header byte count
    avio_rb64(s->pb); // index byte count

    avio_rb32(s->pb); // index sid

    avio_rb64(s->pb); // body offset

    avio_rb32(s->pb); // body sid

    avio_read(s->pb, op, 16);

    if      (op[12] == 1 && op[13] == 1) mxf->op = Op1a;
    else if (op[12] == 1 && op[13] == 2) mxf->op = Op1b;
    else if (op[12] == 1 && op[13] == 3) mxf->op = Op1c;
    else if (op[12] == 2 && op[13] == 1) mxf->op = Op2a;
    else if (op[12] == 2 && op[13] == 2) mxf->op = Op2b;
    else if (op[12] == 2 && op[13] == 3) mxf->op = Op2c;
    else if (op[12] == 3 && op[13] == 1) mxf->op = Op3a;
    else if (op[12] == 3 && op[13] == 2) mxf->op = Op3b;
    else if (op[12] == 3 && op[13] == 3) mxf->op = Op3c;
    else if (op[12] == 0x10)             mxf->op = OpAtom;
    else {
        av_log(mxf->fc, AV_LOG_ERROR, "unknown operational pattern: "
               "%02xh %02xh - assuming Op1a\n", op[12], op[13]);
        mxf->op = Op1a;
    }

    av_dict_set(&s->metadata, "operational_pattern", mxf_operational_patterns[mxf->op].str, 0);

    count = avio_rb32(s->pb);
    avio_skip(s->pb, 4); /* useless size of objects, always 16 according to specs */
    avio_skip(s->pb, count*16);

    return 0;
}

static int mxf_add_metadata_set(MXFContext *mxf, void *metadata_set)
{
    if (mxf->metadata_sets_count+1 >= UINT_MAX / sizeof(*mxf->metadata_sets))
        return AVERROR(ENOMEM);
    mxf->metadata_sets = av_realloc(mxf->metadata_sets, (mxf->metadata_sets_count + 1) * sizeof(*mxf->metadata_sets));
    if (!mxf->metadata_sets)
        return -1;
    mxf->metadata_sets[mxf->metadata_sets_count] = metadata_set;
    mxf->metadata_sets_count++;
    return 0;
}

static int mxf_read_cryptographic_context(AVFormatContext *s, void *arg, int tag, int size, UID uid)
{
    MXFCryptoContext *cryptocontext = arg;
    if (size != 16)
        return -1;
    if (IS_KLV_KEY(uid, mxf_crypto_source_container_ul))
        avio_read(s->pb, cryptocontext->source_container_ul, 16);
    return 0;
}

static int mxf_read_content_storage(AVFormatContext *s, void *arg, int tag, int size, UID uid)
{
    MXFContext *mxf = arg;
    switch (tag) {
    case 0x1901:
        mxf->packages_count = avio_rb32(s->pb);
        if (mxf->packages_count >= UINT_MAX / sizeof(UID))
            return -1;
        mxf->packages_refs = av_malloc(mxf->packages_count * sizeof(UID));
        if (!mxf->packages_refs)
            return -1;
        avio_skip(s->pb, 4); /* useless size of objects, always 16 according to specs */
        avio_read(s->pb, (uint8_t *)mxf->packages_refs, mxf->packages_count * sizeof(UID));
        break;
    }
    return 0;
}

static int mxf_read_source_clip(AVFormatContext *s, void *arg, int tag, int size, UID uid)
{
    MXFStructuralComponent *source_clip = arg;
    switch(tag) {
    case 0x0202:
        source_clip->duration = avio_rb64(s->pb);
        break;
    case 0x1201:
        source_clip->start_position = avio_rb64(s->pb);
        break;
    case 0x1101:
        /* UMID, only get last 16 bytes */
        avio_skip(s->pb, 16);
        avio_read(s->pb, source_clip->source_package_uid, 16);
        break;
    case 0x1102:
        source_clip->source_track_id = avio_rb32(s->pb);
        break;
    }
    return 0;
}

static int mxf_read_timecode_component(AVFormatContext *s, void *arg, int tag, int size, UID uid)
{
    MXFTimecodeComponent *timecode = arg;
    switch(tag) {
    case 0x1501: // Start Time Code
        timecode->start = avio_rb64(s->pb);
        break;
    case 0x1502: // Rounded Time Code Base
        timecode->base = avio_rb16(s->pb);
        break;
    case 0x1503: // Drop Frame
        timecode->drop_frame = avio_r8(s->pb);
        break;
    }
    return 0;
}

static int mxf_read_material_package(AVFormatContext *s, void *arg, int tag, int size, UID uid)
{
    MXFPackage *package = arg;
    switch(tag) {
    case 0x4403:
        package->tracks_count = avio_rb32(s->pb);
        if (package->tracks_count >= UINT_MAX / sizeof(UID))
            return -1;
        package->tracks_refs = av_malloc(package->tracks_count * sizeof(UID));
        if (!package->tracks_refs)
            return -1;
        avio_skip(s->pb, 4); /* useless size of objects, always 16 according to specs */
        avio_read(s->pb, (uint8_t *)package->tracks_refs, package->tracks_count * sizeof(UID));
        break;
    }
    return 0;
}

static int mxf_read_track(AVFormatContext *s, void *arg, int tag, int size, UID uid)
{
    MXFTrack *track = arg;
    switch(tag) {
    case 0x4801:
        track->track_id = avio_rb32(s->pb);
        break;
    case 0x4804:
        avio_read(s->pb, track->track_number, 4);
        break;
    case 0x4B01:
        track->edit_rate.den = avio_rb32(s->pb);
        track->edit_rate.num = avio_rb32(s->pb);
        break;
    case 0x4803:
        avio_read(s->pb, track->sequence_ref, 16);
        break;
    }
    return 0;
}

static int mxf_read_sequence(AVFormatContext *s, void *arg, int tag, int size, UID uid)
{
    MXFSequence *sequence = arg;
    switch(tag) {
    case 0x0202:
        sequence->duration = avio_rb64(s->pb);
        break;
    case 0x0201:
        avio_read(s->pb, sequence->data_definition_ul, 16);
        break;
    case 0x1001:
        sequence->structural_components_count = avio_rb32(s->pb);
        if (sequence->structural_components_count >= UINT_MAX / sizeof(UID))
            return -1;
        sequence->structural_components_refs = av_malloc(sequence->structural_components_count * sizeof(UID));
        if (!sequence->structural_components_refs)
            return -1;
        avio_skip(s->pb, 4); /* useless size of objects, always 16 according to specs */
        avio_read(s->pb, (uint8_t *)sequence->structural_components_refs, sequence->structural_components_count * sizeof(UID));
        break;
    }
    return 0;
}

static int mxf_read_source_package(AVFormatContext *s, void *arg, int tag, int size, UID uid)
{
    MXFPackage *package = arg;
    switch(tag) {
    case 0x4403:
        package->tracks_count = avio_rb32(s->pb);
        if (package->tracks_count >= UINT_MAX / sizeof(UID))
            return -1;
        package->tracks_refs = av_malloc(package->tracks_count * sizeof(UID));
        if (!package->tracks_refs)
            return -1;
        avio_skip(s->pb, 4); /* useless size of objects, always 16 according to specs */
        avio_read(s->pb, (uint8_t *)package->tracks_refs, package->tracks_count * sizeof(UID));
        break;
    case 0x4401:
        /* UMID, only get last 16 bytes */
        avio_skip(s->pb, 16);
        avio_read(s->pb, package->package_uid, 16);
        break;
    case 0x4701:
        avio_read(s->pb, package->descriptor_ref, 16);
        break;
    }
    return 0;
}

static int mxf_read_index_table_segment(AVFormatContext *s, void *arg, int tag, int size, UID uid)
{
    MXFIndexTableSegment *index_segment = arg;
    switch(tag) {
    case 0x3F05:
        index_segment->edit_unit_bytecount = avio_rb32(s->pb);
        av_dlog(s, "edit unit bytecount %d\n", index_segment->edit_unit_bytecount);
        break;
    case 0x3F06:
        index_segment->index_sid = avio_rb32(s->pb);
        av_dlog(s, "index sid %d\n", index_segment->index_sid);
        break;
    case 0x3F07:
        index_segment->body_sid = avio_rb32(s->pb);
        av_dlog(s, "body sid %d\n", index_segment->body_sid);
        break;
        //case 0x3F0B: av_dlog(s, "IndexEditRate %d/%d\n", avio_rb32(s->pb), avio_rb32(s->pb)); break;
    case 0x3F0C:
        index_segment->start = avio_rb64(s->pb);
        av_dlog(s, "start %"PRId64"\n", index_segment->start);
        break;
    case 0x3F0D:
        index_segment->duration = avio_rb64(s->pb);
        av_dlog(s, "duration %"PRId64"\n", index_segment->duration);
        break;
    }
    return 0;
}

static void mxf_read_pixel_layout(AVFormatContext *s, MXFDescriptor *descriptor)
{
    int code, value, ofs = 0;
    char layout[16] = {0};

    do {
        code = avio_r8(s->pb);
        value = avio_r8(s->pb);
        av_dlog(s, "pixel layout: code %#x\n", code);

        if (ofs < 16) {
            layout[ofs++] = code;
            layout[ofs++] = value;
        }
    } while (code != 0); /* SMPTE 377M E.2.46 */

    ff_mxf_decode_pixel_layout(layout, &descriptor->pix_fmt);
}

static int mxf_read_generic_descriptor(AVFormatContext *s, void *arg, int tag, int size, UID uid)
{
    MXFDescriptor *descriptor = arg;
    descriptor->pix_fmt = PIX_FMT_NONE;
    switch(tag) {
    case 0x3F01:
        descriptor->sub_descriptors_count = avio_rb32(s->pb);
        if (descriptor->sub_descriptors_count >= UINT_MAX / sizeof(UID))
            return -1;
        descriptor->sub_descriptors_refs = av_malloc(descriptor->sub_descriptors_count * sizeof(UID));
        if (!descriptor->sub_descriptors_refs)
            return -1;
        avio_skip(s->pb, 4); /* useless size of objects, always 16 according to specs */
        avio_read(s->pb, (uint8_t *)descriptor->sub_descriptors_refs, descriptor->sub_descriptors_count * sizeof(UID));
        break;
    case 0x3004:
        avio_read(s->pb, descriptor->essence_container_ul, 16);
        break;
    case 0x3006:
        descriptor->linked_track_id = avio_rb32(s->pb);
        break;
    case 0x3201: /* PictureEssenceCoding */
        avio_read(s->pb, descriptor->essence_codec_ul, 16);
        break;
    case 0x3203:
        descriptor->width = avio_rb32(s->pb);
        break;
    case 0x3202:
        descriptor->height = avio_rb32(s->pb);
        break;
    case 0x320C:
        descriptor->frame_layout = avio_r8(s->pb);
        break;
    case 0x320E:
        descriptor->aspect_ratio.num = avio_rb32(s->pb);
        descriptor->aspect_ratio.den = avio_rb32(s->pb);
        break;
    case 0x3301:
        descriptor->component_depth = avio_rb32(s->pb);
        break;
    case 0x3302:
        descriptor->horiz_subsampling = avio_rb32(s->pb);
        break;
    case 0x3308:
        descriptor->vert_subsampling = avio_rb32(s->pb);
        break;
    case 0x3D03:
        descriptor->sample_rate.num = avio_rb32(s->pb);
        descriptor->sample_rate.den = avio_rb32(s->pb);
        break;
    case 0x3D06: /* SoundEssenceCompression */
        avio_read(s->pb, descriptor->essence_codec_ul, 16);
        break;
    case 0x3D07:
        descriptor->channels = avio_rb32(s->pb);
        break;
    case 0x3D01:
        descriptor->bits_per_sample = avio_rb32(s->pb);
        break;
    case 0x3401:
        mxf_read_pixel_layout(s, descriptor);
        break;
    default:
        /* Private uid used by SONY C0023S01.mxf */
        if (IS_KLV_KEY(uid, mxf_sony_mpeg4_extradata)) {
            descriptor->extradata = av_malloc(size);
            if (!descriptor->extradata)
                return -1;
            descriptor->extradata_size = size;
            avio_read(s->pb, descriptor->extradata, size);
        } else if (IS_KLV_KEY(uid, mxf_avid_padding_size_uid)) {
            descriptor->padding_size = avio_rb32(s->pb);
        } else if (IS_KLV_KEY(uid, mxf_avid_edit_unit_size_uid)) {
            descriptor->edit_unit_size = avio_rb32(s->pb);
        } else if (tag > 0x7fff) {
            int64_t tmp = 0xffff;
            if (size == 4)
                tmp = avio_rb32(s->pb);
            else if (size == 8)
                tmp = avio_rb64(s->pb);
            av_dlog(s, "val %"PRId64"\n", tmp);
        }
        break;
    }
    return 0;
}

/*
 * Match an uid independently of the version byte and up to len common bytes
 * Returns: boolean
 */
static int mxf_match_uid(const UID key, const UID uid, int len)
{
    int i;
    for (i = 0; i < len; i++) {
        if (i != 7 && key[i] != uid[i])
            return 0;
    }
    return 1;
}

static const MXFCodecUL *mxf_get_codec_ul(const MXFCodecUL *uls, UID *uid)
{
    while (uls->uid[0]) {
        if(mxf_match_uid(uls->uid, *uid, uls->matching_len))
            break;
        uls++;
    }
    return uls;
}

static void *mxf_resolve_strong_ref(MXFContext *mxf, UID *strong_ref, enum MXFMetadataSetType type)
{
    int i;

    if (!strong_ref)
        return NULL;
    for (i = 0; i < mxf->metadata_sets_count; i++) {
        if (!memcmp(*strong_ref, mxf->metadata_sets[i]->uid, 16) &&
            (type == AnyType || mxf->metadata_sets[i]->type == type)) {
            return mxf->metadata_sets[i];
        }
    }
    return NULL;
}

static const MXFCodecUL mxf_essence_container_uls[] = {
    // video essence container uls
    { { 0x06,0x0E,0x2B,0x34,0x04,0x01,0x01,0x02,0x0D,0x01,0x03,0x01,0x02,0x04,0x60,0x01 }, 14, CODEC_ID_MPEG2VIDEO }, /* MPEG-ES Frame wrapped */
    { { 0x06,0x0E,0x2B,0x34,0x04,0x01,0x01,0x01,0x0D,0x01,0x03,0x01,0x02,0x02,0x41,0x01 }, 14,    CODEC_ID_DVVIDEO }, /* DV 625 25mbps */
    { { 0x06,0x0E,0x2B,0x34,0x04,0x01,0x01,0x01,0x0D,0x01,0x03,0x01,0x02,0x05,0x00,0x00 }, 14,   CODEC_ID_RAWVIDEO }, /* Uncompressed Picture */
    // sound essence container uls
    { { 0x06,0x0E,0x2B,0x34,0x04,0x01,0x01,0x01,0x0D,0x01,0x03,0x01,0x02,0x06,0x01,0x00 }, 14, CODEC_ID_PCM_S16LE }, /* BWF Frame wrapped */
    { { 0x06,0x0E,0x2B,0x34,0x04,0x01,0x01,0x02,0x0D,0x01,0x03,0x01,0x02,0x04,0x40,0x01 }, 14,       CODEC_ID_MP2 }, /* MPEG-ES Frame wrapped, 0x40 ??? stream id */
    { { 0x06,0x0E,0x2B,0x34,0x04,0x01,0x01,0x01,0x0D,0x01,0x03,0x01,0x02,0x01,0x01,0x01 }, 14, CODEC_ID_PCM_S16LE }, /* D-10 Mapping 50Mbps PAL Extended Template */
    { { 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00 },  0,      CODEC_ID_NONE },
};

static int mxf_parse_structural_metadata(MXFContext *mxf)
{
    MXFPackage *material_package = NULL;
    MXFPackage *temp_package = NULL;
    int i, j, k;

    av_dlog(mxf->fc, "metadata sets count %d\n", mxf->metadata_sets_count);
    /* TODO: handle multiple material packages (OP3x) */
    for (i = 0; i < mxf->packages_count; i++) {
        PRINT_KEY(mxf->fc, "package", mxf->packages_refs[i]);
        material_package = mxf_resolve_strong_ref(mxf, &mxf->packages_refs[i], MaterialPackage);
        if (material_package) break;
    }
    if (!material_package) {
        av_log(mxf->fc, AV_LOG_ERROR, "no material package found\n");
        return -1;
    }

    for (i = 0; i < material_package->tracks_count; i++) {
        MXFPackage *source_package = NULL;
        MXFTrack *material_track = NULL;
        MXFTrack *source_track = NULL;
        MXFTrack *temp_track = NULL;
        MXFDescriptor *descriptor = NULL;
        MXFStructuralComponent *component = NULL;
        UID *essence_container_ul = NULL;
        const MXFCodecUL *codec_ul = NULL;
        const MXFCodecUL *container_ul = NULL;
        AVStream *st;

        if (!(material_track = mxf_resolve_strong_ref(mxf, &material_package->tracks_refs[i], Track))) {
            av_log(mxf->fc, AV_LOG_ERROR, "could not resolve material track strong ref\n");
            continue;
        }

        if (!(material_track->sequence = mxf_resolve_strong_ref(mxf, &material_track->sequence_ref, Sequence))) {
            av_log(mxf->fc, AV_LOG_ERROR, "could not resolve material track sequence strong ref\n");
            continue;
        }

        /* TODO: handle multiple source clips */
        for (j = 0; j < material_track->sequence->structural_components_count; j++) {
            component = mxf_resolve_strong_ref(mxf, &material_track->sequence->structural_components_refs[j], AnyType);
            if (!component)
                continue;

            if (component->type == TimecodeComponent) {
                MXFTimecodeComponent *tc_component = (MXFTimecodeComponent*)component;
                char timecode[16];
                int ret = ff_framenum_to_timecode(timecode, tc_component->start,
                                                  tc_component->drop_frame, tc_component->base);
                if (ret < 0)
                    av_log(mxf->fc, AV_LOG_ERROR, "error parsing timecode\n");
                else
                    av_dict_set(&mxf->fc->metadata, "timecode", timecode, 0);
                continue;
            } else if (component->type != SourceClip)
                continue;

            for (k = 0; k < mxf->packages_count; k++) {
                temp_package = mxf_resolve_strong_ref(mxf, &mxf->packages_refs[k], SourcePackage);
                if (!temp_package)
                    continue;
                if (!memcmp(temp_package->package_uid, component->source_package_uid, 16)) {
                    source_package = temp_package;
                    break;
                }
            }
            if (!source_package) {
                av_log(mxf->fc, AV_LOG_ERROR, "material track %d: no corresponding source package found\n", material_track->track_id);
                break;
            }
            for (k = 0; k < source_package->tracks_count; k++) {
                if (!(temp_track = mxf_resolve_strong_ref(mxf, &source_package->tracks_refs[k], Track))) {
                    av_log(mxf->fc, AV_LOG_ERROR, "could not resolve source track strong ref\n");
                    return -1;
                }
                if (temp_track->track_id == component->source_track_id) {
                    source_track = temp_track;
                    break;
                }
            }
            if (!source_track) {
                av_log(mxf->fc, AV_LOG_ERROR, "material track %d: no corresponding source track found\n", material_track->track_id);
                break;
            }
        }
        if (!source_track)
            continue;

        st = av_new_stream(mxf->fc, source_track->track_id);
        if (!st) {
            av_log(mxf->fc, AV_LOG_ERROR, "could not allocate stream\n");
            return -1;
        }
        st->priv_data = source_track;
        st->duration = component->duration;
        if (st->duration == -1)
            st->duration = AV_NOPTS_VALUE;
        st->start_time = component->start_position;
        av_set_pts_info(st, 64, material_track->edit_rate.num, material_track->edit_rate.den);

        if (!(source_track->sequence = mxf_resolve_strong_ref(mxf, &source_track->sequence_ref, Sequence))) {
            av_log(mxf->fc, AV_LOG_ERROR, "could not resolve source track sequence strong ref\n");
            return -1;
        }

        PRINT_KEY(mxf->fc, "data definition   ul", source_track->sequence->data_definition_ul);
        codec_ul = mxf_get_codec_ul(ff_mxf_data_definition_uls, &source_track->sequence->data_definition_ul);
        st->codec->codec_type = codec_ul->id;

        source_package->descriptor = mxf_resolve_strong_ref(mxf, &source_package->descriptor_ref, AnyType);
        if (source_package->descriptor) {
            if (source_package->descriptor->type == MultipleDescriptor) {
                for (j = 0; j < source_package->descriptor->sub_descriptors_count; j++) {
                    MXFDescriptor *sub_descriptor = mxf_resolve_strong_ref(mxf, &source_package->descriptor->sub_descriptors_refs[j], Descriptor);

                    if (!sub_descriptor) {
                        av_log(mxf->fc, AV_LOG_ERROR, "could not resolve sub descriptor strong ref\n");
                        continue;
                    }
                    if (sub_descriptor->linked_track_id == source_track->track_id) {
                        descriptor = sub_descriptor;
                        break;
                    }
                }
            } else if (source_package->descriptor->type == Descriptor)
                descriptor = source_package->descriptor;
        }
        if (!descriptor) {
            av_log(mxf->fc, AV_LOG_INFO, "source track %d: stream %d, no descriptor found\n", source_track->track_id, st->index);
            continue;
        }
        PRINT_KEY(mxf->fc, "essence codec     ul", descriptor->essence_codec_ul);
        PRINT_KEY(mxf->fc, "essence container ul", descriptor->essence_container_ul);
        essence_container_ul = &descriptor->essence_container_ul;
        source_track->descriptor = descriptor;
        /* HACK: replacing the original key with mxf_encrypted_essence_container
         * is not allowed according to s429-6, try to find correct information anyway */
        if (IS_KLV_KEY(essence_container_ul, mxf_encrypted_essence_container)) {
            av_log(mxf->fc, AV_LOG_INFO, "broken encrypted mxf file\n");
            for (k = 0; k < mxf->metadata_sets_count; k++) {
                MXFMetadataSet *metadata = mxf->metadata_sets[k];
                if (metadata->type == CryptoContext) {
                    essence_container_ul = &((MXFCryptoContext *)metadata)->source_container_ul;
                    break;
                }
            }
        }
        if (st->codec->codec_type == AVMEDIA_TYPE_DATA)
            continue;
        /* TODO: drop PictureEssenceCoding and SoundEssenceCompression, only check EssenceContainer */
        codec_ul = mxf_get_codec_ul(ff_mxf_codec_uls, &descriptor->essence_codec_ul);
        st->codec->codec_id = codec_ul->id;
        if (descriptor->extradata) {
            st->codec->extradata = descriptor->extradata;
            st->codec->extradata_size = descriptor->extradata_size;
        }
        if (st->codec->codec_type == AVMEDIA_TYPE_VIDEO) {
            container_ul = mxf_get_codec_ul(mxf_essence_container_uls, essence_container_ul);
            if (st->codec->codec_id == CODEC_ID_NONE)
                st->codec->codec_id = container_ul->id;
            st->codec->width = descriptor->width;
            st->codec->height = descriptor->height; /* field height, not frame height */
            switch (descriptor->frame_layout) {
            case 0: // Full Frame
            case 3: // Mixed Fields
                break;
            case 1: // Separate Fields
                st->codec->height *= 2; /* turn field height into frame height */
                break;
            case 2: // Single Field
            case 4: // Segmented Frame
            default:
                av_log(mxf->fc, AV_LOG_INFO, "frame layout %d is not currently supported\n",
                       descriptor->frame_layout);
            }
            if (st->codec->codec_id == CODEC_ID_RAWVIDEO) {
                st->codec->pix_fmt = descriptor->pix_fmt;
                if (st->codec->pix_fmt == PIX_FMT_NONE) {
                    const MXFCodecUL *pix_fmt_ul =
                        mxf_get_codec_ul(ff_mxf_pixel_format_uls, &descriptor->essence_codec_ul);
                    st->codec->pix_fmt = pix_fmt_ul->id;
                    if (st->codec->pix_fmt == PIX_FMT_NONE) {
                        /* support files created before RP224v10 by defaulting to UYVY422
                           if subsampling is 4:2:2 and component depth is 8-bit */
                        if (descriptor->horiz_subsampling == 2 &&
                            descriptor->vert_subsampling == 1 &&
                            descriptor->component_depth == 8)
                            st->codec->pix_fmt = PIX_FMT_UYVY422;
                    }
                }
            }
            if (st->codec->codec_id == CODEC_ID_RAWVIDEO ||
                st->codec->codec_id == CODEC_ID_V210) {
                st->sample_aspect_ratio = st->codec->sample_aspect_ratio =
                    av_div_q(descriptor->aspect_ratio,
                             (AVRational){st->codec->width, st->codec->height});
            }
            st->need_parsing = AVSTREAM_PARSE_HEADERS;
            st->r_frame_rate = (AVRational){ material_track->edit_rate.den,
                                             material_track->edit_rate.num };
        } else if (st->codec->codec_type == AVMEDIA_TYPE_AUDIO) {
            container_ul = mxf_get_codec_ul(mxf_essence_container_uls, essence_container_ul);
            /* Only overwrite existing codec ID if it is unset or A-law, AVID is using wrong UL for some files */
            if (st->codec->codec_id == CODEC_ID_NONE ||
                (st->codec->codec_id == CODEC_ID_PCM_ALAW && container_ul->id != CODEC_ID_NONE))
                st->codec->codec_id = container_ul->id;
            if (descriptor->essence_codec_ul[0] == 0x00)
                st->codec->codec_id = CODEC_ID_PCM_S16LE;
            st->codec->channels = descriptor->channels;
            st->codec->bits_per_coded_sample = descriptor->bits_per_sample;
            st->codec->sample_rate = descriptor->sample_rate.num / descriptor->sample_rate.den;
            /* TODO: implement CODEC_ID_RAWAUDIO */
            if (st->codec->codec_id == CODEC_ID_PCM_S16LE) {
                if (descriptor->bits_per_sample > 16 && descriptor->bits_per_sample <= 24)
                    st->codec->codec_id = CODEC_ID_PCM_S24LE;
                else if (descriptor->bits_per_sample == 32)
                    st->codec->codec_id = CODEC_ID_PCM_S32LE;
            } else if (st->codec->codec_id == CODEC_ID_PCM_S16BE) {
                if (descriptor->bits_per_sample > 16 && descriptor->bits_per_sample <= 24)
                    st->codec->codec_id = CODEC_ID_PCM_S24BE;
                else if (descriptor->bits_per_sample == 32)
                    st->codec->codec_id = CODEC_ID_PCM_S32BE;
            } else if (st->codec->codec_id == CODEC_ID_MP2) {
                st->need_parsing = AVSTREAM_PARSE_FULL;
            }
        }
    }
    return 0;
}

static const MXFMetadataReadTableEntry mxf_metadata_read_table[] = {
    { { 0x06,0x0e,0x2B,0x34,0x02,0x05,0x01,0x01,0x0d,0x01,0x02,0x01,0x01,0x02,0x04,0x00 }, mxf_read_partition }, // closed complete
    { { 0x06,0x0E,0x2B,0x34,0x02,0x05,0x01,0x01,0x0d,0x01,0x02,0x01,0x01,0x05,0x01,0x00 }, mxf_read_primer_pack },
    { { 0x06,0x0E,0x2B,0x34,0x02,0x53,0x01,0x01,0x0d,0x01,0x01,0x01,0x01,0x01,0x18,0x00 }, mxf_read_content_storage, 0, AnyType },
    { { 0x06,0x0E,0x2B,0x34,0x02,0x53,0x01,0x01,0x0d,0x01,0x01,0x01,0x01,0x01,0x37,0x00 }, mxf_read_source_package, sizeof(MXFPackage), SourcePackage },
    { { 0x06,0x0E,0x2B,0x34,0x02,0x53,0x01,0x01,0x0d,0x01,0x01,0x01,0x01,0x01,0x36,0x00 }, mxf_read_material_package, sizeof(MXFPackage), MaterialPackage },
    { { 0x06,0x0E,0x2B,0x34,0x02,0x53,0x01,0x01,0x0d,0x01,0x01,0x01,0x01,0x01,0x0F,0x00 }, mxf_read_sequence, sizeof(MXFSequence), Sequence },
    { { 0x06,0x0E,0x2B,0x34,0x02,0x53,0x01,0x01,0x0d,0x01,0x01,0x01,0x01,0x01,0x11,0x00 }, mxf_read_source_clip, sizeof(MXFStructuralComponent), SourceClip },
    { { 0x06,0x0E,0x2B,0x34,0x02,0x53,0x01,0x01,0x0d,0x01,0x01,0x01,0x01,0x01,0x14,0x00 }, mxf_read_timecode_component, sizeof(MXFTimecodeComponent), TimecodeComponent },

    { { 0x06,0x0E,0x2B,0x34,0x02,0x53,0x01,0x01,0x0d,0x01,0x01,0x01,0x01,0x01,0x44,0x00 }, mxf_read_generic_descriptor, sizeof(MXFDescriptor), MultipleDescriptor },
    { { 0x06,0x0E,0x2B,0x34,0x02,0x53,0x01,0x01,0x0d,0x01,0x01,0x01,0x01,0x01,0x27,0x00 }, mxf_read_generic_descriptor, sizeof(MXFDescriptor), Descriptor }, /* Generic Picture */
    { { 0x06,0x0E,0x2B,0x34,0x02,0x53,0x01,0x01,0x0d,0x01,0x01,0x01,0x01,0x01,0x42,0x00 }, mxf_read_generic_descriptor, sizeof(MXFDescriptor), Descriptor }, /* Generic Sound */
    { { 0x06,0x0E,0x2B,0x34,0x02,0x53,0x01,0x01,0x0d,0x01,0x01,0x01,0x01,0x01,0x28,0x00 }, mxf_read_generic_descriptor, sizeof(MXFDescriptor), Descriptor }, /* CDCI */
    { { 0x06,0x0E,0x2B,0x34,0x02,0x53,0x01,0x01,0x0d,0x01,0x01,0x01,0x01,0x01,0x29,0x00 }, mxf_read_generic_descriptor, sizeof(MXFDescriptor), Descriptor }, /* RGBA */
    { { 0x06,0x0E,0x2B,0x34,0x02,0x53,0x01,0x01,0x0d,0x01,0x01,0x01,0x01,0x01,0x51,0x00 }, mxf_read_generic_descriptor, sizeof(MXFDescriptor), Descriptor }, /* MPEG 2 Video */
    { { 0x06,0x0E,0x2B,0x34,0x02,0x53,0x01,0x01,0x0d,0x01,0x01,0x01,0x01,0x01,0x48,0x00 }, mxf_read_generic_descriptor, sizeof(MXFDescriptor), Descriptor }, /* Wave */
    { { 0x06,0x0E,0x2B,0x34,0x02,0x53,0x01,0x01,0x0d,0x01,0x01,0x01,0x01,0x01,0x47,0x00 }, mxf_read_generic_descriptor, sizeof(MXFDescriptor), Descriptor }, /* AES3 */
    { { 0x06,0x0E,0x2B,0x34,0x02,0x53,0x01,0x01,0x0d,0x01,0x01,0x01,0x01,0x01,0x3A,0x00 }, mxf_read_track, sizeof(MXFTrack), Track }, /* Static Track */
    { { 0x06,0x0E,0x2B,0x34,0x02,0x53,0x01,0x01,0x0d,0x01,0x01,0x01,0x01,0x01,0x3B,0x00 }, mxf_read_track, sizeof(MXFTrack), Track }, /* Generic Track */
    { { 0x06,0x0E,0x2B,0x34,0x02,0x53,0x01,0x01,0x0d,0x01,0x04,0x01,0x02,0x02,0x00,0x00 }, mxf_read_cryptographic_context, sizeof(MXFCryptoContext), CryptoContext },
    { { 0x06,0x0E,0x2B,0x34,0x02,0x53,0x01,0x01,0x0d,0x01,0x02,0x01,0x01,0x10,0x01,0x00 }, mxf_read_index_table_segment, sizeof(MXFIndexTableSegment), IndexTableSegment },
    { { 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00 }, NULL, 0, AnyType },
};

static int mxf_read_local_tags(MXFContext *mxf, KLVPacket *klv, MXFMetadataReadFunc *read_child, int ctx_size, enum MXFMetadataSetType type)
{
    AVIOContext *pb = mxf->fc->pb;
    MXFMetadataSet *ctx = ctx_size ? av_mallocz(ctx_size) : mxf;
    uint64_t klv_end = avio_tell(pb) + klv->length;

    if (!ctx)
        return -1;
    while (avio_tell(pb) + 4 < klv_end) {
        int tag = avio_rb16(pb);
        int size = avio_rb16(pb); /* KLV specified by 0x53 */
        uint64_t next = avio_tell(pb) + size;
        UID uid = {0};

        av_dlog(mxf->fc, "local tag %#04x size %d\n", tag, size);
        if (!size) { /* ignore empty tag, needed for some files with empty UMID tag */
            av_log(mxf->fc, AV_LOG_ERROR, "local tag %#04x with 0 size\n", tag);
            continue;
        }
        if (tag > 0x7FFF) { /* dynamic tag */
            int i;
            for (i = 0; i < mxf->local_tags_count; i++) {
                int local_tag = AV_RB16(mxf->local_tags+i*18);
                if (local_tag == tag) {
                    memcpy(uid, mxf->local_tags+i*18+2, 16);
                    PRINT_KEY(mxf->fc, "uid", uid);
                    break;
                }
            }
        }
        if (ctx_size && tag == 0x3C0A)
            avio_read(pb, ctx->uid, 16);
        else if (read_child(mxf->fc, ctx, tag, size, uid) < 0)
            return -1;

        avio_seek(pb, next, SEEK_SET);
    }
    if (ctx_size) ctx->type = type;
    return ctx_size ? mxf_add_metadata_set(mxf, ctx) : 0;
}

static int mxf_read_random_index_pack(AVFormatContext *s)
{
    UID key;
    int size;

    avio_seek(s->pb, avio_size(s->pb) - 4, SEEK_SET);
    size = avio_rb32(s->pb);
    avio_seek(s->pb, avio_size(s->pb) -size, SEEK_SET);
    avio_read(s->pb, key, 16);
    if (IS_KLV_KEY(key, mxf_random_index_pack_key)) {
        int len = klv_decode_ber_length(s->pb);
        uint64_t offset = 0;
        for (; len >= 12; len -= 12) {
            avio_rb32(s->pb); // BodySID
            offset = avio_rb64(s->pb);
        }
        if (!offset)
            return -1;
        avio_seek(s->pb, offset, SEEK_SET);
        avio_read(s->pb, key, 16);
        PRINT_KEY(s, "rip key", key);
        if (IS_KLV_KEY(key, mxf_footer_partition_key)) {
            avio_seek(s->pb, offset, SEEK_SET);
            return 0;
        }
    }
    return -1;
}

static int mxf_read_header(AVFormatContext *s, AVFormatParameters *ap)
{
    MXFContext *mxf = s->priv_data;
    KLVPacket klv;
    int ret;
    uint64_t essence_klv_offset = 0;

    if (!mxf_read_sync(s->pb, mxf_header_partition_pack_key, 14)) {
        av_log(s, AV_LOG_ERROR, "could not find header partition pack key\n");
        return -1;
    }
    avio_seek(s->pb, -14, SEEK_CUR);
    mxf->fc = s;
    while (!url_feof(s->pb)) {
        const MXFMetadataReadTableEntry *metadata;

        if (klv_read_packet(&klv, s->pb) < 0)
            break;
        PRINT_KEY(s, "read header", klv.key);
        av_dlog(s, "size %"PRIu64" offset %#"PRIx64"\n", klv.length, klv.offset);
        if (IS_KLV_KEY(klv.key, mxf_system_metadata_pack_key)) {
            mxf_parse_system_metadata_pack(s, &klv);
            continue;
        }
        if (IS_KLV_KEY(klv.key, mxf_encrypted_triplet_key) ||
            IS_KLV_KEY(klv.key, mxf_essence_element_key)   ||
            IS_KLV_KEY(klv.key, mxf_avid_essence_element_key)) {
            essence_klv_offset = klv.offset;

            if (s->pb->seekable) {
                if (mxf->footer_partition) {
                    avio_seek(s->pb, mxf->footer_partition, SEEK_SET);
                    mxf->footer_partition = 0;
                    continue;
                } else { // try scanning RIP
                    ret = mxf_read_random_index_pack(s);
                    if (ret < 0)
                        break;
                    continue;
                }
            } else
                break;
        }

        for (metadata = mxf_metadata_read_table; metadata->read; metadata++) {
            if (IS_KLV_KEY(klv.key, metadata->key)) {
                if (klv.key[5] == 0x53)
                    ret = mxf_read_local_tags(mxf, &klv, metadata->read, metadata->ctx_size, metadata->type);
                else
                    ret = metadata->read(s, mxf, 0, 0, NULL);
                if (ret < 0) {
                    av_log(s, AV_LOG_ERROR, "error reading header metadata\n");
                    return -1;
                }
                break;
            }
        }
        if (!metadata->read)
            avio_skip(s->pb, klv.length);
    }

    ret = mxf_parse_structural_metadata(mxf);
    if (ret < 0)
        return ret;

    if (!essence_klv_offset) {
        av_log(s, AV_LOG_ERROR, "could not find any essence element packet\n");
        return -1;
    }

    avio_seek(s->pb, essence_klv_offset, SEEK_SET);
    return 0;
}

static int mxf_read_close(AVFormatContext *s)
{
    MXFContext *mxf = s->priv_data;
    int i;

    av_freep(&mxf->packages_refs);

    for (i = 0; i < s->nb_streams; i++)
        s->streams[i]->priv_data = NULL;

    for (i = 0; i < mxf->metadata_sets_count; i++) {
        switch (mxf->metadata_sets[i]->type) {
        case MultipleDescriptor:
            av_freep(&((MXFDescriptor *)mxf->metadata_sets[i])->sub_descriptors_refs);
            break;
        case Sequence:
            av_freep(&((MXFSequence *)mxf->metadata_sets[i])->structural_components_refs);
            break;
        case SourcePackage:
        case MaterialPackage:
            av_freep(&((MXFPackage *)mxf->metadata_sets[i])->tracks_refs);
            break;
        default:
            break;
        }
        av_freep(&mxf->metadata_sets[i]);
    }
    av_freep(&mxf->metadata_sets);
    av_freep(&mxf->aesc);
    av_freep(&mxf->local_tags);
    return 0;
}

static int mxf_probe(AVProbeData *p) {
    uint8_t *bufp = p->buf;
    uint8_t *end = p->buf + p->buf_size;

    if (p->buf_size < sizeof(mxf_header_partition_pack_key))
        return 0;

    /* Must skip Run-In Sequence and search for MXF header partition pack key SMPTE 377M 5.5 */
    end -= sizeof(mxf_header_partition_pack_key);
    for (; bufp < end; bufp++) {
        if (IS_KLV_KEY(bufp, mxf_header_partition_pack_key))
            return AVPROBE_SCORE_MAX;
    }
    return 0;
}

/* rudimentary byte seek */
/* XXX: use MXF Index */
static int mxf_read_seek(AVFormatContext *s, int stream_index, int64_t sample_time, int flags)
{
    MXFContext *mxf = s->priv_data;
    AVStream *st = s->streams[stream_index];
    MXFTrack *track = st->priv_data;
    int64_t seconds, offset;

    if (sample_time < 0)
        sample_time = 0;

    if (mxf->op == OpAtom && s->nb_streams == 1 && track->edit_unit_bytecount) {
        offset = s->data_offset + track->edit_unit_bytecount * sample_time;
    } else {
        if (!s->bit_rate)
            return -1;
        seconds = av_rescale(sample_time, st->time_base.num, st->time_base.den);
        offset = (s->bit_rate * seconds) >> 3;
    }

    avio_seek(s->pb, offset, SEEK_SET);
    av_update_cur_dts(s, st, sample_time);
    return 0;
}

AVInputFormat ff_mxf_demuxer = {
    .name           = "mxf",
    .long_name      = NULL_IF_CONFIG_SMALL("Material eXchange Format"),
    .priv_data_size = sizeof(MXFContext),
    .read_probe     = mxf_probe,
    .read_header    = mxf_read_header,
    .read_packet    = mxf_read_packet,
    .read_close     = mxf_read_close,
    .read_seek      = mxf_read_seek,
};
