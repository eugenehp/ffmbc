/*
 * DV decoder
 * Copyright (c) 2002 Fabrice Bellard
 * Copyright (c) 2004 Roman Shaposhnik
 *
 * DV encoder
 * Copyright (c) 2003 Roman Shaposhnik
 *
 * 50 Mbps (DVCPRO50) support
 * Copyright (c) 2006 Daniel Maas <dmaas@maasdigital.com>
 *
 * 100 Mbps (DVCPRO HD) support
 * Initial code by Daniel Maas <dmaas@maasdigital.com> (funded by BBC R&D)
 * Final code by Roman Shaposhnik
 *
 * Many thanks to Dan Dennedy <dan@dennedy.org> for providing wealth
 * of DV technical info.
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
 * DV codec.
 */
#define ALT_BITSTREAM_READER
#include "libavutil/pixdesc.h"
#include "avcodec.h"
#include "get_bits.h"
#include "put_bits.h"
#include "simple_idct.h"
#include "dvdata.h"

//#undef NDEBUG
//#include <assert.h>

/* XXX: also include quantization */
static RL_VLC_ELEM dv_rl_vlc[1184];

static av_cold int dvvideo_init(AVCodecContext *avctx)
{
    DVVideoContext *s = avctx->priv_data;
    static int done = 0;
    int i;

    if (!done) {
        ff_dv_init_vlc(dv_rl_vlc);
        ff_dv_vlc_map_tableinit();
        done = 1;
    }

    /* Generic DSP setup */
    dsputil_init(&s->dsp, avctx);
    ff_set_cmp(&s->dsp, s->dsp.ildct_cmp, avctx->ildct_cmp);

    /* 88DCT setup */
    s->fdct[0]     = s->dsp.fdct;
    s->idct_put[0] = s->dsp.idct_put;
    for (i = 0; i < 64; i++)
       s->dv_zigzag[0][i] = s->dsp.idct_permutation[ff_zigzag_direct[i]];

    /* 248DCT setup */
    s->fdct[1]     = s->dsp.fdct248;
    s->idct_put[1] = ff_simple_idct248_put;  // FIXME: need to add it to DSP
    if (avctx->lowres){
        for (i = 0; i < 64; i++){
            int j = ff_zigzag248_direct[i];
            s->dv_zigzag[1][i] = s->dsp.idct_permutation[(j & 7) + (j & 8) * 4 + (j & 48) / 2];
        }
    }else
        memcpy(s->dv_zigzag[1], ff_zigzag248_direct, 64);

    avctx->coded_frame = &s->picture;
    s->avctx = avctx;
    avctx->chroma_sample_location = AVCHROMA_LOC_TOPLEFT;

    return 0;
}

typedef struct BlockInfo {
    const uint32_t *factor_table;
    const uint8_t *scan_table;
    uint8_t pos; /* position in block */
    void (*idct_put)(uint8_t *dest, int line_size, DCTELEM *block);
    uint8_t partial_bit_count;
    uint16_t partial_bit_buffer;
    int shift_offset;
} BlockInfo;

/* decode AC coefficients */
static void dv_decode_ac(GetBitContext *gb, BlockInfo *mb, DCTELEM *block)
{
    int last_index = gb->size_in_bits;
    const uint8_t  *scan_table   = mb->scan_table;
    const uint32_t *factor_table = mb->factor_table;
    int pos               = mb->pos;
    int partial_bit_count = mb->partial_bit_count;
    int level, run, vlc_len, index;

    OPEN_READER(re, gb);
    UPDATE_CACHE(re, gb);

    /* if we must parse a partial VLC, we do it here */
    if (partial_bit_count > 0) {
        re_cache = ((unsigned)re_cache >> partial_bit_count) |
                   (mb->partial_bit_buffer << (sizeof(re_cache) * 8 - partial_bit_count));
        re_index -= partial_bit_count;
        mb->partial_bit_count = 0;
    }

    /* get the AC coefficients until last_index is reached */
    for (;;) {
        av_dlog(NULL, "%2d: bits=%04x index=%d\n", pos, SHOW_UBITS(re, gb, 16),
                re_index);
        /* our own optimized GET_RL_VLC */
        index   = NEG_USR32(re_cache, TEX_VLC_BITS);
        vlc_len = dv_rl_vlc[index].len;
        if (vlc_len < 0) {
            index = NEG_USR32((unsigned)re_cache << TEX_VLC_BITS, -vlc_len) + dv_rl_vlc[index].level;
            vlc_len = TEX_VLC_BITS - vlc_len;
        }
        level = dv_rl_vlc[index].level;
        run   = dv_rl_vlc[index].run;

        /* gotta check if we're still within gb boundaries */
        if (re_index + vlc_len > last_index) {
            /* should be < 16 bits otherwise a codeword could have been parsed */
            mb->partial_bit_count = last_index - re_index;
            mb->partial_bit_buffer = NEG_USR32(re_cache, mb->partial_bit_count);
            re_index = last_index;
            break;
        }
        re_index += vlc_len;

        av_dlog(NULL, "run=%d level=%d\n", run, level);
        pos += run;
        if (pos >= 64)
            break;

        level = (level * factor_table[pos] + (1 << (dv_iweight_bits - 1))) >> dv_iweight_bits;
        block[scan_table[pos]] = level;

        UPDATE_CACHE(re, gb);
    }
    CLOSE_READER(re, gb);
    mb->pos = pos;
}

static inline void bit_copy(PutBitContext *pb, GetBitContext *gb)
{
    int bits_left = get_bits_left(gb);
    while (bits_left >= MIN_CACHE_BITS) {
        put_bits(pb, MIN_CACHE_BITS, get_bits(gb, MIN_CACHE_BITS));
        bits_left -= MIN_CACHE_BITS;
    }
    if (bits_left > 0) {
        put_bits(pb, bits_left, get_bits(gb, bits_left));
    }
}

static av_always_inline void put_block_8x4(DCTELEM *block, uint8_t *restrict p, int linesize)
{
    int i, j;
    uint8_t *cm = ff_cropTbl + MAX_NEG_CROP;

    for (i = 0; i < 4; i++) {
        for (j = 0; j < 8; j++)
            p[j] = cm[block[j]];
        block += 8;
        p += linesize;
    }
}

static void dv100_idct_put_last_row_field_chroma(DVVideoContext *s, uint8_t *data,
                                                 int linesize, DCTELEM *blocks)
{
    s->dsp.idct(blocks + 0*64);
    s->dsp.idct(blocks + 1*64);

    put_block_8x4(blocks+0*64,       data,                linesize*2);
    put_block_8x4(blocks+0*64 + 4*8, data + 8,            linesize*2);
    put_block_8x4(blocks+1*64,       data + linesize,     linesize*2);
    put_block_8x4(blocks+1*64 + 4*8, data + 8 + linesize, linesize*2);
}

static void dv100_idct_put_last_row_field_luma(DVVideoContext *s, uint8_t *data,
                                               int linesize, DCTELEM *blocks)
{
    s->dsp.idct(blocks + 0*64);
    s->dsp.idct(blocks + 1*64);
    s->dsp.idct(blocks + 2*64);
    s->dsp.idct(blocks + 3*64);

    put_block_8x4(blocks+0*64,       data,                 linesize*2);
    put_block_8x4(blocks+0*64 + 4*8, data + 16,            linesize*2);
    put_block_8x4(blocks+1*64,       data + 8,             linesize*2);
    put_block_8x4(blocks+1*64 + 4*8, data + 24,            linesize*2);
    put_block_8x4(blocks+2*64,       data + linesize,      linesize*2);
    put_block_8x4(blocks+2*64 + 4*8, data + 16 + linesize, linesize*2);
    put_block_8x4(blocks+3*64,       data + 8  + linesize, linesize*2);
    put_block_8x4(blocks+3*64 + 4*8, data + 24 + linesize, linesize*2);
}

/* mb_x and mb_y are in units of 8 pixels */
static int dv_decode_video_segment(AVCodecContext *avctx, void *arg)
{
    DVVideoContext *s = avctx->priv_data;
    DVwork_chunk *work_chunk = arg;
    int quant, dc, dct_mode, class1, j;
    int mb_index, mb_x, mb_y, last_index;
    int y_stride, linesize;
    DCTELEM *block, *block1;
    int c_offset;
    uint8_t *y_ptr;
    const uint8_t *buf_ptr;
    PutBitContext pb, vs_pb;
    GetBitContext gb;
    BlockInfo mb_data[5 * DV_MAX_BPM], *mb, *mb1;
    LOCAL_ALIGNED_16(DCTELEM, sblock, [5*DV_MAX_BPM], [64]);
    LOCAL_ALIGNED_16(uint8_t, mb_bit_buffer, [  80 + FF_INPUT_BUFFER_PADDING_SIZE]); /* allow some slack */
    LOCAL_ALIGNED_16(uint8_t, vs_bit_buffer, [5*80 + FF_INPUT_BUFFER_PADDING_SIZE]); /* allow some slack */
    const int log2_blocksize = 3-s->avctx->lowres;
    int is_field_mode[5];

    assert((((int)mb_bit_buffer) & 7) == 0);
    assert((((int)vs_bit_buffer) & 7) == 0);

    memset(sblock, 0, 5*DV_MAX_BPM*sizeof(*sblock));

    /* pass 1: read DC and AC coefficients in blocks */
    buf_ptr = &s->buf[work_chunk->buf_offset*80];
    block1  = &sblock[0][0];
    mb1     = mb_data;
    init_put_bits(&vs_pb, vs_bit_buffer, 5 * 80);
    for (mb_index = 0; mb_index < 5; mb_index++, mb1 += s->sys->bpm, block1 += s->sys->bpm * 64) {
        /* skip header */
        quant = buf_ptr[3] & 0x0f;
        buf_ptr += 4;
        init_put_bits(&pb, mb_bit_buffer, 80);
        mb    = mb1;
        block = block1;
        is_field_mode[mb_index] = 0;
        for (j = 0; j < s->sys->bpm; j++) {
            last_index = s->sys->block_sizes[j];
            init_get_bits(&gb, buf_ptr, last_index);

            /* get the DC */
            dc       = get_sbits(&gb, 9);
            dct_mode = get_bits1(&gb);
            class1   = get_bits(&gb, 2);
            if (DV_PROFILE_IS_HD(s->sys)) {
                mb->idct_put     = s->idct_put[0];
                mb->scan_table   = s->dv_zigzag[0];
                mb->factor_table = &s->sys->idct_factor[(j >= 4)*4*16*64 + class1*16*64 + quant*64];
                is_field_mode[mb_index] |= !j && dct_mode;
            } else {
                mb->idct_put     = s->idct_put[dct_mode && log2_blocksize == 3];
                mb->scan_table   = s->dv_zigzag[dct_mode];
                mb->factor_table = &s->sys->idct_factor[(class1 == 3)*2*22*64 + dct_mode*22*64 +
                                                        (quant + dv_quant_offset[class1])*64];
            }
            dc = dc << 2;
            /* convert to unsigned because 128 is not added in the
               standard IDCT */
            dc += 1024;
            block[0] = dc;
            buf_ptr += last_index >> 3;
            mb->pos               = 0;
            mb->partial_bit_count = 0;

            av_dlog(avctx, "MB block: %d, %d ", mb_index, j);
            dv_decode_ac(&gb, mb, block);

            /* write the remaining bits in a new buffer only if the
               block is finished */
            if (mb->pos >= 64)
                bit_copy(&pb, &gb);

            block += 64;
            mb++;
        }

        /* pass 2: we can do it just after */
        av_dlog(avctx, "***pass 2 size=%d MB#=%d\n", put_bits_count(&pb), mb_index);
        block = block1;
        mb    = mb1;
        init_get_bits(&gb, mb_bit_buffer, put_bits_count(&pb));
        put_bits32(&pb, 0); // padding must be zeroed
        flush_put_bits(&pb);
        for (j = 0; j < s->sys->bpm; j++, block += 64, mb++) {
            if (mb->pos < 64 && get_bits_left(&gb) > 0) {
                dv_decode_ac(&gb, mb, block);
                /* if still not finished, no need to parse other blocks */
                if (mb->pos < 64)
                    break;
            }
        }
        /* all blocks are finished, so the extra bytes can be used at
           the video segment level */
        if (j >= s->sys->bpm)
            bit_copy(&vs_pb, &gb);
    }

    /* we need a pass over the whole video segment */
    av_dlog(avctx, "***pass 3 size=%d\n", put_bits_count(&vs_pb));
    block = &sblock[0][0];
    mb    = mb_data;
    init_get_bits(&gb, vs_bit_buffer, put_bits_count(&vs_pb));
    put_bits32(&vs_pb, 0); // padding must be zeroed
    flush_put_bits(&vs_pb);
    for (mb_index = 0; mb_index < 5; mb_index++) {
        for (j = 0; j < s->sys->bpm; j++) {
            if (mb->pos < 64) {
                av_dlog(avctx, "start %d:%d\n", mb_index, j);
                dv_decode_ac(&gb, mb, block);
            }
            if (mb->pos >= 64 && mb->pos < 127)
                av_log(avctx, AV_LOG_ERROR, "AC EOB marker is absent pos=%d\n", mb->pos);
            block += 64;
            mb++;
        }
    }

    /* compute idct and place blocks */
    block = &sblock[0][0];
    mb    = mb_data;
    for (mb_index = 0; mb_index < 5; mb_index++) {
        dv_calculate_mb_xy(s, work_chunk, mb_index, &mb_x, &mb_y);

        /* idct_put'ting luminance */
        if ((s->sys->pix_fmt == PIX_FMT_YUV420P) ||
            (s->sys->pix_fmt == PIX_FMT_YUV411P && mb_x >= (704 / 8)) ||
            (s->sys->height >= 720 && mb_y != 134)) {
            y_stride = (s->picture.linesize[0] << ((!is_field_mode[mb_index]) * log2_blocksize));
        } else {
            y_stride = (2 << log2_blocksize);
        }
        y_ptr = s->picture.data[0] + ((mb_y * s->picture.linesize[0] + mb_x) << log2_blocksize);
        if (mb_y == 134 && is_field_mode[mb_index]) {
            dv100_idct_put_last_row_field_luma(s, y_ptr, s->picture.linesize[0], block);
        } else {
            linesize = s->picture.linesize[0] << is_field_mode[mb_index];
            mb[0]    .idct_put(y_ptr                                   , linesize, block + 0*64);
            if (s->sys->video_stype == 4) { /* SD 422 */
                mb[2].idct_put(y_ptr + (1 << log2_blocksize)           , linesize, block + 2*64);
            } else {
                mb[1].idct_put(y_ptr + (1 << log2_blocksize)           , linesize, block + 1*64);
                mb[2].idct_put(y_ptr                         + y_stride, linesize, block + 2*64);
                mb[3].idct_put(y_ptr + (1 << log2_blocksize) + y_stride, linesize, block + 3*64);
            }
        }
        mb += 4;
        block += 4*64;

        if (s->avctx->flags & CODEC_FLAG_GRAY) {
            mb += s->sys->bpm - 4;
            block += (s->sys->bpm - 4)*64;
            continue;
        }

        /* idct_put'ting chrominance */
        c_offset = (((mb_y >>  (s->sys->pix_fmt == PIX_FMT_YUV420P)) * s->picture.linesize[1] +
                     (mb_x >> ((s->sys->pix_fmt == PIX_FMT_YUV411P) ? 2 : 1))) << log2_blocksize);
        for (j = 2; j; j--) {
            uint8_t *c_ptr = s->picture.data[j] + c_offset;
            if (s->sys->pix_fmt == PIX_FMT_YUV411P && mb_x >= (704 / 8)) {
                  uint64_t aligned_pixels[64/8];
                  uint8_t *pixels = (uint8_t*)aligned_pixels;
                  uint8_t *c_ptr1, *ptr1;
                  int x, y;
                  mb->idct_put(pixels, 8, block);
                  for (y = 0; y < (1 << log2_blocksize); y++, c_ptr += s->picture.linesize[j], pixels += 8) {
                      ptr1   = pixels + (1 << (log2_blocksize - 1));
                      c_ptr1 = c_ptr + (s->picture.linesize[j] << log2_blocksize);
                      for (x = 0; x < (1 << (log2_blocksize - 1)); x++) {
                          c_ptr[x]  = pixels[x];
                          c_ptr1[x] = ptr1[x];
                      }
                  }
                  block += 64; mb++;
            } else {
                  y_stride = (mb_y == 134) ? (1 << log2_blocksize) :
                                             s->picture.linesize[j] << ((!is_field_mode[mb_index]) * log2_blocksize);
                  if (mb_y == 134 && is_field_mode[mb_index]) {
                      dv100_idct_put_last_row_field_chroma(s, c_ptr, s->picture.linesize[j], block);
                      mb += 2;
                      block += 2*64;
                  } else {
                      linesize = s->picture.linesize[j] << is_field_mode[mb_index];
                      (mb++)->    idct_put(c_ptr           , linesize, block); block += 64;
                      if (s->sys->bpm == 8) {
                          (mb++)->idct_put(c_ptr + y_stride, linesize, block); block += 64;
                      }
                  }
            }
        }
    }
    return 0;
}

/* NOTE: exactly one frame must be given (120000 bytes for NTSC,
   144000 bytes for PAL - or twice those for 50Mbps) */
static int dvvideo_decode_frame(AVCodecContext *avctx,
                                 void *data, int *data_size,
                                 AVPacket *avpkt)
{
    uint8_t *buf = avpkt->data;
    int buf_size = avpkt->size;
    DVVideoContext *s = avctx->priv_data;
    const uint8_t* vsc_pack;

    s->sys = ff_dv_frame_profile(s->sys, buf, buf_size);
    if (!s->sys || buf_size < s->sys->frame_size || ff_dv_init_dynamic_tables(s->sys)) {
        av_log(avctx, AV_LOG_ERROR, "could not find dv frame profile\n");
        return -1; /* NOTE: we only accept several full frames */
    }

    if (s->picture.data[0])
        avctx->release_buffer(avctx, &s->picture);

    avcodec_get_frame_defaults(&s->picture);
    s->picture.reference = 0;
    s->picture.key_frame = 1;
    s->picture.pict_type = AV_PICTURE_TYPE_I;
    avctx->pix_fmt   = s->sys->pix_fmt;
    avctx->time_base = s->sys->time_base;
    avcodec_set_dimensions(avctx, s->sys->width, s->sys->height);
    if (avctx->get_buffer(avctx, &s->picture) < 0) {
        av_log(avctx, AV_LOG_ERROR, "get_buffer() failed\n");
        return -1;
    }

    s->buf = buf;
    avctx->execute(avctx, dv_decode_video_segment, s->sys->work_chunks, NULL,
                   dv_work_pool_size(s->sys), sizeof(DVwork_chunk));

    emms_c();

    /* Determine the codec's sample_aspect ratio from the packet */
    vsc_pack = buf + 80*5 + 48 + 5;
    if (*vsc_pack == dv_video_control) {
        int apt = buf[4] & 0x07;
        int is16_9 = (vsc_pack[2] & 0x07) == 0x02 || (!apt && (vsc_pack[2] & 0x07) == 0x07);
        avctx->sample_aspect_ratio = s->sys->sar[is16_9];
        if (avctx->height != 720) {
            s->picture.interlaced_frame = (vsc_pack[3] & 0x10) == 0x10;
            if (avctx->height == 1080)
                s->picture.top_field_first = (vsc_pack[3] & 0x40) == 0x40;
            else
                s->picture.top_field_first = (vsc_pack[3] & 0x40) != 0x40;
        }
    }

    /* return image */
    *data_size = sizeof(AVFrame);
    *(AVFrame*)data = s->picture;

    return s->sys->frame_size;
}

static int dvvideo_close(AVCodecContext *c)
{
    DVVideoContext *s = c->priv_data;

    if (s->picture.data[0])
        c->release_buffer(c, &s->picture);

    return 0;
}

AVCodec ff_dvvideo_decoder = {
    "dvvideo",
    AVMEDIA_TYPE_VIDEO,
    CODEC_ID_DVVIDEO,
    sizeof(DVVideoContext),
    dvvideo_init,
    NULL,
    dvvideo_close,
    dvvideo_decode_frame,
    CODEC_CAP_DR1 | CODEC_CAP_SLICE_THREADS,
    NULL,
    .max_lowres = 3,
    .long_name = NULL_IF_CONFIG_SMALL("DV (Digital Video)"),
};
