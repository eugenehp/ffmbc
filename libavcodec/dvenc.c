/*
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
#include "dsputil.h"
#include "put_bits.h"
#include "dvdata.h"

//#undef NDEBUG
//#include <assert.h>

static av_cold int dvvideo_init(AVCodecContext *avctx)
{
    DVVideoContext *s = avctx->priv_data;

    ff_dv_vlc_map_tableinit();

    if (avctx->dct_algo != FF_DCT_INT && avctx->dct_algo != FF_DCT_AUTO) {
        av_log(avctx, AV_LOG_ERROR, "error, dct algorithm not supported\n");
        return -1;
    }
    if (avctx->idct_algo != FF_IDCT_SIMPLE && avctx->idct_algo != FF_IDCT_AUTO) {
        av_log(avctx, AV_LOG_ERROR, "error, idct algorithm not supported\n");
        return -1;
    }
    /* Generic DSP setup */
    dsputil_init(&s->dsp, avctx);
    ff_set_cmp(&s->dsp, s->dsp.ildct_cmp, avctx->ildct_cmp);

    /* 88DCT setup */
    s->fdct[0] = s->dsp.fdct;

    /* 248DCT setup */
    s->fdct[1] = s->dsp.fdct248;

    avctx->coded_frame = &s->picture;
    s->avctx = avctx;
    avctx->chroma_sample_location = AVCHROMA_LOC_TOPLEFT;

    return 0;
}

static av_cold int dvvideo_init_encoder(AVCodecContext *avctx)
{
    DVVideoContext *s = avctx->priv_data;
    s->sys = ff_dv_codec_profile(avctx);

    if (!s->sys) {
        av_log(avctx, AV_LOG_ERROR, "Found no DV profile for %ix%i %s video\n",
               avctx->width, avctx->height, av_get_pix_fmt_name(avctx->pix_fmt));
        return -1;
    }

    ff_dv_init_dynamic_tables(s->sys);

    avctx->bit_rate = s->sys->frame_size * 8LL *
        avctx->time_base.den / avctx->time_base.num;

    return dvvideo_init(avctx);
}

/* bit budget for AC only in 5 MBs */
static const int vs_total_ac_bits_hd = (68 * 6 + 52*2) * 5;
static const int vs_total_ac_bits = (100 * 4 + 68*2) * 5;
static const int mb_area_start[5] = { 1, 6, 21, 43, 64 };

typedef struct EncBlockInfo {
    int      area_q[4];
    int      bit_size[4];
    int      prev[5];
    int      cur_ac;
    int      cno;
    int      dct_mode;
    DCTELEM  mb[64];
    uint8_t  next[64];
    uint8_t  sign[64];
    uint8_t  partial_bit_count;
    uint32_t partial_bit_buffer; /* we can't use uint16_t here */
    /* used by DV100 only: a copy of the weighted and classified but
       not-yet-quantized AC coefficients. This is necessary for
       re-quantizing at different steps. */
    DCTELEM  save[64];
    int      min_qlevel; /* DV100 only: minimum qlevel (for AC coefficients >255) */
} EncBlockInfo;

static inline int put_bits_left(PutBitContext* s)
{
    return (s->buf_end - s->buf) * 8 - put_bits_count(s);
}

static av_always_inline PutBitContext *dv_encode_ac(EncBlockInfo *bi,
                                                    PutBitContext *pb_pool,
                                                    PutBitContext *pb_end)
{
    int prev, bits_left;
    PutBitContext *pb = pb_pool;
    int size = bi->partial_bit_count;
    uint32_t vlc = bi->partial_bit_buffer;

    bi->partial_bit_count = bi->partial_bit_buffer = 0;
    for (;;) {
        /* Find suitable storage space */
        for (; size > (bits_left = put_bits_left(pb)); pb++) {
            if (bits_left) {
                size -= bits_left;
                put_bits(pb, bits_left, vlc >> size);
                vlc = vlc & ((1 << size) - 1);
            }
            if (pb + 1 >= pb_end) {
                bi->partial_bit_count  = size;
                bi->partial_bit_buffer = vlc;
                return pb;
            }
        }

        /* Store VLC */
        put_bits(pb, size, vlc);

        if (bi->cur_ac >= 64)
            break;

        /* Construct the next VLC */
        prev       = bi->cur_ac;
        bi->cur_ac = bi->next[prev];
        if (bi->cur_ac < 64){
            size = dv_rl2vlc(bi->cur_ac - prev - 1, bi->mb[bi->cur_ac], bi->sign[bi->cur_ac], &vlc);
        } else {
            size = 4; vlc = 6; /* End Of Block stamp */
        }
    }
    return pb;
}

static av_always_inline int dv_guess_dct_mode(DVVideoContext *s, uint8_t *data, int linesize) {
    if (s->avctx->flags & CODEC_FLAG_INTERLACED_DCT) {
        int ps = s->dsp.ildct_cmp[5](NULL, data, NULL, linesize, 8) - 400;
        if (ps > 0) {
            int is = s->dsp.ildct_cmp[5](NULL, data           , NULL, linesize<<1, 4) +
                     s->dsp.ildct_cmp[5](NULL, data + linesize, NULL, linesize<<1, 4);
            return ps > is;
        }
    }

    return 0;
}

static inline void dv_set_class_number_sd(DCTELEM *blk, EncBlockInfo *bi,
                                          const uint8_t *zigzag_scan,
                                          const int *weight, int bias)
{
    int i, area;
    /* We offer two different methods for class number assignment: the
       method suggested in SMPTE 314M Table 22, and an improved
       method. The SMPTE method is very conservative; it assigns class
       3 (i.e. severe quantization) to any block where the largest AC
       component is greater than 36. FFmpeg's DV encoder tracks AC bit
       consumption precisely, so there is no need to bias most blocks
       towards strongly lossy compression. Instead, we assign class 2
       to most blocks, and use class 3 only when strictly necessary
       (for blocks whose largest AC component exceeds 255). */

#if 0 /* SMPTE spec method */
    static const int classes[] = {12, 24, 36, 0xffff};
#else /* improved FFmpeg method */
    static const int classes[] = {-1, -1, 255, 0xffff};
#endif
    int max  = classes[0];
    int prev = 0;

    bi->mb[0] = blk[0];

    for (area = 0; area < 4; area++) {
        bi->prev[area]     = prev;
        bi->bit_size[area] = 1; // 4 areas 4 bits for EOB :)
        for (i = mb_area_start[area]; i < mb_area_start[area+1]; i++) {
            int level = blk[zigzag_scan[i]];

            if (level + 15 > 30U) {
                bi->sign[i] = (level >> 31) & 1;
                /* weight it and and shift down into range, adding for rounding */
                /* the extra division by a factor of 2^4 reverses the 8x expansion of the DCT
                   AND the 2x doubling of the weights */
                level = (FFABS(level) * weight[i] + (1 << (dv_weight_bits+3))) >> (dv_weight_bits+4);
                bi->mb[i] = level;
                if (level > max)
                    max = level;
                bi->bit_size[area] += dv_rl2vlc_size(i - prev  - 1, level);
                bi->next[prev] = i;
                prev = i;
            }
        }
    }
    bi->next[prev]= i;
    for (bi->cno = 0; max > classes[bi->cno]; bi->cno++)
        ;
    bi->cno += bias;

    if (bi->cno >= 3) {
        bi->cno = 3;
        prev    = 0;
        i       = bi->next[prev];
        for (area = 0; area < 4; area++) {
            bi->prev[area]     = prev;
            bi->bit_size[area] = 1; // 4 areas 4 bits for EOB :)
            for (; i < mb_area_start[area+1]; i = bi->next[i]) {
                bi->mb[i] >>= 1;
                if (bi->mb[i]) {
                    bi->bit_size[area] += dv_rl2vlc_size(i - prev - 1, bi->mb[i]);
                    bi->next[prev] = i;
                    prev = i;
                }
            }
        }
        bi->next[prev] = i;
    }
}

/* this function just copies the DCT coefficients and performs
   the initial (non-)quantization. */
static inline void dv_set_class_number_hd(DCTELEM *blk, EncBlockInfo *bi,
                                          const uint8_t *zigzag_scan,
                                          const int *weight, int bias)
{
    int i, max = 0;

    /* the first quantization (none at all) */
    bi->area_q[0] = 1;

    /* LOOP1: weigh AC components and store to save[] */
    /* (i=0 is the DC component; we only include it to make the
       number of loop iterations even, for future possible SIMD optimization) */
    for (i = 0; i < 64; i += 2) {
        int level0, level1;

        /* get the AC component (in zig-zag order) */
        level0 = blk[zigzag_scan[i+0]];
        level1 = blk[zigzag_scan[i+1]];

        /* extract sign and make it the lowest bit */
        bi->sign[i+0] = (level0>>31)&1;
        bi->sign[i+1] = (level1>>31)&1;

        /* take absolute value of the level */
        level0 = FFABS(level0);
        level1 = FFABS(level1);

        /* weigh it */
        level0 = (level0*weight[i+0] + 4096 + (1<<17)) >> 18;
        level1 = (level1*weight[i+1] + 4096 + (1<<17)) >> 18;

        /* save unquantized value */
        bi->save[i+0] = level0;
        bi->save[i+1] = level1;
    }

    /* find max component */
    for (i = 0; i < 64; i++) {
        int ac = bi->save[i];
        if (ac > max)
            max = ac;
    }

    /* copy DC component */
    bi->mb[0] = blk[0];

    /* the EOB code is 4 bits */
    bi->bit_size[0] = 4;
    bi->bit_size[1] = bi->bit_size[2] = bi->bit_size[3] = 0;

    /* ensure that no AC coefficients are cut off */
    bi->min_qlevel = ((max+256) >> 8);

    bi->area_q[0] = 25; /* set to an "impossible" value */
    bi->cno = 0;
}

static av_always_inline int dv_init_enc_block(EncBlockInfo* bi, uint8_t *data, int linesize,
                                              DVVideoContext *s, int chroma)
{
    LOCAL_ALIGNED_16(DCTELEM, blk, [64]);

    bi->area_q[0] = bi->area_q[1] = bi->area_q[2] = bi->area_q[3] = 0;
    bi->partial_bit_count = 0;
    bi->partial_bit_buffer = 0;
    bi->cur_ac = 0;

    if (data) {
        if (DV_PROFILE_IS_HD(s->sys)) {
            s->dsp.get_pixels(blk, data, linesize << bi->dct_mode);
            s->fdct[0](blk);
        } else {
            bi->dct_mode = dv_guess_dct_mode(s, data, linesize);
            s->dsp.get_pixels(blk, data, linesize);
            s->fdct[bi->dct_mode](blk);
        }
    } else {
        /* We rely on the fact that encoding all zeros leads to an immediate EOB,
           which is precisely what the spec calls for in the "dummy" blocks. */
        memset(blk, 0, 64*sizeof(*blk));
        bi->dct_mode = 0;
    }

    if (DV_PROFILE_IS_HD(s->sys)) {
        const int *weights;
        if (s->sys->height == 1080) {
            weights = dv_weight_1080[chroma];
        } else { /* 720p */
            weights = dv_weight_720[chroma];
        }
        dv_set_class_number_hd(blk, bi,
                               ff_zigzag_direct,
                               weights,
                               dv100_min_bias+chroma*dv100_chroma_bias);
    } else {
        dv_set_class_number_sd(blk, bi,
                               bi->dct_mode ? ff_zigzag248_direct : ff_zigzag_direct,
                               bi->dct_mode ? dv_weight_248 : dv_weight_88,
                               chroma);
    }

    return bi->bit_size[0] + bi->bit_size[1] + bi->bit_size[2] + bi->bit_size[3];
}

/* DV100 quantize
   Perform quantization by divinding the AC component by the qstep.
   As an optimization we use a fixed-point integer multiply instead
   of a divide. */
static av_always_inline int dv100_quantize(int level, int qsinv)
{
    /* this code is equivalent to */
    /* return (level + qs/2) / qs; */

    return (level * qsinv + 1024 + (1<<(dv100_qstep_bits-1))) >> dv100_qstep_bits;

    /* the extra +1024 is needed to make the rounding come out right. */

    /* I (DJM) have verified that the results are exactly the same as
       division for level 0-2048 at all QNOs. */
}

static int dv100_actual_quantize(EncBlockInfo *b, int qlevel)
{
    int prev, k, qsinv;

    int qno = DV100_QLEVEL_QNO(dv100_qlevels[qlevel]);
    int cno = DV100_QLEVEL_CNO(dv100_qlevels[qlevel]);

    if (b->area_q[0] == qno && b->cno == cno)
        return b->bit_size[0];

    qsinv = dv100_qstep_inv[qno];

    /* record the new qstep */
    b->area_q[0] = qno;
    b->cno = cno;

    /* reset encoded size (EOB = 4 bits) */
    b->bit_size[0] = 4;

    /* visit nonzero components and quantize */
    prev = 0;
    for (k = 1; k < 64; k++) {
        /* quantize */
        int ac = dv100_quantize(b->save[k], qsinv) >> cno;
        if (ac) {
            if (ac > 255)
                ac = 255;
            b->mb[k] = ac;
            b->bit_size[0] += dv_rl2vlc_size(k - prev - 1, ac);
            b->next[prev] = k;
            prev = k;
        }
    }
    b->next[prev] = k;

    return b->bit_size[0];
}


static inline void dv_guess_qnos_hd(EncBlockInfo *blks, int *qnos)
{
    EncBlockInfo *b;
    int min_qlevel[5];
    int qlevels[5];
    int size[5];
    int i, j;

    static const int enable_finer = 1;

    /* cache block sizes at hypothetical qlevels */
    uint16_t size_cache[5*8][dv100_num_qlevels];

    memset(size_cache, 0, sizeof(size_cache));

    /* get minimum qlevels */
    for (i = 0; i < 5; i++) {
        min_qlevel[i] = 1;
        for (j = 0; j < 8; j++) {
            if (blks[8*i+j].min_qlevel > min_qlevel[i])
                min_qlevel[i] = blks[8*i+j].min_qlevel;
        }
    }

    /* initialize sizes */
    for (i = 0; i < 5; i++) {
        qlevels[i] = dv100_starting_qno;
        if (qlevels[i] < min_qlevel[i])
            qlevels[i] = min_qlevel[i];

        qnos[i] = DV100_QLEVEL_QNO(dv100_qlevels[qlevels[i]]);
        size[i] = 0;
        for (j = 0; j < 8; j++) {
            size_cache[8*i+j][qlevels[i]] = dv100_actual_quantize(&blks[8*i+j], qlevels[i]);
            size[i] += size_cache[8*i+j][qlevels[i]];
        }
    }

    /* must we go coarser? */
    if (size[0]+size[1]+size[2]+size[3]+size[4] > vs_total_ac_bits_hd) {
        int largest = size[0] % 5; /* 'random' number */

        do {
            /* find the macroblock with the lowest qlevel */
            for (i = 0; i < 5; i++) {
                if (qlevels[i] < dv100_num_qlevels-1 &&
                    qlevels[i] < qlevels[largest])
                    largest = i;
            }

            i = largest;

            /* ensure that we don't enter infinite loop */
            largest = (largest+1) % 5;

            if (qlevels[i] >= dv100_num_qlevels-1) {
                /* can't quantize any more */
                continue;
            }

            /* quantize a little bit more */
            qlevels[i] += dv100_qlevel_inc;
            if (qlevels[i] > dv100_num_qlevels-1)
                qlevels[i] = dv100_num_qlevels-1;

            qnos[i] = DV100_QLEVEL_QNO(dv100_qlevels[qlevels[i]]);
            size[i] = 0;

            /* for each block */
            b = &blks[8*i];
            for (j = 0; j < 8; j++, b++) {
                /* accumulate block size into macroblock */
                if(size_cache[8*i+j][qlevels[i]] == 0) {
                    /* it is safe to use actual_quantize() here because we only go from finer to coarser,
                       and it saves the final actual_quantize() down below */
                    size_cache[8*i+j][qlevels[i]] = dv100_actual_quantize(b, qlevels[i]);
                }
                size[i] += size_cache[8*i+j][qlevels[i]];
            } /* for each block */

        } while (vs_total_ac_bits_hd < size[0] + size[1] + size[2] + size[3] + size[4] &&
                 (qlevels[0] < dv100_num_qlevels-1 ||
                  qlevels[1] < dv100_num_qlevels-1 ||
                  qlevels[2] < dv100_num_qlevels-1 ||
                  qlevels[3] < dv100_num_qlevels-1 ||
                  qlevels[4] < dv100_num_qlevels-1));

        // can we go finer?
    } else if (enable_finer &&
               size[0]+size[1]+size[2]+size[3]+size[4] < vs_total_ac_bits_hd) {
        int save_qlevel;
        int largest = size[0] % 5; /* 'random' number */

        while (qlevels[0] > min_qlevel[0] ||
               qlevels[1] > min_qlevel[1] ||
               qlevels[2] > min_qlevel[2] ||
               qlevels[3] > min_qlevel[3] ||
               qlevels[4] > min_qlevel[4]) {

            /* find the macroblock with the highest qlevel */
            for (i = 0; i < 5; i++) {
                if (qlevels[i] > min_qlevel[i] && qlevels[i] > qlevels[largest])
                    largest = i;
            }

            i = largest;

            /* ensure that we don't enter infinite loop */
            largest = (largest+1) % 5;

            if (qlevels[i] <= min_qlevel[i]) {
                /* can't unquantize any more */
                continue;
            }

            /* quantize a little bit less */
            save_qlevel = qlevels[i];
            qlevels[i] -= dv100_qlevel_inc;
            if (qlevels[i] < min_qlevel[i])
                qlevels[i] = min_qlevel[i];

            qnos[i] = DV100_QLEVEL_QNO(dv100_qlevels[qlevels[i]]);

            size[i] = 0;

            /* for each block */
            b = &blks[8*i];
            for (j = 0; j < 8; j++, b++) {
                /* accumulate block size into macroblock */
                if(size_cache[8*i+j][qlevels[i]] == 0) {
                    size_cache[8*i+j][qlevels[i]] = dv100_actual_quantize(b, qlevels[i]);
                }
                size[i] += size_cache[8*i+j][qlevels[i]];
            } /* for each block */

            /* did we bust the limit? */
            if (vs_total_ac_bits_hd < size[0] + size[1] + size[2] + size[3] + size[4]) {
                /* go back down and exit */
                qlevels[i] = save_qlevel;
                qnos[i] = DV100_QLEVEL_QNO(dv100_qlevels[qlevels[i]]);
                break;
            }
        }
    }

    /* now do the actual quantization */
    for (i = 0; i < 5; i++) {
        /* for each block */
        b = &blks[8*i];
        size[i] = 0;
        for (j = 0; j < 8; j++, b++) {
            /* accumulate block size into macroblock */
            size[i] += dv100_actual_quantize(b, qlevels[i]);
        } /* for each block */
    }
}

static inline void dv_guess_qnos(EncBlockInfo *blks, int *qnos)
{
    int size[5];
    int i, j, k, a, prev, a2;
    EncBlockInfo *b;

    size[0] = size[1] = size[2] = size[3] = size[4] = 1 << 24;
    do {
        b = blks;
        for (i = 0; i < 5; i++) {
            if (!qnos[i])
                continue;

            qnos[i]--;
            size[i] = 0;
            for (j = 0; j < 6; j++, b++) {
                for (a = 0; a < 4; a++) {
                    if (b->area_q[a] != dv_quant_shifts[qnos[i] + dv_quant_offset[b->cno]][a]) {
                        b->bit_size[a] = 1; // 4 areas 4 bits for EOB :)
                        b->area_q[a]++;
                        prev = b->prev[a];
                        assert(b->next[prev] >= mb_area_start[a+1] || b->mb[prev]);
                        for (k = b->next[prev] ; k < mb_area_start[a+1]; k = b->next[k]) {
                            b->mb[k] >>= 1;
                            if (b->mb[k]) {
                                b->bit_size[a] += dv_rl2vlc_size(k - prev - 1, b->mb[k]);
                                prev = k;
                            } else {
                                if (b->next[k] >= mb_area_start[a+1] && b->next[k]<64){
                                    for (a2 = a + 1; b->next[k] >= mb_area_start[a2+1]; a2++)
                                        b->prev[a2] = prev;
                                    assert(a2 < 4);
                                    assert(b->mb[b->next[k]]);
                                    b->bit_size[a2] += dv_rl2vlc_size(b->next[k] - prev - 1, b->mb[b->next[k]])
                                        - dv_rl2vlc_size(b->next[k] - k - 1, b->mb[b->next[k]]);
                                    assert(b->prev[a2] == k && (a2 + 1 >= 4 || b->prev[a2+1] != k));
                                    b->prev[a2] = prev;
                                }
                                b->next[prev] = b->next[k];
                            }
                        }
                        b->prev[a+1] = prev;
                    }
                    size[i] += b->bit_size[a];
                }
            }
            if (vs_total_ac_bits >= size[0] + size[1] + size[2] + size[3] + size[4])
                return;
        }
    } while (qnos[0]|qnos[1]|qnos[2]|qnos[3]|qnos[4]);

    for (a = 2; a == 2 || vs_total_ac_bits < size[0]; a += a) {
        b = blks;
        size[0] = 5 * 6 * 4; //EOB
        for (j = 0; j < 6 * 5; j++, b++) {
            prev = b->prev[0];
            for (k = b->next[prev]; k < 64; k = b->next[k]) {
                if (b->mb[k] < a && b->mb[k] > -a) {
                    b->next[prev] = b->next[k];
                } else {
                    size[0] += dv_rl2vlc_size(k - prev - 1, b->mb[k]);
                    prev = k;
                }
            }
        }
    }
}

/* update all cno values into the blocks, over-writing the old values without
   touching anything else. (only used for DV100) */
static inline void dv_revise_cnos(uint8_t *dif, EncBlockInfo *blk, const DVprofile *profile)
{
    uint8_t *data;
    int mb_index, i;

    for (mb_index = 0; mb_index < 5; mb_index++) {
        data = dif + mb_index*80 + 4;
        for (i = 0; i < profile->bpm; i++) {
            /* zero out the class number */
            data[1] &= 0xCF;
            /* add the new one */
            data[1] |= blk[profile->bpm*mb_index+i].cno << 4;

            data += profile->block_sizes[i] >> 3;
        }
    }
}

static int dv_encode_video_segment(AVCodecContext *avctx, void *arg)
{
    DVVideoContext *s = avctx->priv_data;
    DVwork_chunk *work_chunk = arg;
    int mb_index, i, j;
    int mb_x, mb_y, c_offset, linesize, y_stride;
    uint8_t *y_ptr;
    uint8_t *dif, *p;
    LOCAL_ALIGNED_8(uint8_t, scratch, [64]);
    EncBlockInfo  enc_blks[5*DV_MAX_BPM];
    PutBitContext pbs[5*DV_MAX_BPM];
    PutBitContext *pb;
    EncBlockInfo *enc_blk;
    int vs_bit_size = 0;
    int qnos[5];
    int *qnosp = &qnos[0];

    p = dif = &s->buf[work_chunk->buf_offset*80];
    enc_blk = &enc_blks[0];
    for (mb_index = 0; mb_index < 5; mb_index++) {
        dv_calculate_mb_xy(s, work_chunk, mb_index, &mb_x, &mb_y);

        qnos[mb_index] = DV_PROFILE_IS_HD(s->sys) ? 1 : 15;

        y_ptr    = s->picture.data[0] + ((mb_y * s->picture.linesize[0] + mb_x) << 3);
        linesize = s->picture.linesize[0];

        if (s->sys->height == 1080 && mb_y < 134)
            enc_blk->dct_mode = dv_guess_dct_mode(s, y_ptr, linesize);
        else
            enc_blk->dct_mode = 0;
        for (i = 1; i < 8; i++)
            enc_blk[i].dct_mode = enc_blk->dct_mode;

        /* initializing luminance blocks */
        if ((s->sys->pix_fmt == PIX_FMT_YUV420P) ||
            (s->sys->pix_fmt == PIX_FMT_YUV411P && mb_x >= (704 / 8)) ||
            (s->sys->height >= 720 && mb_y != 134)) {
            y_stride = s->picture.linesize[0] << (3*!enc_blk->dct_mode);
        } else {
            y_stride = 16;
        }
        y_ptr    = s->picture.data[0] + ((mb_y * s->picture.linesize[0] + mb_x) << 3);
        linesize = s->picture.linesize[0];

        if (s->sys->video_stype == 4) { /* SD 422 */
            vs_bit_size +=
                dv_init_enc_block(enc_blk+0, y_ptr               , linesize, s, 0) +
                dv_init_enc_block(enc_blk+1, NULL                , linesize, s, 0) +
                dv_init_enc_block(enc_blk+2, y_ptr + 8           , linesize, s, 0) +
                dv_init_enc_block(enc_blk+3, NULL                , linesize, s, 0);
        } else {
            vs_bit_size +=
                dv_init_enc_block(enc_blk+0, y_ptr               , linesize, s, 0) +
                dv_init_enc_block(enc_blk+1, y_ptr + 8           , linesize, s, 0) +
                dv_init_enc_block(enc_blk+2, y_ptr     + y_stride, linesize, s, 0) +
                dv_init_enc_block(enc_blk+3, y_ptr + 8 + y_stride, linesize, s, 0);
        }
        enc_blk += 4;

        /* initializing chrominance blocks */
        c_offset = (((mb_y >>  (s->sys->pix_fmt == PIX_FMT_YUV420P)) * s->picture.linesize[1] +
                     (mb_x >> ((s->sys->pix_fmt == PIX_FMT_YUV411P) ? 2 : 1))) << 3);
        for (j = 2; j; j--) {
            uint8_t *c_ptr = s->picture.data[j] + c_offset;
            linesize = s->picture.linesize[j];
            y_stride = (mb_y == 134) ? 8 : (s->picture.linesize[j] << (3*!enc_blk->dct_mode));
            if (s->sys->pix_fmt == PIX_FMT_YUV411P && mb_x >= (704 / 8)) {
                uint8_t *d;
                uint8_t *b = scratch;
                for (i = 0; i < 8; i++) {
                    d = c_ptr + (linesize << 3);
                    b[0] = c_ptr[0]; b[1] = c_ptr[1]; b[2] = c_ptr[2]; b[3] = c_ptr[3];
                    b[4] =     d[0]; b[5] =     d[1]; b[6] =     d[2]; b[7] =     d[3];
                    c_ptr += linesize;
                    b += 8;
                }
                c_ptr = scratch;
                linesize = 8;
            }

            vs_bit_size += dv_init_enc_block(    enc_blk++, c_ptr           , linesize, s, 1);
            if (s->sys->bpm == 8) {
                vs_bit_size += dv_init_enc_block(enc_blk++, c_ptr + y_stride, linesize, s, 1);
            }
        }
    }

    if (DV_PROFILE_IS_HD(s->sys)) {
        /* unconditional */
        dv_guess_qnos_hd(&enc_blks[0], qnosp);
    } else if (vs_total_ac_bits < vs_bit_size) {
        dv_guess_qnos(&enc_blks[0], qnosp);
    }

    /* DIF encoding process */
    for (j = 0; j < 5*s->sys->bpm; ) {
        int start_mb = j;

        p[3] = *qnosp++;
        p += 4;

        /* First pass over individual cells only */
        for (i = 0; i < s->sys->bpm; i++, j++) {
            int sz = s->sys->block_sizes[i] >> 3;

            init_put_bits(&pbs[j], p, sz);
            put_sbits(&pbs[j], 9, ((enc_blks[j].mb[0] >> 3) - 1024 + 2) >> 2);
            put_bits(&pbs[j], 1, DV_PROFILE_IS_HD(s->sys) && i ? 1 : enc_blks[j].dct_mode);
            put_bits(&pbs[j], 2, enc_blks[j].cno);

            dv_encode_ac(&enc_blks[j], &pbs[j], &pbs[j+1]);
            p += sz;
        }

        /* Second pass over each MB space */
        pb = &pbs[start_mb];
        for (i = 0; i < s->sys->bpm; i++) {
            if (enc_blks[start_mb+i].partial_bit_count)
                pb = dv_encode_ac(&enc_blks[start_mb+i], pb, &pbs[start_mb+s->sys->bpm]);
        }
    }

    /* Third and final pass over the whole video segment space */
    pb = &pbs[0];
    for (j = 0; j < 5*s->sys->bpm; j++) {
        if (enc_blks[j].partial_bit_count)
            pb = dv_encode_ac(&enc_blks[j], pb, &pbs[s->sys->bpm*5]);
        if (enc_blks[j].partial_bit_count)
            av_log(avctx, AV_LOG_ERROR, "ac bitstream overflow\n");
    }

    for (j = 0; j < 5*s->sys->bpm; j++) {
        int pos;
        int size = pbs[j].size_in_bits >> 3;
        flush_put_bits(&pbs[j]);
        pos = put_bits_count(&pbs[j]) >> 3;
        if (pos > size) {
            av_log(avctx, AV_LOG_ERROR, "bitstream written beyond buffer size\n");
            return -1;
        }
        memset(pbs[j].buf + pos, 0xff, size - pos);
    }

    if (DV_PROFILE_IS_HD(s->sys))
        dv_revise_cnos(dif, enc_blks, s->sys);

    return 0;
}

static inline int dv_write_pack(enum dv_pack_type pack_id, DVVideoContext *c,
                                uint8_t *buf)
{
    /*
     * Here's what SMPTE314M says about these two:
     *    (page 6) APTn, AP1n, AP2n, AP3n: These data shall be identical
     *             as track application IDs (APTn = 001, AP1n =
     *             001, AP2n = 001, AP3n = 001), if the source signal
     *             comes from a digital VCR. If the signal source is
     *             unknown, all bits for these data shall be set to 1.
     *    (page 12) STYPE: STYPE defines a signal type of video signal
     *                     00000b = 4:1:1 compression
     *                     00100b = 4:2:2 compression
     *                     XXXXXX = Reserved
     * Now, I've got two problems with these statements:
     *   1. it looks like APT == 111b should be a safe bet, but it isn't.
     *      It seems that for PAL as defined in IEC 61834 we have to set
     *      APT to 000 and for SMPTE314M to 001.
     *   2. It is not at all clear what STYPE is used for 4:2:0 PAL
     *      compression scheme (if any).
     */
    int apt = (c->sys->pix_fmt == PIX_FMT_YUV420P ||
               (c->sys->pix_fmt == PIX_FMT_YUV411P && c->sys->height == 480)) ? 0 : 1;
    uint8_t aspect = 0;
    int il = c->avctx->height >= 720 || c->avctx->interlaced; // il reserved in 370m
    int fs = c->avctx->height == 720 || (c->avctx->interlaced == 1 + (c->avctx->height < 720));

    if (DV_PROFILE_IS_HD(c->sys) ||
        (int)(av_q2d(c->avctx->sample_aspect_ratio) *
              c->avctx->width / c->avctx->height * 10) >= 17)
        /* HD formats are always 16:9 */
        aspect = 0x02;

    buf[0] = (uint8_t)pack_id;
    switch (pack_id) {
    case dv_header525: /* I can't imagine why these two weren't defined as real */
    case dv_header625: /* packs in SMPTE314M -- they definitely look like ones */
        buf[1] = 0xf8 |        /* reserved -- always 1 */
                 (apt & 0x07); /* APT: Track application ID */
        buf[2] = (0    << 7) | /* TF1: audio data is 0 - valid; 1 - invalid */
                 (0x0f << 3) | /* reserved -- always 1 */
                 (apt & 0x07); /* AP1: Audio application ID */
        buf[3] = (0    << 7) | /* TF2: video data is 0 - valid; 1 - invalid */
                 (0x0f << 3) | /* reserved -- always 1 */
                 (apt & 0x07); /* AP2: Video application ID */
        buf[4] = (0    << 7) | /* TF3: subcode(SSYB) is 0 - valid; 1 - invalid */
                 (0x0f << 3) | /* reserved -- always 1 */
                 (apt & 0x07); /* AP3: Subcode application ID */
        break;
    case dv_video_source:
        buf[1] = 0xff;      /* reserved -- always 1 */
        buf[2] = (1 << 7) | /* B/W: 0 - b/w, 1 - color */
                 (1 << 6) | /* following CLF is valid - 0, invalid - 1 */
                 (3 << 4) | /* CLF: color frames ID (see ITU-R BT.470-4) */
                 0xf;       /* reserved -- always 1 */
        buf[3] = (3 << 6) | /* reserved -- always 1 */
                 (c->sys->dsf << 5) | /* system: 60fields/50fields */
                 c->sys->video_stype; /* signal type video compression */
        buf[4] = 0xff;      /* VISC: 0xff -- no information */
        break;
    case dv_video_control:
        buf[1] = (0 << 6) | /* Copy generation management (CGMS) 0 -- free */
                 0x3f;      /* reserved -- always 1 */
        buf[2] = 0xc8 |     /* reserved -- always b11001xxx */
            aspect;
        buf[3] = (1  << 7) | /* frame/field flag 1 -- frame, 0 -- field */
                 (fs << 6) | /* first/second field flag 0 -- field 2, 1 -- field 1 */
                 (1  << 5) | /* frame change flag 0 -- same picture as before, 1 -- different */
                 (il << 4) | /* 1 - interlaced, 0 - noninterlaced */
                 0xc;       /* reserved -- always b1100 */
        buf[4] = 0xff;      /* reserved -- always 1 */
        break;
    default:
        buf[1] = buf[2] = buf[3] = buf[4] = 0xff;
    }
    return 5;
}

static void dv_format_frame(DVVideoContext* c, uint8_t* buf)
{
    int chan, i, j, k;
     /* We work with 720p frames split in half. The odd half-frame is chan 2,3 */
    int chan_offset = 2*(c->sys->height == 720 && c->avctx->frame_number & 1);

    for (chan = 0; chan < c->sys->n_difchan; chan++) {
        for (i = 0; i < c->sys->difseg_size; i++) {
            memset(buf, 0xff, 80 * 6); /* first 6 DIF blocks are for control data */

            /* DV header: 1DIF */
            buf += dv_write_dif_id(dv_sect_header, chan+chan_offset, i, 0, buf);
            buf += dv_write_pack((c->sys->dsf ? dv_header625 : dv_header525), c, buf);
            buf += 72; /* unused bytes */

            /* DV subcode: 2DIFs */
            for (j = 0; j < 2; j++) {
                buf += dv_write_dif_id(dv_sect_subcode, chan+chan_offset, i, j, buf);
                for (k = 0; k < 6; k++)
                    buf += dv_write_ssyb_id(k, (i < c->sys->difseg_size/2), buf) + 5;
                buf += 29; /* unused bytes */
            }

            /* DV VAUX: 3DIFS */
            for (j = 0; j < 3; j++) {
                buf += dv_write_dif_id(dv_sect_vaux, chan+chan_offset, i, j, buf);
                if ((i & 1) && j == 0) {
                    buf += dv_write_pack(dv_video_source,  c, buf);
                    buf += dv_write_pack(dv_video_control, c, buf);
                } else {
                    buf += 2*5;
                }
                buf += 7*5;
                if (!(i & 1) && j == 2) {
                    buf += dv_write_pack(dv_video_source,  c, buf);
                    buf += dv_write_pack(dv_video_control, c, buf);
                } else {
                    buf += 2*5;
                }
                buf += 4*5 + 2; /* unused bytes */
            }

            /* DV Audio/Video: 135 Video DIFs + 9 Audio DIFs */
            for (j = 0; j < 135; j++) {
                if (j%15 == 0) {
                    memset(buf, 0xff, 80);
                    buf += dv_write_dif_id(dv_sect_audio, chan+chan_offset, i, j/15, buf);
                    buf += 77; /* audio control & shuffled PCM audio */
                }
                buf += dv_write_dif_id(dv_sect_video, chan+chan_offset, i, j, buf);
                buf += 77; /* 1 video macroblock: 1 bytes control
                              4 * 14 bytes Y 8x8 data
                              10 bytes Cr 8x8 data
                              10 bytes Cb 8x8 data */
            }
        }
    }
}

static int dvvideo_encode_frame(AVCodecContext *c, uint8_t *buf, int buf_size,
                                void *data)
{
    DVVideoContext *s = c->priv_data;

    if (!s->sys || buf_size < s->sys->frame_size)
        return -1;

    s->picture           = *((AVFrame *)data);
    s->picture.key_frame = 1;
    s->picture.pict_type = AV_PICTURE_TYPE_I;

    s->buf = buf;

    dv_format_frame(s, buf);

    c->execute(c, dv_encode_video_segment, s->sys->work_chunks, NULL,
               dv_work_pool_size(s->sys), sizeof(DVwork_chunk));

    emms_c();

    return s->sys->frame_size;
}

AVCodec ff_dvvideo_encoder = {
    "dvvideo",
    AVMEDIA_TYPE_VIDEO,
    CODEC_ID_DVVIDEO,
    sizeof(DVVideoContext),
    dvvideo_init_encoder,
    dvvideo_encode_frame,
    .capabilities = CODEC_CAP_SLICE_THREADS,
    .pix_fmts  = (const enum PixelFormat[]) {PIX_FMT_YUV411P, PIX_FMT_YUV422P, PIX_FMT_YUV420P, PIX_FMT_NONE},
    .long_name = NULL_IF_CONFIG_SMALL("DV (Digital Video)"),
};
