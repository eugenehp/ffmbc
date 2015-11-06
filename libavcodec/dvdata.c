/*
 * Constants for DV codec
 * Copyright (c) 2002 Fabrice Bellard
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
 * Constants for DV codec.
 */

#include "libavutil/rational.h"
#include "avcodec.h"
#include "dvdata.h"
#include "simple_idct.h"
#include "dv_vlc_data.h"

void ff_dv_init_vlc(RL_VLC_ELEM dv_rl_vlc[1184])
{
    VLC dv_vlc;
    uint16_t new_dv_vlc_bits[NB_DV_VLC*2];
    uint8_t  new_dv_vlc_len[NB_DV_VLC*2];
    uint8_t  new_dv_vlc_run[NB_DV_VLC*2];
    int16_t  new_dv_vlc_level[NB_DV_VLC*2];
    int i, j;

    /* it's faster to include sign bit in a generic VLC parsing scheme */
    for (i = 0, j = 0; i < NB_DV_VLC; i++, j++) {
        new_dv_vlc_bits[j]  = dv_vlc_bits[i];
        new_dv_vlc_len[j]   = dv_vlc_len[i];
        new_dv_vlc_run[j]   = dv_vlc_run[i];
        new_dv_vlc_level[j] = dv_vlc_level[i];

        if (dv_vlc_level[i]) {
            new_dv_vlc_bits[j] <<= 1;
            new_dv_vlc_len[j]++;

            j++;
            new_dv_vlc_bits[j]  = (dv_vlc_bits[i] << 1) | 1;
            new_dv_vlc_len[j]   =  dv_vlc_len[i] + 1;
            new_dv_vlc_run[j]   =  dv_vlc_run[i];
            new_dv_vlc_level[j] = -dv_vlc_level[i];
        }
    }

    /* NOTE: as a trick, we use the fact the no codes are unused
       to accelerate the parsing of partial codes */
    init_vlc(&dv_vlc, TEX_VLC_BITS, j,
             new_dv_vlc_len, 1, 1, new_dv_vlc_bits, 2, 2, 0);
    assert(dv_vlc.table_size == 1184);

    for (i = 0; i < dv_vlc.table_size; i++){
        int code = dv_vlc.table[i][0];
        int len  = dv_vlc.table[i][1];
        int level, run;

        if (len < 0){ //more bits needed
            run   = 0;
            level = code;
        } else {
            run   = new_dv_vlc_run  [code] + 1;
            level = new_dv_vlc_level[code];
        }
        dv_rl_vlc[i].len   = len;
        dv_rl_vlc[i].level = level;
        dv_rl_vlc[i].run   = run;
    }
    free_vlc(&dv_vlc);
}


/* VLC encoding lookup table */
dv_vlc_pair dv_vlc_map[DV_VLC_MAP_RUN_SIZE][DV_VLC_MAP_LEV_SIZE];

#if CONFIG_HARDCODED_TABLES
#define ff_dv_vlc_map_tableinit()
#include "libavcodec/dv_tables.h"
#else
void ff_dv_vlc_map_tableinit(void)
{
    int i, j;
    for (i = 0; i < NB_DV_VLC - 1; i++) {
        if (dv_vlc_run[i] >= DV_VLC_MAP_RUN_SIZE)
            continue;
#if CONFIG_SMALL
        if (dv_vlc_level[i] >= DV_VLC_MAP_LEV_SIZE)
            continue;
#endif

        if (dv_vlc_map[dv_vlc_run[i]][dv_vlc_level[i]].size != 0)
            continue;

        dv_vlc_map[dv_vlc_run[i]][dv_vlc_level[i]].vlc  =
            dv_vlc_bits[i] << (!!dv_vlc_level[i]);
        dv_vlc_map[dv_vlc_run[i]][dv_vlc_level[i]].size =
            dv_vlc_len[i] + (!!dv_vlc_level[i]);
    }
    for (i = 0; i < DV_VLC_MAP_RUN_SIZE; i++) {
#if CONFIG_SMALL
        for (j = 1; j < DV_VLC_MAP_LEV_SIZE; j++) {
            if (dv_vlc_map[i][j].size == 0) {
                dv_vlc_map[i][j].vlc = dv_vlc_map[0][j].vlc |
                    (dv_vlc_map[i-1][0].vlc << (dv_vlc_map[0][j].size));
                dv_vlc_map[i][j].size = dv_vlc_map[i-1][0].size +
                    dv_vlc_map[0][j].size;
            }
        }
#else
        for (j = 1; j < DV_VLC_MAP_LEV_SIZE/2; j++) {
            if (dv_vlc_map[i][j].size == 0) {
                dv_vlc_map[i][j].vlc = dv_vlc_map[0][j].vlc |
                    (dv_vlc_map[i-1][0].vlc << (dv_vlc_map[0][j].size));
                dv_vlc_map[i][j].size = dv_vlc_map[i-1][0].size +
                    dv_vlc_map[0][j].size;
            }
            dv_vlc_map[i][((uint16_t)(-j))&0x1ff].vlc =
                dv_vlc_map[i][j].vlc | 1;
            dv_vlc_map[i][((uint16_t)(-j))&0x1ff].size =
                dv_vlc_map[i][j].size;
        }
#endif
    }
}
#endif /* CONFIG_HARDCODED_TABLES */

static DVwork_chunk work_chunks_dv25pal   [1*12*27];
static DVwork_chunk work_chunks_dv25pal411[1*12*27];
static DVwork_chunk work_chunks_dv25ntsc  [1*10*27];
static DVwork_chunk work_chunks_dv50pal   [2*12*27];
static DVwork_chunk work_chunks_dv50ntsc  [2*10*27];
static DVwork_chunk work_chunks_dv100palp [2*12*27];
static DVwork_chunk work_chunks_dv100ntscp[2*10*27];
static DVwork_chunk work_chunks_dv100pali [4*12*27];
static DVwork_chunk work_chunks_dv100ntsci[4*10*27];

static uint32_t dv_idct_factor_sd    [2*2*22*64];
static uint32_t dv_idct_factor_hd1080[2*4*16*64];
static uint32_t dv_idct_factor_hd720 [2*4*16*64];

static const DVprofile dv_profiles[] = {
    { .dsf = 0,
      .video_stype = 0x0,
      .frame_size = 120000,        /* IEC 61834, SMPTE-314M - 525/60 (NTSC) */
      .difseg_size = 10,
      .n_difchan = 1,
      .time_base = { 1001, 30000 },
      .ltc_divisor = 30,
      .height = 480,
      .width = 720,
      .sar = {{8, 9}, {32, 27}},
      .work_chunks = &work_chunks_dv25ntsc[0],
      .idct_factor = &dv_idct_factor_sd[0],
      .pix_fmt = PIX_FMT_YUV411P,
      .bpm = 6,
      .block_sizes = block_sizes_dv2550,
      .audio_stride = 90,
      .audio_min_samples  = { 1580, 1452, 1053 }, /* for 48, 44.1 and 32kHz */
      .audio_samples_dist = { 1600, 1602, 1602, 1602, 1602 }, /* per SMPTE-314M */
      .audio_shuffle = dv_audio_shuffle525,
    },
    { .dsf = 1,
      .video_stype = 0x0,
      .frame_size = 144000,        /* SMPTE-314M - 625/50 (PAL) */
      .difseg_size = 12,
      .n_difchan = 1,
      .time_base = { 1, 25 },
      .ltc_divisor = 25,
      .height = 576,
      .width = 720,
      .sar = {{16, 15}, {64, 45}},
      .work_chunks = &work_chunks_dv25pal411[0],
      .idct_factor = &dv_idct_factor_sd[0],
      .pix_fmt = PIX_FMT_YUV411P,
      .bpm = 6,
      .block_sizes = block_sizes_dv2550,
      .audio_stride = 108,
      .audio_min_samples  = { 1896, 1742, 1264 }, /* for 48, 44.1 and 32kHz */
      .audio_samples_dist = { 1920, 1920, 1920, 1920, 1920 },
      .audio_shuffle = dv_audio_shuffle625,
    },
    { .dsf = 0,
      .video_stype = 0x4,
      .frame_size = 240000,        /* SMPTE-314M - 525/60 (NTSC) 50 Mbps */
      .difseg_size = 10,           /* also known as "DVCPRO50" */
      .n_difchan = 2,
      .time_base = { 1001, 30000 },
      .ltc_divisor = 30,
      .height = 480,
      .width = 720,
      .sar = {{8, 9}, {32, 27}},
      .work_chunks = &work_chunks_dv50ntsc[0],
      .idct_factor = &dv_idct_factor_sd[0],
      .pix_fmt = PIX_FMT_YUV422P,
      .bpm = 6,
      .block_sizes = block_sizes_dv2550,
      .audio_stride = 90,
      .audio_min_samples  = { 1580, 1452, 1053 }, /* for 48, 44.1 and 32kHz */
      .audio_samples_dist = { 1600, 1602, 1602, 1602, 1602 }, /* per SMPTE-314M */
      .audio_shuffle = dv_audio_shuffle525,
    },
    { .dsf = 1,
      .video_stype = 0x4,
      .frame_size = 288000,        /* SMPTE-314M - 625/50 (PAL) 50 Mbps */
      .difseg_size = 12,           /* also known as "DVCPRO50" */
      .n_difchan = 2,
      .time_base = { 1, 25 },
      .ltc_divisor = 25,
      .height = 576,
      .width = 720,
      .sar = {{16, 15}, {64, 45}},
      .work_chunks = &work_chunks_dv50pal[0],
      .idct_factor = &dv_idct_factor_sd[0],
      .pix_fmt = PIX_FMT_YUV422P,
      .bpm = 6,
      .block_sizes = block_sizes_dv2550,
      .audio_stride = 108,
      .audio_min_samples  = { 1896, 1742, 1264 }, /* for 48, 44.1 and 32kHz */
      .audio_samples_dist = { 1920, 1920, 1920, 1920, 1920 },
      .audio_shuffle = dv_audio_shuffle625,
    },
    { .dsf = 0,
      .video_stype = 0x14,
      .frame_size = 480000,        /* SMPTE-370M - 1080i60 100 Mbps */
      .difseg_size = 10,           /* also known as "DVCPRO HD" */
      .n_difchan = 4,
      .time_base = { 1001, 30000 },
      .ltc_divisor = 30,
      .height = 1080,
      .width = 1280,
      .sar = {{1, 1}, {3, 2}},
      .work_chunks = &work_chunks_dv100ntsci[0],
      .idct_factor = &dv_idct_factor_hd1080[0],
      .pix_fmt = PIX_FMT_YUV422P,
      .bpm = 8,
      .block_sizes = block_sizes_dv100,
      .audio_stride = 90,
      .audio_min_samples  = { 1580, 1452, 1053 }, /* for 48, 44.1 and 32kHz */
      .audio_samples_dist = { 1600, 1602, 1602, 1602, 1602 }, /* per SMPTE-314M */
      .audio_shuffle = dv_audio_shuffle525,
    },
    { .dsf = 1,
      .video_stype = 0x14,
      .frame_size = 576000,        /* SMPTE-370M - 1080i50 100 Mbps */
      .difseg_size = 12,           /* also known as "DVCPRO HD" */
      .n_difchan = 4,
      .time_base = { 1, 25 },
      .ltc_divisor = 25,
      .height = 1080,
      .width = 1440,
      .sar = {{1, 1}, {4, 3}},
      .work_chunks = &work_chunks_dv100pali[0],
      .idct_factor = &dv_idct_factor_hd1080[0],
      .pix_fmt = PIX_FMT_YUV422P,
      .bpm = 8,
      .block_sizes = block_sizes_dv100,
      .audio_stride = 108,
      .audio_min_samples  = { 1896, 1742, 1264 }, /* for 48, 44.1 and 32kHz */
      .audio_samples_dist = { 1920, 1920, 1920, 1920, 1920 },
      .audio_shuffle = dv_audio_shuffle625,
    },
    { .dsf = 0,
      .video_stype = 0x18,
      .frame_size = 240000,        /* SMPTE-370M - 720p60 100 Mbps */
      .difseg_size = 10,           /* also known as "DVCPRO HD" */
      .n_difchan = 2,
      .time_base = { 1001, 60000 },
      .ltc_divisor = 60,
      .height = 720,
      .width = 960,
      .sar = {{1, 1}, {4, 3}},
      .work_chunks = &work_chunks_dv100ntscp[0],
      .idct_factor = &dv_idct_factor_hd720[0],
      .pix_fmt = PIX_FMT_YUV422P,
      .bpm = 8,
      .block_sizes = block_sizes_dv100,
      .audio_stride = 90,
      .audio_min_samples  = { 1580, 1452, 1053 }, /* for 48, 44.1 and 32kHz */
      .audio_samples_dist = { 1600, 1602, 1602, 1602, 1602 }, /* per SMPTE-314M */
      .audio_shuffle = dv_audio_shuffle525,
    },
    { .dsf = 1,
      .video_stype = 0x18,
      .frame_size = 288000,        /* SMPTE-370M - 720p50 100 Mbps */
      .difseg_size = 12,           /* also known as "DVCPRO HD" */
      .n_difchan = 2,
      .time_base = { 1, 50 },
      .ltc_divisor = 50,
      .height = 720,
      .width = 960,
      .sar = {{1, 1}, {4, 3}},
      .work_chunks = &work_chunks_dv100palp[0],
      .idct_factor = &dv_idct_factor_hd720[0],
      .pix_fmt = PIX_FMT_YUV422P,
      .bpm = 8,
      .block_sizes = block_sizes_dv100,
      .audio_stride = 90,
      .audio_min_samples  = { 1896, 1742, 1264 }, /* for 48, 44.1 and 32kHz */
      .audio_samples_dist = { 1920, 1920, 1920, 1920, 1920 },
      .audio_shuffle = dv_audio_shuffle625,
    },
    { .dsf = 1,
      .video_stype = 0x0,
      .frame_size = 144000,        /* IEC 61834 - 625/50 (PAL) */
      .difseg_size = 12,
      .n_difchan = 1,
      .time_base = { 1, 25 },
      .ltc_divisor = 25,
      .height = 576,
      .width = 720,
      .sar = {{16, 15}, {64, 45}},
      .work_chunks = &work_chunks_dv25pal[0],
      .idct_factor = &dv_idct_factor_sd[0],
      .pix_fmt = PIX_FMT_YUV420P,
      .bpm = 6,
      .block_sizes = block_sizes_dv2550,
      .audio_stride = 108,
      .audio_min_samples  = { 1896, 1742, 1264 }, /* for 48, 44.1 and 32kHz */
      .audio_samples_dist = { 1920, 1920, 1920, 1920, 1920 },
      .audio_shuffle = dv_audio_shuffle625,
    }
};

/* See http://msdn.microsoft.com/en-us/library/dd407314(v=vs.85).aspx
   about fourcc DVSL/DVSD fourccs and stype */
const DVprofile* ff_dv_frame_profile(const DVprofile *sys,
                                  const uint8_t* frame, unsigned buf_size)
{
   int i;
   int dsf = (frame[3] & 0x80) >> 7;
   int stype = frame[80*5 + 48 + 3] & 0x1f;
   int apt = frame[4] & 0x7;
   int pal = (frame[80*5 + 48 + 3] >> 5) & 0x1;

   if (stype <= 1) { // DV25
       if (pal && apt) // SMPTE-314M
           return &dv_profiles[1];
       else if (pal) // IEC 61834
           return &dv_profiles[8];
       else
           return &dv_profiles[0];
   }

   for (i=0; i<FF_ARRAY_ELEMS(dv_profiles); i++)
       if (dsf == dv_profiles[i].dsf && stype == dv_profiles[i].video_stype)
           return &dv_profiles[i];

   /* check if old sys matches and assumes corrupted input */
   if (sys && buf_size == sys->frame_size)
       return sys;

   return NULL;
}

const DVprofile* ff_dv_codec_profile(AVCodecContext* codec)
{
    int i;

    for (i=0; i<FF_ARRAY_ELEMS(dv_profiles); i++)
       if (codec->height  == dv_profiles[i].height  &&
           codec->pix_fmt == dv_profiles[i].pix_fmt &&
           codec->width   == dv_profiles[i].width) {
           if (codec->height == 720) // 720p special case
               if ((codec->time_base.den == 25 || codec->time_base.den == 50)
                   && codec->time_base.num == 1)
                   i++;
               return &dv_profiles[i];
       }

    return NULL;
}

static inline void dv_calc_mb_coordinates(const DVprofile *d, int chan, int seq, int slot,
                                          uint16_t *tbl)
{
    static const uint8_t off[] = { 2, 6, 8, 0, 4 };
    static const uint8_t shuf1[] = { 36, 18, 54, 0, 72 };
    static const uint8_t shuf2[] = { 24, 12, 36, 0, 48 };
    static const uint8_t shuf3[] = { 18, 9, 27, 0, 36 };

    static const uint8_t l_start[] = {0, 4, 9, 13, 18, 22, 27, 31, 36, 40};
    static const uint8_t l_start_shuffled[] = { 9, 4, 13, 0, 18 };

    static const uint8_t serpent1[] = {0, 1, 2, 2, 1, 0,
                                       0, 1, 2, 2, 1, 0,
                                       0, 1, 2, 2, 1, 0,
                                       0, 1, 2, 2, 1, 0,
                                       0, 1, 2};
    static const uint8_t serpent2[] = {0, 1, 2, 3, 4, 5, 5, 4, 3, 2, 1, 0,
                                       0, 1, 2, 3, 4, 5, 5, 4, 3, 2, 1, 0,
                                       0, 1, 2, 3, 4, 5};

    static const uint8_t remap[][2] = {{ 0, 0}, { 0, 0}, { 0, 0}, { 0, 0}, /* dummy */
                                       { 0, 0}, { 0, 1}, { 0, 2}, { 0, 3}, {10, 0},
                                       {10, 1}, {10, 2}, {10, 3}, {20, 0}, {20, 1},
                                       {20, 2}, {20, 3}, {30, 0}, {30, 1}, {30, 2},
                                       {30, 3}, {40, 0}, {40, 1}, {40, 2}, {40, 3},
                                       {50, 0}, {50, 1}, {50, 2}, {50, 3}, {60, 0},
                                       {60, 1}, {60, 2}, {60, 3}, {70, 0}, {70, 1},
                                       {70, 2}, {70, 3}, { 0,64}, { 0,65}, { 0,66},
                                       {10,64}, {10,65}, {10,66}, {20,64}, {20,65},
                                       {20,66}, {30,64}, {30,65}, {30,66}, {40,64},
                                       {40,65}, {40,66}, {50,64}, {50,65}, {50,66},
                                       {60,64}, {60,65}, {60,66}, {70,64}, {70,65},
                                       {70,66}, { 0,67}, {20,67}, {40,67}, {60,67}};

    int i, k, m;
    int x, y, blk;

    for (m = 0; m < 5; m++) {
        switch (d->width) {
        case 1440:
            blk = (chan*11+seq)*27+slot;

            if (chan == 0 && seq == 11) {
                x = m*27+slot;
                if (x<90) {
                    y = 0;
                } else {
                    x = (x - 90)*2;
                    y = 67;
                }
            } else {
                i = (4*chan + blk + off[m])%11;
                k = (blk/11)%27;

                x = shuf1[m] + (chan&1)*9 + k%9;
                y = (i*3+k/9)*2 + (chan>>1) + 1;
            }
            tbl[m] = (x<<1)|(y<<9);
            break;
        case 1280:
            blk = (chan*10+seq)*27+slot;

            i = (4*chan + (seq/5) + 2*blk + off[m])%10;
            k = (blk/5)%27;

            x = shuf1[m]+(chan&1)*9 + k%9;
            y = (i*3+k/9)*2 + (chan>>1) + 4;

            if (x >= 80) {
                x = remap[y][0]+((x-80)<<(y>59));
                y = remap[y][1];
            }
            tbl[m] = (x<<1)|(y<<9);
            break;
        case 960:
            blk = (chan*10+seq)*27+slot;

            i = (4*chan + (seq/5) + 2*blk + off[m])%10;
            k = (blk/5)%27 + (i&1)*3;

            x = shuf2[m] + k%6 + 6*(chan&1);
            y = l_start[i] + k/6 + 45*(chan>>1);
            tbl[m] = (x<<1)|(y<<9);
            break;
        case 720:
            switch (d->pix_fmt) {
            case PIX_FMT_YUV422P:
                x = shuf3[m] + slot/3;
                y = serpent1[slot] +
                    ((((seq + off[m]) % d->difseg_size)<<1) + chan)*3;
                tbl[m] = (x<<1)|(y<<8);
                break;
            case PIX_FMT_YUV420P:
                x = shuf3[m] + slot/3;
                y = serpent1[slot] +
                    ((seq + off[m]) % d->difseg_size)*3;
                tbl[m] = (x<<1)|(y<<9);
                break;
            case PIX_FMT_YUV411P:
                i = (seq + off[m]) % d->difseg_size;
                k = slot + ((m==1||m==2)?3:0);

                x = l_start_shuffled[m] + k/6;
                y = serpent2[k] + i*6;
                if (x > 21)
                    y = y*2 - i*6;
                tbl[m] = (x<<2)|(y<<8);
                break;
            }
        default:
            break;
        }
    }
}

int ff_dv_init_dynamic_tables(const DVprofile *d)
{
    int j,i,c,s,p;
    uint32_t *factor1, *factor2;
    const int *iweight1, *iweight2;

    if (!d->work_chunks[dv_work_pool_size(d)-1].buf_offset) {
        p = i = 0;
        for (c = 0; c < d->n_difchan; c++) {
            for (s = 0; s < d->difseg_size; s++) {
                p += 6;
                for (j = 0; j < 27; j++) {
                    p += !(j%3);
                    if (!(DV_PROFILE_IS_1080i50(d) && c != 0 && s == 11) &&
                        !(DV_PROFILE_IS_720p50(d) && s > 9)) {
                        dv_calc_mb_coordinates(d, c, s, j, &d->work_chunks[i].mb_coordinates[0]);
                        d->work_chunks[i++].buf_offset = p;
                    }
                    p += 5;
                }
            }
        }
    }

    if (!d->idct_factor[DV_PROFILE_IS_HD(d)?8191:5631]) {
        factor1 = &d->idct_factor[0];
        factor2 = &d->idct_factor[DV_PROFILE_IS_HD(d)?4096:2816];
        if (d->height == 720) {
            iweight1 = &dv_iweight_720_y[0];
            iweight2 = &dv_iweight_720_c[0];
        } else {
            iweight1 = &dv_iweight_1080_y[0];
            iweight2 = &dv_iweight_1080_c[0];
        }
        if (DV_PROFILE_IS_HD(d)) {
            for (c = 0; c < 4; c++) {
                for (s = 0; s < 16; s++) {
                    for (i = 0; i < 64; i++) {
                        *factor1++ = (dv100_qstep[s] << (c + 9)) * iweight1[i];
                        *factor2++ = (dv100_qstep[s] << (c + 9)) * iweight2[i];
                    }
                }
            }
        } else {
            iweight1 = &dv_iweight_88[0];
            for (j = 0; j < 2; j++, iweight1 = &dv_iweight_248[0]) {
                for (s = 0; s < 22; s++) {
                    for (i = c = 0; c < 4; c++) {
                        for (; i < dv_quant_areas[c]; i++) {
                            *factor1   = iweight1[i] << (dv_quant_shifts[s][c] + 1);
                            *factor2++ = (*factor1++) << 1;
                        }
                    }
                }
            }
        }
    }

    return 0;
}
