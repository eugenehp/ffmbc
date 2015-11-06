/*
 * Directshow capture interface
 * Copyright (c) 2010 Ramiro Polla
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

#include "dshow.h"

long ff_copy_dshow_media_type(AM_MEDIA_TYPE *dst, const AM_MEDIA_TYPE *src)
{
    uint8_t *pbFormat = NULL;

    if (src->cbFormat) {
        pbFormat = CoTaskMemAlloc(src->cbFormat);
        if (!pbFormat)
            return E_OUTOFMEMORY;
        memcpy(pbFormat, src->pbFormat, src->cbFormat);
    }

    *dst = *src;
    dst->pUnk = NULL;
    dst->pbFormat = pbFormat;

    return S_OK;
}

void ff_printGUID(const GUID *g)
{
#if DSHOWDEBUG
    const uint32_t *d = (const uint32_t *) &g->Data1;
    const uint16_t *w = (const uint16_t *) &g->Data2;
    const uint8_t  *c = (const uint8_t  *) &g->Data4;

    dshowdebug("0x%08x 0x%04x 0x%04x %02x%02x%02x%02x%02x%02x%02x%02x",
               d[0], w[0], w[1],
               c[0], c[1], c[2], c[3], c[4], c[5], c[6], c[7]);
#endif
}

static const char *dshow_context_to_name(void *ptr)
{
    return "dshow";
}
static const AVClass ff_dshow_context_class = { "DirectShow", dshow_context_to_name };
const AVClass *ff_dshow_context_class_ptr = &ff_dshow_context_class;

#define dstruct(pctx, sname, var, type) \
    dshowdebug("      "#var":\t%"type"\n", sname->var)

#if DSHOWDEBUG
static void dump_bih(void *s, BITMAPINFOHEADER *bih)
{
    dshowdebug("      BITMAPINFOHEADER\n");
    dstruct(s, bih, biSize, "lu");
    dstruct(s, bih, biWidth, "ld");
    dstruct(s, bih, biHeight, "ld");
    dstruct(s, bih, biPlanes, "d");
    dstruct(s, bih, biBitCount, "d");
    dstruct(s, bih, biCompression, "lu");
    dshowdebug("      biCompression:\t\"%.4s\"\n",
                   (char*) &bih->biCompression);
    dstruct(s, bih, biSizeImage, "lu");
    dstruct(s, bih, biXPelsPerMeter, "lu");
    dstruct(s, bih, biYPelsPerMeter, "lu");
    dstruct(s, bih, biClrUsed, "lu");
    dstruct(s, bih, biClrImportant, "lu");
}
#endif

void ff_print_AM_MEDIA_TYPE(const AM_MEDIA_TYPE *type)
{
#if DSHOWDEBUG
    dshowdebug("    majortype\t");
    ff_printGUID(&type->majortype);
    dshowdebug("\n");
    dshowdebug("    subtype\t");
    ff_printGUID(&type->subtype);
    dshowdebug("\n");
    dshowdebug("    bFixedSizeSamples\t%d\n", type->bFixedSizeSamples);
    dshowdebug("    bTemporalCompression\t%d\n", type->bTemporalCompression);
    dshowdebug("    lSampleSize\t%lu\n", type->lSampleSize);
    dshowdebug("    formattype\t");
    ff_printGUID(&type->formattype);
    dshowdebug("\n");
    dshowdebug("    pUnk\t%p\n", type->pUnk);
    dshowdebug("    cbFormat\t%lu\n", type->cbFormat);
    dshowdebug("    pbFormat\t%p\n", type->pbFormat);

    if (IsEqualGUID(&type->formattype, &FORMAT_VideoInfo)) {
        VIDEOINFOHEADER *v = (void *) type->pbFormat;
        dshowdebug("      rcSource: left %ld top %ld right %ld bottom %ld\n",
                   v->rcSource.left, v->rcSource.top, v->rcSource.right, v->rcSource.bottom);
        dshowdebug("      rcTarget: left %ld top %ld right %ld bottom %ld\n",
                   v->rcTarget.left, v->rcTarget.top, v->rcTarget.right, v->rcTarget.bottom);
        dshowdebug("      dwBitRate: %lu\n", v->dwBitRate);
        dshowdebug("      dwBitErrorRate: %lu\n", v->dwBitErrorRate);
        dshowdebug("      AvgTimePerFrame: %"PRId64"\n", v->AvgTimePerFrame);
        dump_bih(NULL, &v->bmiHeader);
    } else if (IsEqualGUID(&type->formattype, &FORMAT_VideoInfo2)) {
        VIDEOINFOHEADER2 *v = (void *) type->pbFormat;
        dshowdebug("      rcSource: left %ld top %ld right %ld bottom %ld\n",
                   v->rcSource.left, v->rcSource.top, v->rcSource.right, v->rcSource.bottom);
        dshowdebug("      rcTarget: left %ld top %ld right %ld bottom %ld\n",
                   v->rcTarget.left, v->rcTarget.top, v->rcTarget.right, v->rcTarget.bottom);
        dshowdebug("      dwBitRate: %lu\n", v->dwBitRate);
        dshowdebug("      dwBitErrorRate: %lu\n", v->dwBitErrorRate);
        dshowdebug("      AvgTimePerFrame: %"PRId64"\n", v->AvgTimePerFrame);
        dshowdebug("      dwInterlaceFlags: %lu\n", v->dwInterlaceFlags);
        dshowdebug("      dwCopyProtectFlags: %lu\n", v->dwCopyProtectFlags);
        dshowdebug("      dwPictAspectRatioX: %lu\n", v->dwPictAspectRatioX);
        dshowdebug("      dwPictAspectRatioY: %lu\n", v->dwPictAspectRatioY);
//        dshowdebug("      dwReserved1: %lu\n", v->u.dwReserved1); /* mingw-w64 is buggy and doesn't name unnamed unions */
        dshowdebug("      dwReserved2: %lu\n", v->dwReserved2);
        dump_bih(NULL, &v->bmiHeader);
    } else if (IsEqualGUID(&type->formattype, &FORMAT_WaveFormatEx)) {
        WAVEFORMATEX *fx = (void *) type->pbFormat;
        dshowdebug("      wFormatTag: %u\n", fx->wFormatTag);
        dshowdebug("      nChannels: %u\n", fx->nChannels);
        dshowdebug("      nSamplesPerSec: %lu\n", fx->nSamplesPerSec);
        dshowdebug("      nAvgBytesPerSec: %lu\n", fx->nAvgBytesPerSec);
        dshowdebug("      nBlockAlign: %u\n", fx->nBlockAlign);
        dshowdebug("      wBitsPerSample: %u\n", fx->wBitsPerSample);
        dshowdebug("      cbSize: %u\n", fx->cbSize);
    }
#endif
}
