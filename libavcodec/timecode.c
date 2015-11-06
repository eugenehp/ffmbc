/*
 * Timecode helper functions
 * copyright (c) 2006 Baptiste Coudurier <baptiste.coudurier@free.fr>
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

#include <stdlib.h>
#include <stdio.h>
#include "timecode.h"

int ff_framenum_to_drop_timecode(int frame_num, int fps)
{
    /* only works for NTSC 29.97 and HD 59.94 */
    int factor = fps/30;
    int d = frame_num / (17982*factor);
    int m = frame_num % (17982*factor);

    if (fps != 30 && fps != 60)
        return -3;
    //if (m < 2) m += 2; /* not needed since -2,-1 / 1798 in C returns 0 */
    return frame_num + 18*factor * d + 2*factor * ((m - 2) / (1798*factor));
}

int ff_framenum_to_timecode(char timecode[16], int frame_num, int drop, int fps)
{
    int hours, mins, secs, frames, neg = 0;

    timecode[0] = 0;

    if (fps != 24 && fps != 25 && fps != 30 && fps != 50 && fps != 60)
        return -2;
    if (drop && fps != 30 && fps != 60)
        return -3;

    if (frame_num < 0) {
        neg = 1;
        frame_num = -frame_num;
    }

    if (drop)
        frame_num = ff_framenum_to_drop_timecode(frame_num, fps);

    frames = frame_num % fps;
    secs = (frame_num / fps) % 60;
    mins = (frame_num / (60*fps)) % 60;
    hours = frame_num / (3600*fps);
    snprintf(timecode, 16, "%s%02d:%02d:%02d%c%02d",
             neg ? "-" : "", hours, mins, secs, drop ? ';' : ':', frames);
    return 0;
}

int ff_timecode_to_framenum(const char *timecode, AVRational tb, int *drop)
{
    int hours, mins, secs, frames, fps, framenum;
    char *p = timecode;

    fps = (tb.den + tb.num/2) / tb.num;
    switch (fps) {
    case 24:
    case 25:
    case 30:
    case 50:
    case 60:
        break;
    default:
        return -2;
    }

    hours  = strtol(p,     &p, 10);
    if (*p != ':')
        return -1;
    mins   = strtol(p + 1, &p, 10);
    if (*p != ':')
        return -1;
    secs   = strtol(p + 1, &p, 10);
    if (*p == ';')
        *drop = 1;
    else if (*p == ':')
        *drop = 0;
    else
        return -1;
    frames = strtol(p + 1, &p, 10);

    framenum = (hours * 3600 + mins * 60 + secs) * fps + frames;
    if (*drop) { /* adjust */
        int tmins = 60 * hours + mins;
        int factor = fps/30;
        if (fps != 30 && fps != 60)
            return -3;
        framenum -= 2*factor * (tmins - tmins / 10);
    }
    return framenum;
}
