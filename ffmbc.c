/*
 * ffmpeg main
 * Copyright (c) 2000-2003 Fabrice Bellard
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

#include "config.h"
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <errno.h>
#include <signal.h>
#include <limits.h>
#include <unistd.h>
#include "libavformat/avformat.h"
#include "libavdevice/avdevice.h"
#include "libswscale/swscale.h"
#include "libavutil/opt.h"
#include "libavcodec/audioconvert.h"
#include "libavutil/audioconvert.h"
#include "libavutil/parseutils.h"
#include "libavutil/samplefmt.h"
#include "libavutil/colorspace.h"
#include "libavutil/fifo.h"
#include "libavutil/intreadwrite.h"
#include "libavutil/dict.h"
#include "libavutil/mathematics.h"
#include "libavutil/pixdesc.h"
#include "libavutil/avstring.h"
#include "libavutil/libm.h"
#include "libavformat/os_support.h"

#if CONFIG_AVFILTER
# include "libavfilter/avcodec.h"
# include "libavfilter/avfilter.h"
# include "libavfilter/avfiltergraph.h"
# include "libavfilter/vsink_buffer.h"
# include "libavfilter/vsrc_buffer.h"
#endif

#if HAVE_SYS_RESOURCE_H
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#elif HAVE_GETPROCESSTIMES
#include <windows.h>
#endif
#if HAVE_GETPROCESSMEMORYINFO
#include <windows.h>
#include <psapi.h>
#endif

#if HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#if HAVE_TERMIOS_H
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <termios.h>
#elif HAVE_KBHIT
#include <conio.h>
#endif
#include <time.h>

#include "cmdutils.h"

#include "libavutil/avassert.h"

const char program_name[] = "FFmbc";
const int program_birth_year = 2008;

/* select an input stream for an output stream */
typedef struct StreamMap {
    int file_index;
    int stream_index;
    int sync_file_index;
    int sync_stream_index;
} StreamMap;

/* select an audio channel to extract */
typedef struct AudioChannelMap {
    int file_index;
    int stream_index;
    int channel_index;
    int out_file_index;
    int out_stream_index;
    int out_channel_index;
} AudioChannelMap;

/**
 * select an input file for an output file
 */
typedef struct MetadataMap {
    int  file;      //< file index
    char type;      //< type of metadata to copy -- (g)lobal, (s)tream, (c)hapter or (p)rogram
    int  index;     //< stream/chapter/program number
} MetadataMap;

typedef struct ChapterMap {
    int in_file;
    int out_file;
} ChapterMap;

static const OptionDef options[];

#define MAX_FILES 100
#define MAX_STREAMS 1024    /* arbitrary sanity check value */
static const char *last_asked_format = NULL;
static double *ts_scale;
static int  nb_ts_scale;

static AVFormatContext *output_files[MAX_FILES];
static AVDictionary *output_opts[MAX_FILES];
static int nb_output_files = 0;

static StreamMap *stream_maps = NULL;
static int nb_stream_maps;

#define MAX_AUDIO_CHANNEL_MAPS 64
static AudioChannelMap audio_channel_maps[MAX_AUDIO_CHANNEL_MAPS];
static int nb_audio_channel_maps;

/* first item specifies output metadata, second is input */
static MetadataMap (*meta_data_maps)[2] = NULL;
static int nb_meta_data_maps;
static int metadata_streams_autocopy  = 0;
static int metadata_chapters_autocopy = 0;

static ChapterMap *chapter_maps = NULL;
static int nb_chapter_maps;

/* indexed by output file stream index */
static int *streamid_map = NULL;
static int nb_streamid_map = 0;

static int frame_width  = 0;
static int frame_height = 0;
static AVRational frame_aspect_ratio;
static enum PixelFormat frame_pix_fmt = PIX_FMT_NONE;
static int frame_bits_per_raw_sample = 0;
static enum AVSampleFormat audio_sample_fmt = AV_SAMPLE_FMT_NONE;
static int max_frames[4] = {INT_MAX, INT_MAX, INT_MAX, INT_MAX};
static AVRational frame_rate;
static float video_qscale = 0;
static uint16_t *intra_matrix = NULL;
static uint16_t *inter_matrix = NULL;
static const char *video_rc_override_string=NULL;
static int video_disable = 0;
static int video_discard = 0;
static char *video_codec_name = NULL;
static unsigned int video_codec_tag = 0;
static char *video_language = NULL;
static int same_quality = 0;
static int interlaced = 0;
static int intra_dc_precision = 8;
static int loop_input = 0;
static int loop_output = AVFMT_NOOUTPUTLOOP;
static int qp_hist = 0;
#if CONFIG_AVFILTER
static char *vfilters = NULL;
#endif

static int intra_only = 0;
static int audio_sample_rate = 0;
#define QSCALE_NONE -99999
static float audio_qscale = QSCALE_NONE;
static int audio_disable = 0;
static int audio_channels = 0;
static char  *audio_codec_name = NULL;
static unsigned int audio_codec_tag = 0;
static char *audio_language = NULL;

static int subtitle_disable = 0;
static char *subtitle_codec_name = NULL;
static char *subtitle_language = NULL;
static unsigned int subtitle_codec_tag = 0;

static int data_disable = 0;
static char *data_codec_name = NULL;
static unsigned int data_codec_tag = 0;

static float mux_preload= 0.5;
static float mux_max_delay= 0.7;

static int64_t recording_time = INT64_MAX;
static int64_t start_time = 0;
static int64_t input_ts_offset = 0;
static int file_overwrite = 0;
static AVDictionary *metadata;
static int do_benchmark = 0;
static int do_hex_dump = 0;
static int do_pkt_dump = 0;
static int do_psnr = 0;
static int do_pass = 0;
static const char *pass_logfilename_prefix;
static int audio_stream_copy = 0;
static int video_stream_copy = 0;
static int subtitle_stream_copy = 0;
static int data_stream_copy = 0;
static int video_sync_method= 1;
static int audio_sync_method= 1;
static float audio_drift_threshold= 0.1;
static int copy_ts= 0;
static int copy_tb= 0;
static int opt_shortest = 0;
static char *vstats_filename;
static FILE *vstats_file;
static int opt_programid = 0;
static int copy_initial_nonkeyframes = 0;

static int rate_emu = 0;

static const char *target;

static int update_interval = 250000;

static int audio_volume = 256;

static int exit_on_error = 0;
static int using_stdin = 0;
static int verbose = 1;
static int run_as_daemon  = 0;
static int thread_count= 1;
static int q_pressed = 0;
static int64_t video_size = 0;
static int64_t audio_size = 0;
static int64_t extra_size = 0;
static int nb_frames_dup = 0;
static int nb_frames_drop = 0;
static int input_sync;
static uint64_t limit_filesize = 0;
static int force_fps = 0;
static char *forced_key_frames = NULL;

static float dts_delta_threshold = 10;

static int64_t timer_start;

static uint8_t *input_tmp;
static uint8_t *audio_buf;
static uint8_t *audio_out;
static unsigned int allocated_audio_out_size, allocated_audio_buf_size, input_tmp_size;

static short *samples;

static AVBitStreamFilterContext *video_bitstream_filters=NULL;
static AVBitStreamFilterContext *audio_bitstream_filters=NULL;
static AVBitStreamFilterContext *subtitle_bitstream_filters=NULL;

#define DEFAULT_PASS_LOGFILENAME_PREFIX "ffmpeg2pass"

struct InputStream;

typedef struct {
    uint8_t *buf;
    unsigned buf_size;
    unsigned last_sample_pos; /* last valid sample pos in buf */
    unsigned buf_index[MAX_AUDIO_CHANNEL_MAPS];
    unsigned sample_size; /* size of one sample */
    unsigned out_channels;
} AudioMergeContext;

typedef struct OutputStream {
    int file_index;          /* file index */
    int index;               /* stream index in the output file */
    int source_index[MAX_AUDIO_CHANNEL_MAPS]; /* AVInputStream index */
    int nb_source_indexes;   /* number of source indexes */
    AVStream *st;            /* stream in the output file */
    int encoding_needed;     /* true if encoding needed for this stream */
    int frame_number;
    /* input pts and corresponding output pts
       for A/V sync */
    //double sync_ipts;        /* dts from the AVPacket of the demuxer in second units */
    struct InputStream *sync_ist; /* input stream to sync against */
    int is_start;            /* is 1 at the start and after a discontinuity */
    int64_t sync_opts;       /* output frame counter, could be changed to some true timestamp */ //FIXME look at frame_number
    AVFrame prev_frame;
    int free_prev_frame;
    AVBitStreamFilterContext *bitstream_filters;
    AVCodec *enc;

    /* video only */
    int video_resample;
    AVFrame resample_frame;              /* temporary frame for image resampling */
    struct SwsContext *img_resample_ctx; /* for image resampling */
    int resample_height;
    int resample_width;
    int resample_pix_fmt;
    AVRational frame_rate;
    int force_fps;
    AVRational frame_aspect_ratio;

    const char *target;

    /* forced key frames */
    int64_t *forced_kf_pts;
    int forced_kf_count;
    int forced_kf_index;

    /* audio only */
    AudioChannelMap *audio_channel_maps[MAX_AUDIO_CHANNEL_MAPS];
    int nb_audio_channel_maps;
    uint8_t *audio_merge_buf;
    AudioMergeContext audiomerge;

    int audio_resample;
    ReSampleContext *resample; /* for audio resampling */
    int resample_sample_fmt;
    int resample_channels;
    int resample_sample_rate;
    int reformat_pair;
    AVAudioConvert *reformat_ctx;
    AVFifoBuffer *fifo;     /* for compression: one audio fifo per codec */
    FILE *logfile;

#if CONFIG_AVFILTER
    AVFilterContext *output_video_filter;
    AVFilterContext *input_video_filter;
    AVFilterBufferRef *picref;
    AVFilterBufferRef *prev_picref;
    char *avfilter;
    AVFilterGraph *graph;
#endif

   int sws_flags;
   AVDictionary *opts;
} OutputStream;

static OutputStream **output_streams_for_file[MAX_FILES] = { NULL };
static int nb_output_streams_for_file[MAX_FILES] = { 0 };

typedef struct InputStream {
    int file_index;
    AVStream *st;
    int discard;             /* true if stream data should be discarded */
    int decoding_needed;     /* true if the packets must be decoded in 'raw_fifo' */
    AVCodec *dec;

    int64_t       start;     /* time when read started */
    int64_t       next_pts;  /* synthetic pts for cases where pkt.pts
                                is not defined */
    int64_t       pts;       /* current pts */
    double ts_scale;
    int64_t       dts;       /* current dts */
    int dts_is_reordered_pts;/* set this to 1 if format is avi/wmv/raw, ie format has no pts */
    int showed_multi_packet_warning;
    int is_past_recording_time;
    AVDictionary *opts;
    uint8_t *pkt_data_to_free;
    AVRational frame_rate;
} InputStream;

typedef struct InputFile {
    AVFormatContext *ctx;
    int eof_reached;      /* true if eof reached */
    int ist_index;        /* index of first stream in ist_table */
    int buffer_size;      /* current total buffer size */
    int64_t ts_offset;
} InputFile;

#if HAVE_TERMIOS_H

/* init terminal so that we can grab keys */
static struct termios oldtty;
#endif

static InputStream *input_streams = NULL;
static int         nb_input_streams = 0;
static InputFile   *input_files   = NULL;
static int         nb_input_files   = 0;

#if CONFIG_AVFILTER

static int configure_video_filters(InputStream *ist, OutputStream *ost)
{
    AVFilterContext *last_filter, *filter;
    /** filter graph containing all filters including input & output */
    AVCodecContext *codec = ost->st->codec;
    AVCodecContext *icodec = ist->st->codec;
    enum PixelFormat pix_fmts[] = { codec->pix_fmt, PIX_FMT_NONE };
    AVRational sample_aspect_ratio;
    AVDictionaryEntry *t;
    char args[255];
    int ret;

    ost->graph = avfilter_graph_alloc();

    if (ist->st->sample_aspect_ratio.num)
        sample_aspect_ratio = ist->st->sample_aspect_ratio;
    else if (ist->st->codec->sample_aspect_ratio.num)
        sample_aspect_ratio = ist->st->codec->sample_aspect_ratio;
    else
        sample_aspect_ratio = (AVRational){1,1};

    snprintf(args, 255, "%d:%d:%d:%d:%d:%d:%d", ist->st->codec->width,
             ist->st->codec->height, ist->st->codec->pix_fmt, 1, AV_TIME_BASE,
             sample_aspect_ratio.num, sample_aspect_ratio.den);

    ret = avfilter_graph_create_filter(&ost->input_video_filter, avfilter_get_by_name("buffer"),
                                       "src", args, NULL, ost->graph);
    if (ret < 0)
        return ret;
    ret = avfilter_graph_create_filter(&ost->output_video_filter, avfilter_get_by_name("buffersink"),
                                       "out", NULL, pix_fmts, ost->graph);
    if (ret < 0)
        return ret;
    last_filter = ost->input_video_filter;

    if (t = av_dict_get(ist->st->metadata, "rotate", NULL, 0)) {
        AVFilterContext *filter;
        snprintf(args, 255, "%d", atoi(t->value));
        av_log(NULL, AV_LOG_INFO, "Auto-inserting rotate filter of %d degrees\n",
                atoi(t->value));
        if ((ret = avfilter_open(&filter, avfilter_get_by_name("rotate"),
                                 "auto-rotate")) < 0)
            return ret;
        if ((ret = avfilter_init_filter(filter, args, NULL)) < 0)
            return ret;
        if ((ret = avfilter_link(last_filter, 0, filter, 0)) < 0)
            return ret;
        last_filter = filter;
        avfilter_graph_add_filter(ost->graph, last_filter);
    }

    if (codec->width  != icodec->width || codec->height != icodec->height) {
        snprintf(args, 255, "%d:%d:flags=0x%X",
                 codec->width,
                 codec->height,
                 ost->sws_flags);
        if ((ret = avfilter_graph_create_filter(&filter, avfilter_get_by_name("scale"),
                                                "auto-inserted scaler",
                                                args, NULL, ost->graph)) < 0)
            return ret;
        if ((ret = avfilter_link(last_filter, 0, filter, 0)) < 0)
            return ret;
        last_filter = filter;
    }

    snprintf(args, sizeof(args), "flags=0x%X", ost->sws_flags);
    ost->graph->scale_sws_opts = av_strdup(args);

    if (ost->avfilter) {
        AVFilterInOut *outputs = avfilter_inout_alloc();
        AVFilterInOut *inputs  = avfilter_inout_alloc();

        outputs->name    = av_strdup("in");
        outputs->filter_ctx = last_filter;
        outputs->pad_idx = 0;
        outputs->next    = NULL;

        inputs->name    = av_strdup("out");
        inputs->filter_ctx = ost->output_video_filter;
        inputs->pad_idx = 0;
        inputs->next    = NULL;

        if ((ret = avfilter_graph_parse(ost->graph, ost->avfilter, &inputs, &outputs, NULL)) < 0)
            return ret;
        av_freep(&ost->avfilter);
    } else {
        if ((ret = avfilter_link(last_filter, 0, ost->output_video_filter, 0)) < 0)
            return ret;
    }

    if ((ret = avfilter_graph_config(ost->graph)) < 0)
        return ret;

    codec->width  = ost->output_video_filter->inputs[0]->w;
    codec->height = ost->output_video_filter->inputs[0]->h;
    codec->sample_aspect_ratio = ost->st->sample_aspect_ratio =
        ost->frame_aspect_ratio.num ? // overriden by the -aspect cli option
        av_mul_q(ost->frame_aspect_ratio, (AVRational){ codec->height, codec->width }) :
        ost->output_video_filter->inputs[0]->sample_aspect_ratio;

    return 0;
}
#endif /* CONFIG_AVFILTER */

static void term_exit(void)
{
    av_log(NULL, AV_LOG_QUIET, "%s", "");
#if HAVE_TERMIOS_H
    if(!run_as_daemon)
        tcsetattr (0, TCSANOW, &oldtty);
#endif
}

static volatile int received_sigterm = 0;

static void
sigterm_handler(int sig)
{
    received_sigterm = sig;
    q_pressed++;
    if (q_pressed > 3)
        exit(1);
    term_exit();
}

static void term_init(void)
{
#if HAVE_TERMIOS_H
    if(!run_as_daemon){
    struct termios tty;

    tcgetattr (0, &tty);
    oldtty = tty;
    atexit(term_exit);

    tty.c_iflag &= ~(IGNBRK|BRKINT|PARMRK|ISTRIP
                          |INLCR|IGNCR|ICRNL|IXON);
    tty.c_oflag |= OPOST;
    tty.c_lflag &= ~(ECHO|ECHONL|ICANON|IEXTEN);
    tty.c_cflag &= ~(CSIZE|PARENB);
    tty.c_cflag |= CS8;
    tty.c_cc[VMIN] = 1;
    tty.c_cc[VTIME] = 0;

    tcsetattr (0, TCSANOW, &tty);
    signal(SIGQUIT, sigterm_handler); /* Quit (POSIX).  */
    }
#endif

    signal(SIGINT , sigterm_handler); /* Interrupt (ANSI).  */
    signal(SIGTERM, sigterm_handler); /* Termination (ANSI).  */
#ifdef SIGXCPU
    signal(SIGXCPU, sigterm_handler);
#endif
}

/* read a key without blocking */
static int read_key(void)
{
#if HAVE_TERMIOS_H
    int n = 1;
    unsigned char ch;
    struct timeval tv;
    fd_set rfds;

    if(run_as_daemon)
        return -1;

    FD_ZERO(&rfds);
    FD_SET(0, &rfds);
    tv.tv_sec = 0;
    tv.tv_usec = 0;
    n = select(1, &rfds, NULL, NULL, &tv);
    if (n > 0) {
        n = read(0, &ch, 1);
        if (n == 1)
            return ch;

        return n;
    }
#elif HAVE_KBHIT
    if(kbhit())
        return(getch());
#endif
    return -1;
}

static int decode_interrupt_cb(void)
{
    q_pressed += read_key() == 'q';
    return q_pressed > 1;
}

static int ffmpeg_exit(int ret)
{
    int i;

    /* close files */
    for(i=0;i<nb_output_files;i++) {
        AVFormatContext *s = output_files[i];
        if (!(s->oformat->flags & AVFMT_NOFILE) && s->pb)
            avio_close(s->pb);
        avformat_free_context(s);
        av_free(output_streams_for_file[i]);
        av_dict_free(&output_opts[i]);
    }
    for(i=0;i<nb_input_files;i++) {
        av_close_input_file(input_files[i].ctx);
    }
    for (i = 0; i < nb_input_streams; i++)
        av_dict_free(&input_streams[i].opts);

    av_free(intra_matrix);
    av_free(inter_matrix);

    if (vstats_file)
        fclose(vstats_file);
    av_free(vstats_filename);

    av_free(streamid_map);
    av_free(stream_maps);
    av_free(meta_data_maps);

    av_freep(&input_streams);
    av_freep(&input_files);

    av_free(video_codec_name);
    av_free(audio_codec_name);
    av_free(subtitle_codec_name);
    av_free(data_codec_name);

    uninit_opts();
    av_free(audio_buf);
    av_free(audio_out);
    av_free(input_tmp);
    allocated_audio_buf_size= allocated_audio_out_size= 0;
    av_free(samples);

#if CONFIG_AVFILTER
    avfilter_uninit();
#endif

    if (received_sigterm) {
        av_log(NULL, AV_LOG_INFO,
            "Received signal %d: terminating.\n",
            (int) received_sigterm);
        exit (255);
    }

    exit(ret); /* not all OS-es handle main() return value */
    return ret;
}

static void assert_avoptions(AVDictionary *m)
{
    AVDictionaryEntry *t;
    if ((t = av_dict_get(m, "", NULL, AV_DICT_IGNORE_SUFFIX))) {
        av_log(NULL, AV_LOG_ERROR, "Option %s not found.\n", t->key);
        ffmpeg_exit(1);
    }
}

static void assert_codec_experimental(AVCodecContext *c, int encoder)
{
    const char *codec_string = encoder ? "encoder" : "decoder";
    AVCodec *codec;
    if (c->codec->capabilities & CODEC_CAP_EXPERIMENTAL &&
        c->strict_std_compliance > FF_COMPLIANCE_EXPERIMENTAL) {
        av_log(NULL, AV_LOG_ERROR, "%s '%s' is experimental and might produce bad "
                "results.\nAdd '-strict experimental' if you want to use it.\n",
                codec_string, c->codec->name);
        codec = encoder ? avcodec_find_encoder(c->codec->id) : avcodec_find_decoder(c->codec->id);
        if (!(codec->capabilities & CODEC_CAP_EXPERIMENTAL))
            av_log(NULL, AV_LOG_ERROR, "Or use the non experimental %s '%s'.\n",
                   codec_string, codec->name);
        ffmpeg_exit(1);
    }
}

/* similar to ff_dynarray_add() and av_fast_realloc() */
static void *grow_array(void *array, int elem_size, int *size, int new_size)
{
    if (new_size >= INT_MAX / elem_size) {
        av_log(NULL, AV_LOG_ERROR, "Array too big.\n");
        ffmpeg_exit(1);
    }
    if (*size < new_size) {
        uint8_t *tmp = av_realloc(array, new_size*elem_size);
        if (!tmp) {
            av_log(NULL, AV_LOG_ERROR, "Could not alloc buffer.\n");
            ffmpeg_exit(1);
        }
        memset(tmp + *size*elem_size, 0, (new_size-*size) * elem_size);
        *size = new_size;
        return tmp;
    }
    return array;
}

static void choose_sample_fmt(AVStream *st, AVCodec *codec)
{
    if(codec && codec->sample_fmts){
        const enum AVSampleFormat *p= codec->sample_fmts;
        for(; *p!=-1; p++){
            if(*p == st->codec->sample_fmt)
                break;
        }
        if (*p == -1) {
            if((codec->capabilities & CODEC_CAP_LOSSLESS) && av_get_sample_fmt_name(st->codec->sample_fmt) > av_get_sample_fmt_name(codec->sample_fmts[0]))
                av_log(NULL, AV_LOG_ERROR, "Convertion will not be lossless'\n");
            av_log(NULL, AV_LOG_WARNING,
                   "Incompatible sample format '%s' for codec '%s', auto-selecting format '%s'\n",
                   av_get_sample_fmt_name(st->codec->sample_fmt),
                   codec->name,
                   av_get_sample_fmt_name(codec->sample_fmts[0]));
            st->codec->sample_fmt = codec->sample_fmts[0];
        }
    }
}

static void choose_sample_rate(AVStream *st, AVCodec *codec)
{
    if(codec && codec->supported_samplerates){
        const int *p= codec->supported_samplerates;
        int best=0;
        int best_dist=INT_MAX;
        for(; *p; p++){
            int dist= abs(st->codec->sample_rate - *p);
            if(dist < best_dist){
                best_dist= dist;
                best= *p;
            }
        }
        if(best_dist){
            av_log(st->codec, AV_LOG_WARNING, "Requested sampling rate unsupported using closest supported (%d)\n", best);
        }
        st->codec->sample_rate= best;
    }
}

static void choose_pixel_fmt(AVStream *st, AVCodec *codec)
{
    if(codec && codec->pix_fmts){
        const enum PixelFormat *p= codec->pix_fmts;
        enum PixelFormat pix_fmt = codec->pix_fmts[0];

        if(st->codec->strict_std_compliance <= FF_COMPLIANCE_UNOFFICIAL){
            if(st->codec->codec_id==CODEC_ID_MJPEG){
                p= (const enum PixelFormat[]){PIX_FMT_YUVJ420P, PIX_FMT_YUVJ422P, PIX_FMT_YUV420P, PIX_FMT_YUV422P, PIX_FMT_NONE};
            }else if(st->codec->codec_id==CODEC_ID_LJPEG){
                p= (const enum PixelFormat[]){PIX_FMT_YUVJ420P, PIX_FMT_YUVJ422P, PIX_FMT_YUVJ444P, PIX_FMT_YUV420P, PIX_FMT_YUV422P, PIX_FMT_YUV444P, PIX_FMT_BGRA, PIX_FMT_NONE};
            }
        }
        for(; *p!=-1; p++){
            if(*p == st->codec->pix_fmt)
                break;
            if (*p == PIX_FMT_YUV422P &&
                (st->codec->pix_fmt == PIX_FMT_YUV422P16 ||
                 st->codec->pix_fmt == PIX_FMT_YUV422P10))
                pix_fmt = PIX_FMT_YUV422P;
        }
        if (*p == -1) {
            if(st->codec->pix_fmt != PIX_FMT_NONE)
                av_log(NULL, AV_LOG_WARNING,
                        "Incompatible pixel format '%s' for codec '%s', auto-selecting format '%s'\n",
                        av_pix_fmt_descriptors[st->codec->pix_fmt].name,
                        codec->name,
                        av_pix_fmt_descriptors[pix_fmt].name);
            st->codec->pix_fmt = pix_fmt;
        }
    }
}

static OutputStream *new_output_stream(AVFormatContext *oc, int file_idx, AVCodec *codec)
{
    OutputStream *ost;
    AVStream *st = av_new_stream(oc, oc->nb_streams < nb_streamid_map ? streamid_map[oc->nb_streams] : 0);
    int idx      = oc->nb_streams - 1;

    if (!st) {
        av_log(NULL, AV_LOG_ERROR, "Could not alloc stream.\n");
        ffmpeg_exit(1);
    }

    output_streams_for_file[file_idx] =
        grow_array(output_streams_for_file[file_idx],
                   sizeof(*output_streams_for_file[file_idx]),
                   &nb_output_streams_for_file[file_idx],
                   oc->nb_streams);
    ost = output_streams_for_file[file_idx][idx] =
        av_mallocz(sizeof(OutputStream));
    if (!ost) {
        av_log(NULL, AV_LOG_ERROR, "Could not alloc output stream\n");
        ffmpeg_exit(1);
    }
    ost->file_index = file_idx;
    ost->index = idx;
    ost->st    = st;
    ost->enc   = codec;
    if (codec)
        ost->opts  = filter_codec_opts(codec_opts, codec->id, 1);

    avcodec_get_context_defaults3(st->codec, codec);

    ost->sws_flags = av_get_int(sws_opts, "sws_flags", NULL);
    return ost;
}

static double
get_sync_ipts(const OutputStream *ost)
{
    const InputStream *ist = ost->sync_ist;
    return (double)(ist->pts - start_time)/AV_TIME_BASE;
}

static void write_frame(AVFormatContext *s, AVPacket *pkt, AVCodecContext *avctx, AVBitStreamFilterContext *bsfc){
    int ret;

    while(bsfc){
        AVPacket new_pkt= *pkt;
        int a= av_bitstream_filter_filter(bsfc, avctx, NULL,
                                          &new_pkt.data, &new_pkt.size,
                                          pkt->data, pkt->size,
                                          pkt->flags & AV_PKT_FLAG_KEY);
        if(a>0){
            av_free_packet(pkt);
            new_pkt.destruct= av_destruct_packet;
        } else if(a<0){
            av_log(NULL, AV_LOG_ERROR, "%s failed for stream %d, codec %s",
                    bsfc->filter->name, pkt->stream_index,
                    avctx->codec ? avctx->codec->name : "copy");
            print_error("", a);
            if (exit_on_error)
                ffmpeg_exit(1);
        }
        *pkt= new_pkt;

        bsfc= bsfc->next;
    }

    ret= av_interleaved_write_frame(s, pkt);
    if(ret < 0){
        print_error("av_interleaved_write_frame()", ret);
        ffmpeg_exit(1);
    }
}

static int audiomerge_init(AudioMergeContext *a, int out_channels, int sample_size)
{
    int i;

    if (sample_size <= 0 || out_channels <= 0)
        return -1;

    a->out_channels = out_channels;
    a->sample_size = sample_size;

    a->buf_size = out_channels*sample_size*48000; // 1 sec at 48khz
    a->buf = av_malloc(a->buf_size);
    if (!a->buf)
        return AVERROR(ENOMEM);

    for (i = 0; i < out_channels; i++)
        a->buf_index[i] = i*sample_size;

    return 0;
}

static int audiomerge_add_channel(AudioMergeContext *a, uint8_t *input,
                                  unsigned in_channel, unsigned out_channel,
                                  unsigned in_channels, unsigned samples)
{
    if (out_channel >= a->out_channels)
        return -1;

    //fprintf(stderr, "in channel %d out channel %d samples %d\n", in_channel, out_channel, samples);

    if (a->buf_index[out_channel] +
        (int64_t)samples*a->sample_size*a->out_channels >= a->buf_size) {
        uint8_t *buf;
        if (a->buf_size + (int64_t)samples*a->sample_size*a->out_channels >= UINT_MAX)
            goto error;
        a->buf_size += (int64_t)samples*a->sample_size*a->out_channels;
        buf = av_realloc(a->buf, a->buf_size);
        if (!buf) {
        error:
            av_log(NULL, AV_LOG_ERROR, "error reallocating audiomerge buffer\n");
            return -1;
        }
        a->buf = buf;
    }

    input += a->sample_size*in_channel;
    while (samples--) {
        memcpy(a->buf + a->buf_index[out_channel], input, a->sample_size);
        a->buf_index[out_channel] += a->sample_size*a->out_channels;
        input += a->sample_size*in_channels;
    }

    a->last_sample_pos = FFMAX(a->buf_index[out_channel], a->last_sample_pos);

    return 0;
}


static unsigned audiomerge_complete_size(AudioMergeContext *a)
{
    int i, min = INT_MAX;
    int align = a->out_channels*a->sample_size;

    for (i = 0; i < a->out_channels; i++)
        min = FFMIN(a->buf_index[i], min);
    return (min / align) * align;
}

static unsigned audiomerge_get_buffered_samples(const OutputStream *ost, const InputStream *ist)
{
    int i;

    if (!ost->nb_audio_channel_maps)
        return 0;

    for (i = 0; i < ost->nb_audio_channel_maps; i++) {
        if (ost->audio_channel_maps[i]->file_index == ist->file_index &&
            ost->audio_channel_maps[i]->stream_index == ist->st->index) {
            return ost->audiomerge.buf_index[ost->audio_channel_maps[i]->out_channel_index] /
                (ost->audiomerge.sample_size*ost->audiomerge.out_channels);
        }
    }
    av_log(NULL, AV_LOG_ERROR, "error, could not find corresponding channel mapping\n");
    return 0;
}

static void audiomerge_drain_complete_size(AudioMergeContext *a)
{
    int i;
    unsigned complete_size = audiomerge_complete_size(a);

    if (!complete_size)
        return;

    memmove(a->buf, a->buf + complete_size,
            a->last_sample_pos - complete_size);
    a->last_sample_pos -= complete_size;
    for (i = 0; i < a->out_channels; i++)
        a->buf_index[i] -= complete_size;
}

#define MAX_AUDIO_PACKET_SIZE (128 * 1024)

static void do_audio_out(AVFormatContext *s,
                         OutputStream *ost,
                         InputStream *ist,
                         unsigned char *buf, int size)
{
    uint8_t *buftmp;
    int64_t audio_out_size, audio_buf_size;
    int size_out, frame_bytes, ret, resample_changed, i, in_channels;
    AVCodecContext *enc= ost->st->codec;
    AVCodecContext *dec= ist->st->codec;
    int osize = av_get_bytes_per_sample(enc->sample_fmt);
    int isize = av_get_bytes_per_sample(dec->sample_fmt);
    const int coded_bps = av_get_bits_per_sample(enc->codec->id);

    if (ost->nb_audio_channel_maps > 0)
        in_channels = enc->channels; // do not mix channels
    else
        in_channels = dec->channels;

    if (enc->channels != in_channels)
        ost->audio_resample = 1;

    resample_changed = ost->resample_sample_fmt != dec->sample_fmt ||
        (!ost->nb_audio_channel_maps &&
         ost->resample_channels != dec->channels) ||
        ost->resample_sample_rate != dec->sample_rate;

    if ((ost->audio_resample && !ost->resample) || resample_changed) {
        if (resample_changed) {
            if (ost->nb_audio_channel_maps > 0) {
                av_log(NULL, AV_LOG_ERROR, "Input stream audio configuration changed, aborting\n");
                ffmpeg_exit(1);
            }
            av_log(NULL, AV_LOG_INFO, "Input stream #%d.%d frame changed from rate:%d fmt:%s ch:%d to rate:%d fmt:%s ch:%d\n",
                   ist->file_index, ist->st->index,
                   ost->resample_sample_rate, av_get_sample_fmt_name(ost->resample_sample_fmt), ost->resample_channels,
                   dec->sample_rate, av_get_sample_fmt_name(dec->sample_fmt), dec->channels);
            ost->resample_sample_fmt  = dec->sample_fmt;
            ost->resample_channels    = dec->channels;
            ost->resample_sample_rate = dec->sample_rate;
            if (ost->resample)
                audio_resample_close(ost->resample);
        }
        /* if audio_sync_method is >1 the resampler is needed for audio drift compensation */
        if (audio_sync_method <= 1 &&
            ost->resample_sample_fmt  == enc->sample_fmt &&
            ost->resample_channels    == enc->channels   &&
            ost->resample_sample_rate == enc->sample_rate) {
            ost->resample = NULL;
            ost->audio_resample = 0;
        } else {
            ost->audio_resample = 1;
            if (dec->sample_fmt != AV_SAMPLE_FMT_S16 && enc->sample_fmt != AV_SAMPLE_FMT_S16)
                av_log(NULL, AV_LOG_ERROR, "Warning, using s16 intermediate sample format for resampling\n");
            ost->resample = av_audio_resample_init(enc->channels,    in_channels,
                                                   enc->sample_rate, dec->sample_rate,
                                                   enc->sample_fmt,  dec->sample_fmt,
                                                   32, 10, 0, 0.97);
            if (!ost->resample) {
                av_log(NULL, AV_LOG_ERROR, "Can not resample %d channels @ %d Hz to %d channels @ %d Hz\n",
                        in_channels, dec->sample_rate,
                        enc->channels, enc->sample_rate);
                ffmpeg_exit(1);
            }
        }
    }

#define MAKE_SFMT_PAIR(a,b) ((a)+AV_SAMPLE_FMT_NB*(b))
    if (!ost->audio_resample && dec->sample_fmt!=enc->sample_fmt &&
        MAKE_SFMT_PAIR(enc->sample_fmt,dec->sample_fmt)!=ost->reformat_pair) {
        if (ost->reformat_ctx)
            av_audio_convert_free(ost->reformat_ctx);
        ost->reformat_ctx = av_audio_convert_alloc(enc->sample_fmt, 1,
                                                   dec->sample_fmt, 1, NULL, 0);
        if (!ost->reformat_ctx) {
            av_log(NULL, AV_LOG_ERROR, "Cannot convert %s sample format to %s sample format\n",
                av_get_sample_fmt_name(dec->sample_fmt),
                av_get_sample_fmt_name(enc->sample_fmt));
            ffmpeg_exit(1);
        }
        ost->reformat_pair=MAKE_SFMT_PAIR(enc->sample_fmt,dec->sample_fmt);
    }

    // sync_ist will be set to the first input stream with audiomerge
    if(ost->audio_channel_maps > 0)
        ost->sync_ist = ist;

    if(audio_sync_method){
        double delta = get_sync_ipts(ost) * enc->sample_rate - ost->sync_opts
            - av_fifo_size(ost->fifo)/(enc->channels * osize)
            - audiomerge_get_buffered_samples(ost, ist) /
            (double)dec->sample_rate * enc->sample_rate;
        double idelta= delta*dec->sample_rate / enc->sample_rate;
        int byte_delta= ((int)idelta)*isize*dec->channels;

        if (verbose > 3)
            av_log(NULL, AV_LOG_INFO, "adelta:%f ost->sync_opts:%"PRId64", ost->sync_ipts:%f, size:%d, stream:#%d.%d\n",
                    delta / enc->sample_rate, ost->sync_opts, get_sync_ipts(ost), size, ist->file_index, ist->st->index);

        //FIXME resample delay
        if(fabs(delta) > 50){
            if(ost->is_start || fabs(delta) > audio_drift_threshold*enc->sample_rate){
                if(byte_delta < 0){
                    byte_delta= FFMAX(byte_delta, -size);
                    size += byte_delta;
                    buf  -= byte_delta;
                    if(verbose > 0)
                        av_log(NULL, AV_LOG_INFO, "discarding %d audio samples in stream #%d.%d\n",
                                -byte_delta/(isize*ist->st->codec->channels),
                                ist->file_index, ist->st->index);
                    if(!size)
                        return;
                }else{
                    if (byte_delta + size > INT_MAX/5) {
                        av_log(NULL, AV_LOG_ERROR, "error, audio timestamps difference is too big, "
                               "cannot compensate a/v sync\n");
                        return;
                    }
                    av_fast_malloc(&input_tmp, &input_tmp_size, byte_delta + size);
                    memset(input_tmp, 0, byte_delta);
                    memcpy(input_tmp + byte_delta, buf, size);
                    buf= input_tmp;
                    size += byte_delta;
                    if(verbose > 0)
                        av_log(NULL, AV_LOG_INFO, "adding %d audio samples in stream #%d.%d\n",
                                byte_delta/(isize*ist->st->codec->channels),
                                ist->file_index, ist->st->index);
                }
            }else if(audio_sync_method>1){
                int comp= av_clip(delta, -audio_sync_method, audio_sync_method);
                av_assert0(ost->audio_resample);
                if(verbose > 2)
                    av_log(NULL, AV_LOG_INFO, "compensating audio timestamp drift:%f compensation:%d in:%d\n", delta, comp, enc->sample_rate);
//                fprintf(stderr, "drift:%f len:%d opts:%"PRId64" ipts:%"PRId64" fifo:%d\n", delta, -1, ost->sync_opts, (int64_t)(get_sync_ipts(ost) * enc->sample_rate), av_fifo_size(ost->fifo)/(ost->st->codec->channels * 2));
                av_resample_compensate(*(struct AVResampleContext**)ost->resample, comp, enc->sample_rate);
            }
        }
    }else
        ost->sync_opts= lrintf(get_sync_ipts(ost) * enc->sample_rate)
                        - av_fifo_size(ost->fifo)/(enc->channels * osize); //FIXME wrong

    audio_buf_size= (size + isize*dec->channels - 1) / (isize*dec->channels);
    audio_buf_size= (audio_buf_size*enc->sample_rate + dec->sample_rate) / dec->sample_rate;
    audio_buf_size= audio_buf_size*2 + 10000; //safety factors for the deprecated resampling API
    audio_buf_size= FFMAX(audio_buf_size, enc->frame_size);
    audio_buf_size*= osize*enc->channels;

    audio_out_size= FFMAX(audio_buf_size, enc->frame_size * osize * enc->channels);
    if(coded_bps > 8*osize)
        audio_out_size= audio_out_size * coded_bps / (8*osize);
    audio_out_size += FF_MIN_BUFFER_SIZE;

    if(audio_out_size > INT_MAX || audio_buf_size > INT_MAX){
        av_log(NULL, AV_LOG_ERROR, "audio buffer size too small\n");
        ffmpeg_exit(1);
    }

    av_fast_malloc(&audio_buf, &allocated_audio_buf_size, audio_buf_size);
    av_fast_malloc(&audio_out, &allocated_audio_out_size, audio_out_size);
    if (!audio_buf || !audio_out){
        av_log(NULL, AV_LOG_ERROR, "out of memory in do_audio_out\n");
        ffmpeg_exit(1);
    }

    if (ost->nb_audio_channel_maps > 0) {
        for (i = 0; i < ost->nb_audio_channel_maps; i++) {
            if (ost->audio_channel_maps[i]->file_index == ist->file_index &&
                ost->audio_channel_maps[i]->stream_index == ist->st->index) {
                if (audiomerge_add_channel(&ost->audiomerge, buf,
                                           ost->audio_channel_maps[i]->channel_index,
                                           ost->audio_channel_maps[i]->out_channel_index,
                                           dec->channels,
                                           size/(isize*dec->channels)) < 0) {
                    av_log(NULL, AV_LOG_ERROR, "audiomerge failed\n");
                    ffmpeg_exit(1);
                }
            }
        }
        buftmp = ost->audiomerge.buf;
        size_out = audiomerge_complete_size(&ost->audiomerge);
        if (!size_out)
            return; // no complete frame
    } else {
        buftmp = buf;
        size_out = size;
    }

    if (!ost->audio_resample && dec->sample_fmt!=enc->sample_fmt) {
        const void *ibuf[6]= {buftmp};
        void *obuf[6]= {audio_buf};
        int istride[6]= {isize};
        int ostride[6]= {osize};
        int len= size_out/istride[0];
        if (av_audio_convert(ost->reformat_ctx, obuf, ostride, ibuf, istride, len)<0) {
            printf("av_audio_convert() failed\n");
            if (exit_on_error)
                ffmpeg_exit(1);
            return;
        }
        buftmp = audio_buf;
        size_out = len*osize;
    }

    if (ost->audio_resample) {
        size_out = audio_resample(ost->resample,
                                  (short *)audio_buf, (short *)buftmp,
                                  size_out / (in_channels * isize));
        size_out = size_out * enc->channels * osize;
        buftmp = audio_buf;
    }

    /* now encode as many frames as possible */
    if (enc->frame_size > 1) {
        /* output resampled raw samples */
        if (av_fifo_realloc2(ost->fifo, av_fifo_size(ost->fifo) + size_out) < 0) {
            av_log(NULL, AV_LOG_ERROR, "av_fifo_realloc2() failed\n");
            ffmpeg_exit(1);
        }
        av_fifo_generic_write(ost->fifo, buftmp, size_out, NULL);

        frame_bytes = enc->frame_size * osize * enc->channels;

        while (av_fifo_size(ost->fifo) >= frame_bytes) {
            AVPacket pkt;
            av_init_packet(&pkt);

            av_fifo_generic_read(ost->fifo, audio_buf, frame_bytes, NULL);

            //FIXME pass ost->sync_opts as AVFrame.pts in avcodec_encode_audio()

            ret = avcodec_encode_audio(enc, audio_out, audio_out_size,
                                       (short *)audio_buf);
            if (ret < 0) {
                av_log(NULL, AV_LOG_ERROR, "Audio encoding failed\n");
                ffmpeg_exit(1);
            }
            audio_size += ret;
            pkt.stream_index= ost->index;
            pkt.data= audio_out;
            pkt.size= ret;
            if(enc->coded_frame && enc->coded_frame->pts != AV_NOPTS_VALUE)
                pkt.pts= av_rescale_q(enc->coded_frame->pts, enc->time_base, ost->st->time_base);
            pkt.flags |= AV_PKT_FLAG_KEY;
            write_frame(s, &pkt, enc, ost->bitstream_filters);

            ost->sync_opts += enc->frame_size;
        }
    } else {
        AVPacket pkt;
        av_init_packet(&pkt);

        ost->sync_opts += size_out / (osize * enc->channels);

        /* output a pcm frame */
        /* determine the size of the coded buffer */
        size_out /= osize;
        if (coded_bps)
            size_out = size_out*coded_bps/8;

        if(size_out > audio_out_size){
            av_log(NULL, AV_LOG_ERROR, "Internal error, buffer size too small\n");
            ffmpeg_exit(1);
        }

        //FIXME pass ost->sync_opts as AVFrame.pts in avcodec_encode_audio()
        ret = avcodec_encode_audio(enc, audio_out, size_out,
                                   (short *)buftmp);
        if (ret < 0) {
            av_log(NULL, AV_LOG_ERROR, "Audio encoding failed\n");
            ffmpeg_exit(1);
        }
        audio_size += ret;
        pkt.stream_index= ost->index;
        pkt.data= audio_out;
        pkt.size= ret;
        if(enc->coded_frame && enc->coded_frame->pts != AV_NOPTS_VALUE)
            pkt.pts= av_rescale_q(enc->coded_frame->pts, enc->time_base, ost->st->time_base);
        pkt.flags |= AV_PKT_FLAG_KEY;
        write_frame(s, &pkt, enc, ost->bitstream_filters);
    }

    if (ost->nb_audio_channel_maps > 0)
        audiomerge_drain_complete_size(&ost->audiomerge);

    ost->is_start = 0;
}

/* we begin to correct av delay at this threshold */
#define AV_DELAY_MAX 0.100

static void do_subtitle_out(AVFormatContext *s,
                            OutputStream *ost,
                            InputStream *ist,
                            AVSubtitle *sub,
                            int64_t pts)
{
    static uint8_t *subtitle_out = NULL;
    int subtitle_out_max_size = 1024 * 1024;
    int subtitle_out_size, nb, i;
    AVCodecContext *enc;
    AVPacket pkt;

    if (pts == AV_NOPTS_VALUE) {
        av_log(NULL, AV_LOG_ERROR, "Subtitle packets must have a pts\n");
        if (exit_on_error)
            ffmpeg_exit(1);
        return;
    }

    enc = ost->st->codec;

    if (!subtitle_out) {
        subtitle_out = av_malloc(subtitle_out_max_size);
    }

    /* Note: DVB subtitle need one packet to draw them and one other
       packet to clear them */
    /* XXX: signal it in the codec context ? */
    if (enc->codec_id == CODEC_ID_DVB_SUBTITLE)
        nb = 2;
    else
        nb = 1;

    for(i = 0; i < nb; i++) {
        sub->pts = av_rescale_q(pts, ist->st->time_base, AV_TIME_BASE_Q);
        // start_display_time is required to be 0
        sub->pts              += av_rescale_q(sub->start_display_time, (AVRational){1, 1000}, AV_TIME_BASE_Q);
        sub->end_display_time -= sub->start_display_time;
        sub->start_display_time = 0;
        subtitle_out_size = avcodec_encode_subtitle(enc, subtitle_out,
                                                    subtitle_out_max_size, sub);
        if (subtitle_out_size < 0) {
            av_log(NULL, AV_LOG_ERROR, "Subtitle encoding failed\n");
            ffmpeg_exit(1);
        }

        av_init_packet(&pkt);
        pkt.stream_index = ost->index;
        pkt.data = subtitle_out;
        pkt.size = subtitle_out_size;
        pkt.pts = av_rescale_q(sub->pts, AV_TIME_BASE_Q, ost->st->time_base);
        if (enc->codec_id == CODEC_ID_DVB_SUBTITLE) {
            /* XXX: the pts correction is handled here. Maybe handling
               it in the codec would be better */
            if (i == 0)
                pkt.pts += 90 * sub->start_display_time;
            else
                pkt.pts += 90 * sub->end_display_time;
        }
        write_frame(s, &pkt, ost->st->codec, ost->bitstream_filters);
    }
}

static int bit_buffer_size= 1024*256;
static uint8_t *bit_buffer= NULL;

static void encode_frame(AVFormatContext *s,
                         OutputStream *ost, InputStream *ist, int nb_frames,
                         AVFrame *frame, int *frame_size, int quality)
{
    AVCodecContext *enc = ost->st->codec;
    int i, ret;

    /* duplicates frame if needed */
    for (i = 0; i < nb_frames; i++) {
        AVPacket pkt;
        av_init_packet(&pkt);
        pkt.stream_index = ost->index;

        if (s->oformat->flags & AVFMT_RAWPICTURE) {
            /* raw pictures are written as AVPicture structure to
               avoid any copies. We support temorarily the older
               method. */
            pkt.data = (uint8_t *)frame;
            pkt.size = sizeof(AVPicture);
            pkt.pts = av_rescale_q(ost->sync_opts, enc->time_base, ost->st->time_base);
            pkt.flags |= AV_PKT_FLAG_KEY;

            write_frame(s, &pkt, ost->st->codec, ost->bitstream_filters);
            video_size += avpicture_get_size(enc->pix_fmt, enc->width, enc->height);
        } else {
            /* handles sameq here. This is not correct because it may
               not be a global option */
            frame->quality = quality;
            frame->pict_type = 0;
            frame->pts = ost->sync_opts;

            if (ost->forced_kf_index < ost->forced_kf_count &&
                frame->pts >= ost->forced_kf_pts[ost->forced_kf_index]) {
                frame->pict_type = FF_I_TYPE;
                ost->forced_kf_index++;
            }
            ret = avcodec_encode_video(enc,
                                       bit_buffer, bit_buffer_size,
                                       frame);
            if (ret < 0) {
                av_log(NULL, AV_LOG_ERROR, "Video encoding failed\n");
                ffmpeg_exit(1);
            }

            if (ret > 0) {
                pkt.data = bit_buffer;
                pkt.size = ret;
                if (enc->coded_frame->pts != AV_NOPTS_VALUE)
                    pkt.pts = av_rescale_q(enc->coded_frame->pts, enc->time_base, ost->st->time_base);

                if (enc->coded_frame->key_frame)
                    pkt.flags |= AV_PKT_FLAG_KEY;
                write_frame(s, &pkt, ost->st->codec, ost->bitstream_filters);
                *frame_size = ret;
                video_size += ret;
                if (ost->logfile && enc->stats_out) {
                    fprintf(ost->logfile, "%s", enc->stats_out);
                }
            }
        }
        ost->sync_opts++;
        ost->frame_number++;
    }
}

static double psnr(double d){
    return -10.0*log(d)/log(10.0);
}

static void do_video_stats(OutputStream *ost, int frame_size)
{
    AVCodecContext *enc;
    int frame_number;
    double ti1, bitrate, avg_bitrate;

    /* this is executed just the first time do_video_stats is called */
    if (!vstats_file) {
        vstats_file = fopen(vstats_filename, "w");
        if (!vstats_file) {
            perror("fopen");
            ffmpeg_exit(1);
        }
    }

    enc = ost->st->codec;
    if (enc->codec_type == AVMEDIA_TYPE_VIDEO) {
        frame_number = ost->frame_number;
        fprintf(vstats_file, "frame= %5d q= %2.1f ", frame_number, enc->coded_frame->quality/(float)FF_QP2LAMBDA);
        if (enc->flags&CODEC_FLAG_PSNR && enc->codec_id != CODEC_ID_H264)
            fprintf(vstats_file, "PSNR= %6.2f ", psnr(enc->coded_frame->error[0]/(enc->width*enc->height*255.0*255.0)));

        fprintf(vstats_file,"f_size= %6d ", frame_size);
        /* compute pts value */
        ti1 = ost->sync_opts * av_q2d(enc->time_base);

        bitrate = (frame_size * 8) / av_q2d(enc->time_base) / 1000.0;
        avg_bitrate = ti1 ? video_size * 8 / ti1 / 1000.0 : 0;
        fprintf(vstats_file, "s_size= %8.0fkB time= %0.3f br= %7.1fkbits/s avg_br= %7.1fkbits/s ",
            (double)video_size / 1024, ti1, bitrate, avg_bitrate);
        fprintf(vstats_file,"type= %c\n", av_get_picture_type_char(enc->coded_frame->pict_type));
    }
}

static void do_video_out(AVFormatContext *s, OutputStream *ost, InputStream *ist,
                         AVFrame *in_picture, float quality, int vsync_method)
{
    int nb_frames, av_unused resample_changed;
    int frame_size = 0;
    AVFrame *final_picture, *formatted_picture;
    AVFrame frame;
    AVCodecContext *enc = ost->st->codec;
    double sync_ipts;
    int frames_left = max_frames[AVMEDIA_TYPE_VIDEO] - ost->frame_number;

    if (frames_left <= 0)
        return;

    sync_ipts = get_sync_ipts(ost) / av_q2d(enc->time_base);

    /* by default, we output a single frame */
    nb_frames = 1;

    if (vsync_method && vsync_method != 3) {
        double vdelta;
        if (ist->dts_is_reordered_pts && ist->st->codec->has_b_frames > 0)
            sync_ipts -= ist->st->codec->has_b_frames;
        vdelta = sync_ipts - ost->sync_opts;
        if (vdelta <= -0.6)
            nb_frames = 0;
        else if (vsync_method == 2 || (vsync_method<0 && (s->oformat->flags & AVFMT_VARIABLE_FPS))){
            if(vdelta<=-0.6){
                nb_frames=0;
            }else if(vdelta>0.6)
                ost->sync_opts= lrintf(sync_ipts);
        }else if (vdelta > 0.6)
            nb_frames += lrintf(vdelta);
        if (verbose>3)
            av_log(NULL, AV_LOG_INFO, "vdelta:%f, ost->sync_opts:%"PRId64", ost->sync_ipts:%f nb_frames:%d\n",
                    vdelta, ost->sync_opts, get_sync_ipts(ost), nb_frames);
    } else if (!vsync_method) {
        if (ist->dts_is_reordered_pts && ist->st->codec->has_b_frames > 0)
            sync_ipts -= ist->st->codec->has_b_frames;
        ost->sync_opts= lrintf(sync_ipts);
    }

    if (nb_frames == 0) {
        ++nb_frames_drop;
        if (verbose > 2)
            av_log(NULL, AV_LOG_INFO, "*** drop!\n");
    }
    nb_frames = FFMIN(nb_frames, frames_left);
    if (nb_frames <= 0)
        return;
    if (nb_frames > 1) {
        nb_frames_dup += nb_frames - 1;
        if (verbose > 2)
            av_log(NULL, AV_LOG_INFO, "*** %d dup!\n", nb_frames-1);
    }

    formatted_picture = in_picture;
    final_picture = formatted_picture;

#if !CONFIG_AVFILTER
    resample_changed = ost->resample_width   != dec->width  ||
                       ost->resample_height  != dec->height ||
                       ost->resample_pix_fmt != dec->pix_fmt;

    if (resample_changed) {
        av_log(NULL, AV_LOG_INFO,
               "Input stream #%d.%d frame changed from size:%dx%d fmt:%s to size:%dx%d fmt:%s\n",
               ist->file_index, ist->st->index,
               ost->resample_width, ost->resample_height, av_get_pix_fmt_name(ost->resample_pix_fmt),
               dec->width         , dec->height         , av_get_pix_fmt_name(dec->pix_fmt));
        ost->resample_width   = dec->width;
        ost->resample_height  = dec->height;
        ost->resample_pix_fmt = dec->pix_fmt;
    }

    ost->video_resample = dec->width   != enc->width  ||
                          dec->height  != enc->height ||
                          dec->pix_fmt != enc->pix_fmt;

    if (ost->video_resample) {
        final_picture = &ost->resample_frame;
        if (!ost->img_resample_ctx || resample_changed) {
            /* initialize the destination picture */
            if (!ost->resample_frame.data[0]) {
                avcodec_get_frame_defaults(&ost->resample_frame);
                if (avpicture_alloc((AVPicture *)&ost->resample_frame, enc->pix_fmt,
                                    enc->width, enc->height)) {
                    av_log(NULL, AV_LOG_ERROR, "Cannot allocate temp picture, check pix fmt\n");
                    ffmpeg_exit(1);
                }
            }
            /* initialize a new scaler context */
            sws_freeContext(ost->img_resample_ctx);
            ost->img_resample_ctx = sws_getContext(dec->width, dec->height, dec->pix_fmt,
                                                   enc->width, enc->height, enc->pix_fmt,
                                                   ost->sws_flags, NULL, NULL, NULL);
            if (ost->img_resample_ctx == NULL) {
                av_log(NULL, AV_LOG_ERROR, "Cannot get resampling context\n");
                ffmpeg_exit(1);
            }
        }
        sws_scale(ost->img_resample_ctx, formatted_picture->data, formatted_picture->linesize,
              0, ost->resample_height, final_picture->data, final_picture->linesize);
    }
#endif

    /* better than nothing: use input picture interlaced
       settings */
    frame = *final_picture;
    frame.interlaced_frame = ost->st->codec->interlaced > 0;
    frame.top_field_first = ost->st->codec->interlaced == 1;

    if (nb_frames > 1) { // dup frames
        encode_frame(s, ost, ist, nb_frames - 1,
                     ost->prev_frame.data[0] ? &ost->prev_frame :
                     &frame, &frame_size, quality);
        if (vstats_filename && frame_size)
            do_video_stats(ost, frame_size);
    }

    encode_frame(s, ost, ist, 1, &frame, &frame_size, quality);
    if (vstats_filename && frame_size)
        do_video_stats(ost, frame_size);
    if (ist->st->codec->codec->capabilities & CODEC_CAP_DR1 || ost->picref) {
        ost->prev_frame = frame;
        if (ost->prev_picref)
            avfilter_unref_buffer(ost->prev_picref);
        ost->prev_picref = ost->picref;
        ost->picref = NULL;
    }
}

static void print_report(AVFormatContext **output_files,
                         OutputStream **ost_table, int nb_ostreams,
                         int is_last_report, int64_t duration)
{
    char buf[1024];
    OutputStream *ost, *vst = NULL;
    AVFormatContext *oc;
    int64_t total_size;
    AVCodecContext *enc;
    int i, frame_diff;
    double bitrate;
    int64_t pts = INT64_MAX;
    int64_t cur_time, elapsed_time = 0;
    static int64_t last_time = -1;
    static int qp_histogram[52];
    static int64_t prev_pts;
    static int prev_frame_number, max_status_len;

    cur_time = av_gettime();
    if (is_last_report) {
        elapsed_time = cur_time - timer_start;
    } else {
        if (last_time == -1) {
            last_time = cur_time;
            return;
        }
        elapsed_time = cur_time - last_time;
        /* display the report every 0.5 seconds */
        if (elapsed_time < update_interval)
            return;
        last_time = cur_time;
    }

    oc = output_files[0];

    total_size = avio_size(oc->pb);
    if(total_size<0) // FIXME improve url_fsize() so it works with non seekable output too
        total_size= avio_tell(oc->pb);
    if(total_size<0)
        total_size = 0;

    buf[0] = '\0';
    for(i=0;i<nb_ostreams;i++) {
        float q = -1;
        ost = ost_table[i];
        enc = ost->st->codec;
        if (vst && enc->coded_frame && enc->codec_type == AVMEDIA_TYPE_VIDEO) {
            snprintf(buf + strlen(buf), sizeof(buf) - strlen(buf), "q=%2.1f ",
                     enc->coded_frame->quality/(float)FF_QP2LAMBDA);
        }
        if (!vst && enc->codec_type == AVMEDIA_TYPE_VIDEO) {
            float t = elapsed_time / 1000000.0;
            frame_diff = is_last_report ? ost->frame_number :
                ost->frame_number - prev_frame_number;
            prev_frame_number = ost->frame_number;
            snprintf(buf + strlen(buf), sizeof(buf) - strlen(buf), "frame=%5d fps=%3.0f ",
                     ost->frame_number, frame_diff / t);
            if (enc->coded_frame) {
                snprintf(buf + strlen(buf), sizeof(buf) - strlen(buf), "q=%2.1f ",
                         enc->coded_frame->quality/(float)FF_QP2LAMBDA);
            }
            if(is_last_report)
                snprintf(buf + strlen(buf), sizeof(buf) - strlen(buf), "L");
            if(qp_hist){
                int j;
                int qp = lrintf(q);
                if(qp>=0 && qp<FF_ARRAY_ELEMS(qp_histogram))
                    qp_histogram[qp]++;
                for(j=0; j<32; j++)
                    snprintf(buf + strlen(buf), sizeof(buf) - strlen(buf), "%X", (int)lrintf(log(qp_histogram[j]+1)/log(2)));
            }
            if (enc->flags&CODEC_FLAG_PSNR && enc->codec_id != CODEC_ID_H264){
                int j;
                double error, error_sum=0;
                double scale, scale_sum=0;
                char type[3]= {'Y','U','V'};
                snprintf(buf + strlen(buf), sizeof(buf) - strlen(buf), "PSNR=");
                for(j=0; j<3; j++){
                    if(is_last_report){
                        error= enc->error[j];
                        scale= enc->width*enc->height*255.0*255.0*ost->frame_number;
                    }else{
                        error= enc->coded_frame->error[j];
                        scale= enc->width*enc->height*255.0*255.0;
                    }
                    if(j) scale/=4;
                    error_sum += error;
                    scale_sum += scale;
                    snprintf(buf + strlen(buf), sizeof(buf) - strlen(buf), "%c:%2.2f ", type[j], psnr(error/scale));
                }
                snprintf(buf + strlen(buf), sizeof(buf) - strlen(buf), "*:%2.2f ", psnr(error_sum/scale_sum));
            }
            vst = ost;
        }
        /* compute min output value */
        pts = FFMIN(pts, av_rescale_q(ost->sync_opts,
                                      ost->st->codec->time_base, AV_TIME_BASE_Q));
    }

    if (verbose > 0 || is_last_report) {
        int hours, mins, secs, us;
        int64_t time_left;
        double speed;
        break_time(pts, &hours, &mins, &secs, &us);

        bitrate = pts ? total_size * 8 / (pts / 1000.0) : 0;
        snprintf(buf + strlen(buf), sizeof(buf) - strlen(buf),
                 "size=%8.0fkB time=", total_size / 1024.0);
        snprintf(buf + strlen(buf), sizeof(buf) - strlen(buf),
                 "%02d:%02d:%02d.%02d ", hours, mins, secs,
                 (100 * us) / AV_TIME_BASE);
        snprintf(buf + strlen(buf), sizeof(buf) - strlen(buf),
                 "bitrate=%6.1fkbits/s", bitrate);

        if (!is_last_report) {
            if (vst && max_frames[AVMEDIA_TYPE_VIDEO] != INT_MAX && vst->frame_number > 0) {
                speed = (double)frame_diff / elapsed_time;
                time_left = (max_frames[AVMEDIA_TYPE_VIDEO] - vst->frame_number) / speed;
                goto eta;
            } else if (duration > 0 && pts > 0) {
                float pts_diff = pts - prev_pts;
                prev_pts = pts;
                speed = pts_diff / elapsed_time;
                time_left = (duration - pts) / speed;
            eta:
                if (time_left < 0)
                    time_left = 0;
                break_time(time_left, &hours, &mins, &secs, &us);
                snprintf(buf + strlen(buf), sizeof(buf) - strlen(buf),
                         " eta=%02d:%02d:%02d.%02d", hours, mins, secs,
                         (100 * us) / AV_TIME_BASE);
            } else {
                snprintf(buf + strlen(buf), sizeof(buf) - strlen(buf), " eta=N/A");
            }
        }

        if (nb_frames_dup || nb_frames_drop)
          snprintf(buf + strlen(buf), sizeof(buf) - strlen(buf), " dup=%d drop=%d",
                  nb_frames_dup, nb_frames_drop);

        if (verbose >= 0) {
            int len = strlen(buf);
            max_status_len = FFMAX(max_status_len, len);
            av_log(NULL, AV_LOG_INFO,  "%s%*s", buf, max_status_len + 1 - len,
                   isatty(2) ? "\r" : "\n");
        }

        fflush(stderr);
    }

    if (is_last_report && verbose >= 0){
        int64_t raw= audio_size + video_size + extra_size;
        av_log(NULL, AV_LOG_INFO, "video:%1.0fkB audio:%1.0fkB global headers:%1.0fkB muxing overhead %f%%\n",
                video_size/1024.0,
                audio_size/1024.0,
                extra_size/1024.0,
                total_size ? 100.0*(total_size - raw)/raw : 0);
    }
}

static void generate_silence(uint8_t* buf, enum AVSampleFormat sample_fmt, size_t size)
{
    int fill_char = 0x00;
    if (sample_fmt == AV_SAMPLE_FMT_U8)
        fill_char = 0x80;
    memset(buf, fill_char, size);
}

/* pkt = NULL means EOF (needed to flush decoder buffers) */
static int output_packet(InputStream *ist, int ist_index,
                         OutputStream **ost_table, int nb_ostreams,
                         const AVPacket *pkt)
{
    AVFormatContext *os;
    OutputStream *ost;
    int ret, i, j;
    int got_output;
    AVFrame picture;
    static unsigned int samples_size= 0;
    AVSubtitle subtitle, *subtitle_to_free = NULL;
    int64_t pkt_pts = AV_NOPTS_VALUE;
#if CONFIG_AVFILTER
    int frame_available;
#endif
    float quality = 0;

    AVPacket avpkt;
    int bps = av_get_bytes_per_sample(ist->st->codec->sample_fmt);

    if(ist->next_pts == AV_NOPTS_VALUE)
        ist->next_pts= ist->pts;

    if (ist->is_past_recording_time)
        goto discard_packet;

    if (pkt == NULL) {
        /* EOF handling */
        av_init_packet(&avpkt);
        avpkt.data = NULL;
        avpkt.size = 0;
        goto handle_eof;
    } else {
        avpkt = *pkt;
    }

    if(pkt->dts != AV_NOPTS_VALUE)
        ist->next_pts = ist->pts = av_rescale_q(pkt->dts, ist->st->time_base, AV_TIME_BASE_Q);
    if(pkt->pts != AV_NOPTS_VALUE)
        pkt_pts = av_rescale_q(pkt->pts, ist->st->time_base, AV_TIME_BASE_Q);

    //while we have more to decode or while the decoder did output something on EOF
    while (avpkt.size > 0 || (!pkt && got_output)) {
        uint8_t *data_buf, *decoded_data_buf;
        int data_size, decoded_data_size;
    handle_eof:
        ist->pts= ist->next_pts;

        if(avpkt.size && avpkt.size != pkt->size &&
           ((!ist->showed_multi_packet_warning && verbose>0) || verbose>1)){
            av_log(NULL, AV_LOG_INFO, "Multiple frames in a packet from stream %d\n", pkt->stream_index);
            ist->showed_multi_packet_warning=1;
        }

        /* decode the packet if needed */
        decoded_data_buf = NULL; /* fail safe */
        decoded_data_size= 0;
        data_buf  = avpkt.data;
        data_size = avpkt.size;
        subtitle_to_free = NULL;
        if (ist->decoding_needed) {
            switch(ist->st->codec->codec_type) {
            case AVMEDIA_TYPE_AUDIO:{
                if(pkt && samples_size < FFMAX(pkt->size*sizeof(*samples), AVCODEC_MAX_AUDIO_FRAME_SIZE)) {
                    samples_size = FFMAX(pkt->size*sizeof(*samples), AVCODEC_MAX_AUDIO_FRAME_SIZE);
                    av_free(samples);
                    samples= av_malloc(samples_size);
                }
                decoded_data_size= samples_size;
                    /* XXX: could avoid copy if PCM 16 bits with same
                       endianness as CPU */
                ret = avcodec_decode_audio3(ist->st->codec, samples, &decoded_data_size,
                                            &avpkt);
                if (ret < 0)
                    return ret;
                avpkt.data += ret;
                avpkt.size -= ret;
                data_size   = ret;
                got_output  = decoded_data_size > 0;
                /* Some bug in mpeg audio decoder gives */
                /* decoded_data_size < 0, it seems they are overflows */
                /* let's try to catch it by failing if it still happens */
                if (decoded_data_size < 0)
                    return decoded_data_size;
                if (!decoded_data_size) {
                    /* no audio frame */
                    continue;
                }
                decoded_data_buf = (uint8_t *)samples;
                ist->next_pts += ((int64_t)AV_TIME_BASE/bps * decoded_data_size) /
                    (ist->st->codec->sample_rate * ist->st->codec->channels);
                break;}
            case AVMEDIA_TYPE_VIDEO:
                    decoded_data_size = (ist->st->codec->width * ist->st->codec->height * 3) / 2;
                    /* XXX: allocate picture correctly */
                    avcodec_get_frame_defaults(&picture);
                    avpkt.pts = pkt_pts;
                    avpkt.dts = ist->pts;
                    pkt_pts = AV_NOPTS_VALUE;

                    ret = avcodec_decode_video2(ist->st->codec,
                                                &picture, &got_output, &avpkt);
                    quality = same_quality ? picture.quality : 0;
                    if (ret < 0)
                        return ret;
                    if (!got_output) {
                        /* no picture yet */
                        goto discard_packet;
                    }
                    ist->next_pts = ist->pts = picture.best_effort_timestamp;
                    if (ist->st->codec->time_base.num != 0) {
                        int ticks = ist->st->codec->ticks_per_frame;
                        ist->next_pts += ((int64_t)AV_TIME_BASE *
                                          ist->st->codec->time_base.num * ticks) /
                            ist->st->codec->time_base.den;
                    } else if (ist->st->avg_frame_rate.num) {
                        ist->next_pts += ((int64_t)AV_TIME_BASE * ist->st->avg_frame_rate.den) /
                            ist->st->avg_frame_rate.num;
                    }
                    avpkt.size = 0;
                    break;
            case AVMEDIA_TYPE_SUBTITLE:
                ret = avcodec_decode_subtitle2(ist->st->codec,
                                               &subtitle, &got_output, &avpkt);
                if (ret < 0)
                    return ret;
                if (!got_output) {
                    goto discard_packet;
                }
                subtitle_to_free = &subtitle;
                avpkt.size = 0;
                break;
            default:
                return -1;
            }
        } else {
            switch(ist->st->codec->codec_type) {
            case AVMEDIA_TYPE_AUDIO:
                ist->next_pts += ((int64_t)AV_TIME_BASE * ist->st->codec->frame_size) /
                    ist->st->codec->sample_rate;
                break;
            case AVMEDIA_TYPE_VIDEO:
                // offset dts by delay when stream copying
                ist->pts += av_rescale_q(ist->st->start_time - ist->st->first_dts, ist->st->time_base, AV_TIME_BASE_Q);
                if (ist->st->codec->time_base.num != 0) {
                    int ticks = ist->st->codec->ticks_per_frame;
                    ist->next_pts += ((int64_t)AV_TIME_BASE *
                                      ist->st->codec->time_base.num * ticks) /
                        ist->st->codec->time_base.den;
                } else if (ist->st->avg_frame_rate.num) {
                    ist->next_pts += ((int64_t)AV_TIME_BASE * ist->st->avg_frame_rate.den) /
                        ist->st->avg_frame_rate.num;
                }
                break;
            }
            ret = avpkt.size;
            avpkt.size = 0;
        }

        if (ist->pts - start_time >= recording_time) {
            ist->is_past_recording_time = 1;
            goto discard_packet;
        }

        // preprocess audio (volume)
        if (ist->st->codec->codec_type == AVMEDIA_TYPE_AUDIO) {
            if (audio_volume != 256) {
                short *volp;
                volp = samples;
                for(i=0;i<(decoded_data_size / sizeof(short));i++) {
                    int v = ((*volp) * audio_volume + 128) >> 8;
                    if (v < -32768) v = -32768;
                    if (v >  32767) v = 32767;
                    *volp++ = v;
                }
            }
        }

        /* frame rate emulation */
        if (rate_emu) {
            int64_t pts = av_rescale(ist->pts, 1000000, AV_TIME_BASE);
            int64_t now = av_gettime() - ist->start;
            if (pts > now)
                usleep(pts - now);
        }
        /* if output time reached then transcode raw format,
           encode packets and output them */
        if (start_time == 0 || ist->pts >= start_time)
            for(i=0;i<nb_ostreams;i++) {
                AVFrame oframe, *oframe_ptr = &picture;

                ost = ost_table[i];
                for(j=0;j<ost->nb_source_indexes;j++) {
                    if (ost->source_index[j] == ist_index)
                        break;
                }
                if (j == ost->nb_source_indexes)
                    continue;
#if CONFIG_AVFILTER
                if (ist->st->codec->codec_type == AVMEDIA_TYPE_VIDEO && ost->input_video_filter) {
                    // add it to be filtered
                    picture.pts = ist->pts;
                    av_vsrc_buffer_add_frame(ost->input_video_filter, &picture, ist->pts);
                }

                frame_available = ist->st->codec->codec_type != AVMEDIA_TYPE_VIDEO ||
                    !ost->output_video_filter || avfilter_poll_frame(ost->output_video_filter->inputs[0], 0);
                while (frame_available) {
                    if (ist->st->codec->codec_type == AVMEDIA_TYPE_VIDEO && ost->output_video_filter) {
                        AVRational ist_pts_tb = ost->output_video_filter->inputs[0]->time_base;
                        if (av_vsink_buffer_get_video_buffer_ref(ost->output_video_filter, &ost->picref, 0) < 0)
                            goto cont;
                        if (ost->picref) {
                            avcodec_get_frame_defaults(&oframe);
                            avfilter_fill_frame_from_video_buffer_ref(&oframe, ost->picref);
                            ist->pts = av_rescale_q(ost->picref->pts, ist_pts_tb, AV_TIME_BASE_Q);
                            oframe_ptr = &oframe;
                        }
                    }
#endif
                    os = output_files[ost->file_index];

                    /* set the input output pts pairs */
                    //ost->sync_ipts = (double)(ist->pts + input_files[ist->file_index].ts_offset - start_time)/ AV_TIME_BASE;

                    if (ost->encoding_needed) {
                        av_assert0(ist->decoding_needed);
                        switch(ost->st->codec->codec_type) {
                        case AVMEDIA_TYPE_AUDIO:
                            do_audio_out(os, ost, ist, decoded_data_buf, decoded_data_size);
                            break;
                        case AVMEDIA_TYPE_VIDEO:
                            do_video_out(os, ost, ist, oframe_ptr, same_quality ?
                                         quality : ost->st->codec->global_quality, video_sync_method);
                            break;
                        case AVMEDIA_TYPE_SUBTITLE:
                            do_subtitle_out(os, ost, ist, &subtitle,
                                            pkt->pts);
                            break;
                        default:
                            abort();
                        }
                    } else {
                        AVPacket opkt;
                        AVPicture pict;
                        int64_t ost_tb_start_time= av_rescale_q(start_time, AV_TIME_BASE_Q, ost->st->time_base);

                        av_init_packet(&opkt);

                        if ((!ost->frame_number && !(pkt->flags & AV_PKT_FLAG_KEY)) &&
                            !copy_initial_nonkeyframes &&
                            ost->st->codec->codec_type == AVMEDIA_TYPE_VIDEO &&
                            ost->st->codec->codec_id != CODEC_ID_NONE) {
                            av_log(NULL, AV_LOG_WARNING, "Stream #%d.%d dropping frames "
                                    "before first keyframe pts %"PRId64" "
                                    "dts %"PRId64" duration %d size %d flags %x\n",
                                    ist->file_index, ist->st->index, pkt->pts,
                                    pkt->dts, pkt->duration, pkt->size, pkt->flags);

#if !CONFIG_AVFILTER
                            continue;
#else
                            goto cont;
#endif
                        }

                        /* no reencoding needed : output the packet directly */
                        /* force the input stream PTS */

                        if(ost->st->codec->codec_type == AVMEDIA_TYPE_AUDIO)
                            audio_size += data_size;
                        else if (ost->st->codec->codec_type == AVMEDIA_TYPE_VIDEO) {
                            video_size += data_size;
                            ost->sync_opts++;
                        }

                        if (verbose > 3) {
                            av_log(NULL, AV_LOG_INFO, "Stream #%d.%d copy pts %"PRId64" dts %"PRId64" duration %d size %d flags %x start time %"PRId64"\n",
                                    ost->file_index, ost->index, pkt->pts, pkt->dts, pkt->duration, pkt->size, pkt->flags, ost_tb_start_time);
                            av_log(NULL, AV_LOG_INFO, "Stream #%d.%d copy ist pts %"PRId64" next pts %"PRId64"\n",
                                    ost->file_index, ost->index, ist->pts, ist->next_pts);
                        }

                        opkt.stream_index= ost->index;
                        if(pkt->pts != AV_NOPTS_VALUE)
                            opkt.pts= av_rescale_q(pkt->pts, ist->st->time_base, ost->st->time_base) - ost_tb_start_time;
                        else
                            opkt.pts= AV_NOPTS_VALUE;

                        if (pkt->dts == AV_NOPTS_VALUE && pkt->pts == AV_NOPTS_VALUE)
                            opkt.dts = av_rescale_q(ist->pts, AV_TIME_BASE_Q, ost->st->time_base);
                        else if (pkt->dts != AV_NOPTS_VALUE)
                            opkt.dts = av_rescale_q(pkt->dts, ist->st->time_base, ost->st->time_base);
                        opkt.dts -= ost_tb_start_time;

                        opkt.duration = av_rescale_q(pkt->duration, ist->st->time_base, ost->st->time_base);
                        opkt.flags = pkt->flags;
                        if (ist->dts_is_reordered_pts && ist->st->codec->has_b_frames > 0) {
                            if (opkt.pts != AV_NOPTS_VALUE)
                                opkt.pts -= ist->st->codec->has_b_frames*opkt.duration;
                            if (opkt.dts != AV_NOPTS_VALUE)
                                opkt.dts -= ist->st->codec->has_b_frames*opkt.duration;
                        }

                        //FIXME remove the following 2 lines they shall be replaced by the bitstream filters
                        if(   ost->st->codec->codec_id != CODEC_ID_H264
                           && ost->st->codec->codec_id != CODEC_ID_MPEG1VIDEO
                           && ost->st->codec->codec_id != CODEC_ID_MPEG2VIDEO
                           ) {
                            if(av_parser_change(ist->st->parser, ost->st->codec, &opkt.data, &opkt.size, data_buf, data_size, pkt->flags & AV_PKT_FLAG_KEY))
                                opkt.destruct= av_destruct_packet;
                        } else {
                            opkt.data = data_buf;
                            opkt.size = data_size;
                        }

                        if (os->oformat->flags & AVFMT_RAWPICTURE) {
                            /* store AVPicture in AVPacket, as expected by the output format */
                            avpicture_fill(&pict, opkt.data, ost->st->codec->pix_fmt, ost->st->codec->width, ost->st->codec->height);
                            opkt.data = (uint8_t *)&pict;
                            opkt.size = sizeof(AVPicture);
                            opkt.flags |= AV_PKT_FLAG_KEY;
                        }
                        write_frame(os, &opkt, ost->st->codec, ost->bitstream_filters);
                        ost->st->codec->frame_number++;
                        ost->frame_number++;
                        av_free_packet(&opkt);
                    }
#if CONFIG_AVFILTER
                    cont:
                    frame_available = (ist->st->codec->codec_type == AVMEDIA_TYPE_VIDEO) &&
                        ost->output_video_filter && avfilter_poll_frame(ost->output_video_filter->inputs[0], 0);
                    avfilter_unref_buffer(ost->picref);
                }
#endif
            }

        /* XXX: allocate the subtitles in the codec ? */
        if (subtitle_to_free) {
            avsubtitle_free(subtitle_to_free);
            subtitle_to_free = NULL;
        }
    }
 discard_packet:
    if (pkt == NULL) {
        /* EOF handling */

        for(i=0;i<nb_ostreams;i++) {
            ost = ost_table[i];
            for(j=0;j<ost->nb_source_indexes;j++) {
                if (ost->source_index[j] == ist_index)
                    break;
            }
            if (j < ost->nb_source_indexes) {
                AVCodecContext *enc= ost->st->codec;
                os = output_files[ost->file_index];

                if (ost->st->codec->codec_type == AVMEDIA_TYPE_VIDEO &&
                    ost->output_video_filter) {
                    while (avfilter_poll_frame(ost->output_video_filter->inputs[0], 1)) {
                        AVFrame frame;
                        AVRational ist_pts_tb = ost->output_video_filter->inputs[0]->time_base;
                        if (av_vsink_buffer_get_video_buffer_ref(ost->output_video_filter, &ost->picref, 0) < 0)
                            goto cont;
                        if (ost->picref) {
                            avfilter_fill_frame_from_video_buffer_ref(&frame, ost->picref);
                            ist->pts = av_rescale_q(ost->picref->pts, ist_pts_tb, AV_TIME_BASE_Q);
                            do_video_out(os, ost, ist, &frame, same_quality ?
                                         quality : ost->st->codec->global_quality, video_sync_method);
                            avfilter_unref_buffer(ost->picref);
                        }
                    }
                }

                if (ost->st->codec->codec_type == AVMEDIA_TYPE_VIDEO &&
                    1/av_q2d(ost->st->codec->time_base) > av_q2d(ist->st->r_frame_rate) &&
                    ost->sync_opts > 0) {
                    double sync_ipts = get_sync_ipts(ost) / av_q2d(enc->time_base);
                    while (sync_ipts - ost->sync_opts > 0.6) {
                        int64_t sync_opts = ost->sync_opts;
                        do_video_out(os, ost, ist, &ost->prev_frame, same_quality ?
                                     quality : ost->st->codec->global_quality, 3);
                        if (ost->sync_opts == sync_opts)
                            break;
                        if (verbose > 2)
                            fprintf(stderr, "*** 1 dup!\n");
                        nb_frames_dup++;
                    }
                }

                if(ost->st->codec->codec_type == AVMEDIA_TYPE_AUDIO && enc->frame_size <=1)
                    continue;
                if(ost->st->codec->codec_type == AVMEDIA_TYPE_VIDEO && (os->oformat->flags & AVFMT_RAWPICTURE))
                    continue;

                if (ost->encoding_needed) {
                    for(;;) {
                        AVPacket pkt;
                        int fifo_bytes;
                        av_init_packet(&pkt);
                        pkt.stream_index= ost->index;

                        switch(ost->st->codec->codec_type) {
                        case AVMEDIA_TYPE_AUDIO:
                            fifo_bytes = av_fifo_size(ost->fifo);
                            ret = 0;
                            /* encode any samples remaining in fifo */
                            if (fifo_bytes > 0) {
                                int osize = av_get_bytes_per_sample(enc->sample_fmt);
                                int fs_tmp = enc->frame_size;

                                av_fifo_generic_read(ost->fifo, audio_buf, fifo_bytes, NULL);
                                if (enc->codec->capabilities & CODEC_CAP_SMALL_LAST_FRAME) {
                                    enc->frame_size = fifo_bytes / (osize * enc->channels);
                                } else { /* pad */
                                    int frame_bytes = enc->frame_size*osize*enc->channels;
                                    if (allocated_audio_buf_size < frame_bytes)
                                        ffmpeg_exit(1);
                                    generate_silence(audio_buf+fifo_bytes, enc->sample_fmt, frame_bytes - fifo_bytes);
                                }

                                ret = avcodec_encode_audio(enc, bit_buffer, bit_buffer_size, (short *)audio_buf);
                                pkt.duration = av_rescale((int64_t)enc->frame_size*ost->st->time_base.den,
                                                          ost->st->time_base.num, enc->sample_rate);
                                enc->frame_size = fs_tmp;
                            }
                            if(ret <= 0) {
                                ret = avcodec_encode_audio(enc, bit_buffer, bit_buffer_size, NULL);
                            }
                            if (ret < 0) {
                                av_log(NULL, AV_LOG_ERROR, "Audio encoding failed\n");
                                ffmpeg_exit(1);
                            }
                            audio_size += ret;
                            pkt.flags |= AV_PKT_FLAG_KEY;
                            break;
                        case AVMEDIA_TYPE_VIDEO:
                            ret = avcodec_encode_video(enc, bit_buffer, bit_buffer_size, NULL);
                            if (ret < 0) {
                                av_log(NULL, AV_LOG_ERROR, "Video encoding failed\n");
                                ffmpeg_exit(1);
                            }
                            if (ret == 0)
                                break;
                            video_size += ret;
                            if(enc->coded_frame && enc->coded_frame->key_frame)
                                pkt.flags |= AV_PKT_FLAG_KEY;
                            if (ost->logfile && enc->stats_out) {
                                fprintf(ost->logfile, "%s", enc->stats_out);
                            }
                            break;
                        default:
                            ret=-1;
                        }

                        if(ret<=0)
                            break;
                        pkt.data= bit_buffer;
                        pkt.size= ret;
                        if(enc->coded_frame && enc->coded_frame->pts != AV_NOPTS_VALUE)
                            pkt.pts= av_rescale_q(enc->coded_frame->pts, enc->time_base, ost->st->time_base);
                        write_frame(os, &pkt, ost->st->codec, ost->bitstream_filters);
                    }
                }
            }
        }
    }

    return 0;
}

static void print_sdp(AVFormatContext **avc, int n)
{
    char sdp[2048];

    av_sdp_create(avc, n, sdp, sizeof(sdp));
    printf("SDP:\n%s\n", sdp);
    fflush(stdout);
}

static int copy_chapters(int infile, int outfile)
{
    AVFormatContext *is = input_files[infile].ctx;
    AVFormatContext *os = output_files[outfile];
    int i;

    for (i = 0; i < is->nb_chapters; i++) {
        AVChapter *in_ch = is->chapters[i], *out_ch;
        int64_t ts_off   = av_rescale_q(start_time - input_files[infile].ts_offset,
                                      AV_TIME_BASE_Q, in_ch->time_base);
        int64_t rt       = (recording_time == INT64_MAX) ? INT64_MAX :
                           av_rescale_q(recording_time, AV_TIME_BASE_Q, in_ch->time_base);


        if (in_ch->end < ts_off)
            continue;
        if (rt != INT64_MAX && in_ch->start > rt + ts_off)
            break;

        out_ch = av_mallocz(sizeof(AVChapter));
        if (!out_ch)
            return AVERROR(ENOMEM);

        out_ch->id        = in_ch->id;
        out_ch->time_base = in_ch->time_base;
        out_ch->start     = FFMAX(0,  in_ch->start - ts_off);
        out_ch->end       = FFMIN(rt, in_ch->end   - ts_off);

        if (metadata_chapters_autocopy)
            av_dict_copy(&out_ch->metadata, in_ch->metadata, 0);

        os->nb_chapters++;
        os->chapters = av_realloc(os->chapters, sizeof(AVChapter)*os->nb_chapters);
        if (!os->chapters)
            return AVERROR(ENOMEM);
        os->chapters[os->nb_chapters - 1] = out_ch;
    }
    return 0;
}

static void parse_forced_key_frames(char *kf, OutputStream *ost,
                                    AVCodecContext *avctx)
{
    char *p;
    int n = 1, i;
    int64_t t;

    for (p = kf; *p; p++)
        if (*p == ',')
            n++;
    ost->forced_kf_count = n;
    ost->forced_kf_pts = av_malloc(sizeof(*ost->forced_kf_pts) * n);
    if (!ost->forced_kf_pts) {
        av_log(NULL, AV_LOG_FATAL, "Could not allocate forced key frames array.\n");
        ffmpeg_exit(1);
    }
    for (i = 0; i < n; i++) {
        p = i ? strchr(p, ',') + 1 : kf;
        t = parse_time_or_die("force_key_frames", p, 1);
        ost->forced_kf_pts[i] = av_rescale_q(t, AV_TIME_BASE_Q, avctx->time_base);
    }
}

static const AVRational frame_rate_tab[] = {
    {24000, 1001},
    {   24,    1},
    {   25,    1},
    {30000, 1001},
    {   30,    1},
    {   50,    1},
    {60000, 1001},
    {   60,    1},
    {    0,    0},
};

static void validate_audio_target(AVFormatContext *s, OutputStream *ost)
{
    int sample_rate;
    if (!strcmp(ost->target, "vcd") || !strcmp(ost->target, "svcd"))
        sample_rate = 44100;
    else
        sample_rate = 48000;
    if (ost->st->codec->sample_rate != 48000) {
        av_log(NULL, AV_LOG_ERROR, "Error, target %s only supports "
                "%d sample rate\n", ost->target, sample_rate);
        av_log(NULL, AV_LOG_ERROR, "Resample with \"-ar %d\"\n", sample_rate);
        ffmpeg_exit(1);
    }
    if (!strcmp(s->oformat->name, "mxf")) {
        if (ost->st->codec->channels != 1) {
            av_log(NULL, AV_LOG_ERROR, "Error, target %s only supports "
                    "mono audio tracks\n", ost->target);
            ffmpeg_exit(1);
        }
    }
}

static void avfilter_graph_remove_auto_scalers(AVFilterGraph *graph)
{
    int i;
    for (i = 0; i < graph->filter_count; i++) {
        AVFilterContext *filter = graph->filters[i];
        if (!filter)
            continue;
        if (!strncmp(filter->name, "auto-inserted scaler", 20)) {
            AVFilterContext *src = filter->inputs[0]->src;
            AVFilterContext *dst = filter->outputs[0]->dst;
            avfilter_free(filter);
            avfilter_link(src, 0, dst, 0);
            graph->filters[i] = NULL;
            return;
        }
    }
}

static int auto_scale_pad(OutputStream *ost, int width, int scale_height, int pad_height)
{
    AVFilterContext *filter, *last;
    AVCodecContext *codec = ost->st->codec;
    char scale_args[128], pad_args[128];

    av_log(NULL, AV_LOG_INFO, "Auto-rescaling to %s resolution\n", ost->target);

    avfilter_graph_remove_auto_scalers(ost->graph);
    last = ost->output_video_filter->inputs[0]->src;
    av_freep(&last->outputs[0]);
    ost->output_video_filter->inputs[0] = NULL;

    scale_args[0] = 0;
    pad_args[0] = 0;

    if (scale_height > 0 && (ost->st->codec->width != width ||
                             ost->st->codec->height != scale_height)) {
        snprintf(scale_args, sizeof(scale_args),
                 "%d:%d:flags=0x%Xn:interl=-1", width, scale_height, ost->sws_flags);
        avfilter_graph_create_filter(&filter, avfilter_get_by_name("scale"),
                                     "target-scale", scale_args, NULL, ost->graph);
        avfilter_link(last, 0, filter, 0);
        last = filter;
    }

    if (pad_height > 0 && pad_height > scale_height && pad_height != scale_height) {
        snprintf(pad_args, sizeof(pad_args), "%d:%d:0:%d:black:1", width,
                 pad_height, pad_height - scale_height);
        avfilter_graph_create_filter(&filter, avfilter_get_by_name("pad"),
                                     "target-pad", pad_args, NULL, ost->graph);
        avfilter_link(last, 0, filter, 0);
        last = filter;
    }

    avfilter_link(last, 0, ost->output_video_filter, 0);
    if (avfilter_graph_config(ost->graph) < 0)
        ffmpeg_exit(1);

    codec->width  = ost->output_video_filter->inputs[0]->w;
    codec->height = ost->output_video_filter->inputs[0]->h;
    codec->sample_aspect_ratio = ost->st->sample_aspect_ratio =
        ost->frame_aspect_ratio.num ? // overriden by the -aspect cli option
        av_mul_q(ost->frame_aspect_ratio, (AVRational){codec->height, codec->width}) :
        ost->output_video_filter->inputs[0]->sample_aspect_ratio;
    return 0;
}

static void validate_video_target(AVFormatContext *s, OutputStream *ost)
{
    enum { NTSC_FILM, FILM, PAL, NTSC, HD50P, HD60P, UNKNOWN } norm = UNKNOWN;
    int frame_rate = (int)(ost->st->codec->time_base.den * 1000.0
                           / ost->st->codec->time_base.num);
    if (!ost->target)
        return;

    switch (frame_rate) {
    case 23976: norm = NTSC_FILM; break;
    case 24000: norm = FILM; break;
    case 25000: norm = PAL; break;
    case 29970: norm = NTSC; break;
    case 50000: norm = HD50P; break;
    case 59940: norm = HD60P; break;
    default:
        av_log(NULL, AV_LOG_ERROR, "Could not determine frame rate for target\n");
        av_log(NULL, AV_LOG_ERROR, "Set one from 23.98/24/25/29.97/50/59.94 with \"-r xxx\"\n");
        ffmpeg_exit(1);
    }

    if (!strcmp(ost->target, "vcd") || !strcmp(ost->target, "svcd") ||
        !strcmp(ost->target, "dvcpro50") ||
        !strcmp(ost->target, "dvcpro") || !strcmp(ost->target, "dvcam") ||
        !strncmp(ost->target, "imx", 3)) {
        if (norm != PAL && norm != NTSC) {
            av_log(NULL, AV_LOG_ERROR, "Error, target %s only supports ntsc "
                    "or pal frame rate\n", ost->target);
            ffmpeg_exit(1);
        }
    } else if (!strcmp(ost->target, "dvd")) {
        if (norm != PAL && norm != NTSC && norm != NTSC_FILM) {
            av_log(NULL, AV_LOG_ERROR, "Error, target %s only supports ntsc, "
                    "pal or ntsc film frame rates\n", ost->target);
            ffmpeg_exit(1);
        }
    }

    if (!strcmp(ost->target, "vcd")) {
        if (ost->st->codec->width != 352 ||
            (norm == PAL && ost->st->codec->height != 288) ||
            (norm == NTSC && ost->st->codec->height != 240)) {
            av_log(NULL, AV_LOG_ERROR, "Error, target vcd only supports 352x288(pal) "
                    "or 352x240(ntsc) resolutions\n");
            ffmpeg_exit(1);
        }
    } else if (!strcmp(ost->target, "svcd")) {
        if (ost->st->codec->width != 480 ||
            (norm == PAL && ost->st->codec->height != 576) ||
            (norm == NTSC && ost->st->codec->height != 480)) {
            av_log(NULL, AV_LOG_ERROR, "Error, target svcd only supports 480x576(pal) "
                    "or 480x480(ntsc) resolutions\n");
            ffmpeg_exit(1);
        }
    } else if (!strcmp(ost->target, "dvd") || !strcmp(ost->target, "dvcpro50") ||
               !strcmp(ost->target, "dvcpro") || !strcmp(ost->target, "dvcam")) {
        if (ost->st->codec->width != 720 ||
            (norm == PAL && ost->st->codec->height != 576) ||
            (norm == NTSC && ost->st->codec->height != 480)) {
            if (CONFIG_AVFILTER && !ost->st->stream_copy) {
                if (norm == PAL)
                    auto_scale_pad(ost, 720, 576, 0);
                else
                    auto_scale_pad(ost, 720, 480, 0);
            } else {
                av_log(NULL, AV_LOG_ERROR, "Error, target %s only supports 720x576(pal) "
                        "or 720x480(ntsc) resolutions\n", ost->target);
                ffmpeg_exit(1);
            }
        }
    } else if (!strcmp(ost->target, "dvcprohd")) {
        if ((norm == PAL && (ost->st->codec->width != 1440 ||
                             ost->st->codec->height != 1080)) ||
            (norm == NTSC && (ost->st->codec->width != 1280 ||
                              ost->st->codec->width != 1080)) ||
            ((norm == HD50P || norm == HD60P) && (ost->st->codec->width != 960 ||
                                                  ost->st->codec->height != 720))) {
            if (CONFIG_AVFILTER && !ost->st->stream_copy) {
                if (norm == HD50P || norm == HD60P)
                    auto_scale_pad(ost, 960, 720, 0);
                else if (norm == PAL)
                    auto_scale_pad(ost, 1440, 1080, 0);
                else if (norm == NTSC)
                    auto_scale_pad(ost, 1280, 1080, 0);
                else {
                    av_log(NULL, AV_LOG_ERROR, "Error, target %s only supports pal, ntsc, 50p or 60p "
                            "frame rate\n", ost->target);
                    ffmpeg_exit(1);
                }
            } else {
                av_log(NULL, AV_LOG_ERROR, "Error, target %s only supports 1440x1080(pal), "
                        "1280x1080(ntsc) or 960x720(50p,60p) resolutions\n", ost->target);
                ffmpeg_exit(1);
            }
        }
    } else if (!strncmp(ost->target, "imx", 3)) {
        if (ost->st->codec->width != 720 ||
            (norm == PAL && ost->st->codec->height != 608 ||
             norm == NTSC && ost->st->codec->height != 512)) {
            if (CONFIG_AVFILTER && !ost->st->stream_copy) {
                if (norm == PAL)
                    auto_scale_pad(ost, 720, 576, 608);
                else
                    auto_scale_pad(ost, 720, 486, 512);
            } else {
                av_log(NULL, AV_LOG_ERROR, "Error, target %s only supports 720x608(pal) "
                        "or 720x512(ntsc) resolutions\n", ost->target);
                ffmpeg_exit(1);
            }
        }
    } else if (!strcmp(ost->target, "xdcamhd422")) {
        if ((norm == HD50P || norm == HD60P) &&
            (ost->st->codec->width != 1280 || ost->st->codec->height != 720)) {
            av_log(NULL, AV_LOG_ERROR, "Error, target %s only supports 1280x720 resolution with "
                    "this frame rate\n", ost->target);
            ffmpeg_exit(1);
        }
        if (!((ost->st->codec->width == 1920 && ost->st->codec->height == 1080) ||
              (ost->st->codec->width == 1280 && ost->st->codec->height == 720))) {
            av_log(NULL, AV_LOG_ERROR, "Error, target %s only supports 1920x1080 "
                    "or 1280x720 resolutions\n", ost->target);
            ffmpeg_exit(1);
        }
        if (!strcmp(s->oformat->name, "mov")) {
            if (ost->st->codec->height == 720) {
                if (norm == FILM || norm == NTSC_FILM)
                    ost->st->codec->codec_tag = AV_RL32("xd54");
                else if (norm == PAL)
                    ost->st->codec->codec_tag = AV_RL32("xd55");
                else if (norm == HD60P)
                    ost->st->codec->codec_tag = AV_RL32("xd59");
                else
                    ost->st->codec->codec_tag = AV_RL32("xd5a");
            } else {
                if (ost->st->codec->interlaced) {
                    if (norm == NTSC)
                        ost->st->codec->codec_tag = AV_RL32("xd5b");
                    else
                        ost->st->codec->codec_tag = AV_RL32("xd5c");
                } else {
                    if (norm == FILM || norm == NTSC_FILM)
                        ost->st->codec->codec_tag = AV_RL32("xd5d");
                    else if (norm == PAL)
                        ost->st->codec->codec_tag = AV_RL32("xd5e");
                    else if (norm == NTSC)
                        ost->st->codec->codec_tag = AV_RL32("xd5f");
                }
            }
        }
    } else if (!strcmp(ost->target, "avcintra100") || !strcmp(ost->target, "avcintra200")) {
        if ((norm == HD50P || norm == HD60P) && !strcmp(ost->target, "avcintra100") &&
            (ost->st->codec->width != 1280 || ost->st->codec->height != 720)) {
            av_log(NULL, AV_LOG_ERROR, "Error, target %s only supports 1280x720 resolution with "
                    "this frame rate\n", ost->target);
            ffmpeg_exit(1);
        }
        if (!((ost->st->codec->width == 1920 && ost->st->codec->height == 1080) ||
              (ost->st->codec->width == 1280 && ost->st->codec->height == 720))) {
            av_log(NULL, AV_LOG_ERROR, "Error, target %s only supports 1920x1080 "
                    "or 1280x720 resolutions\n", ost->target);
            ffmpeg_exit(1);
        }
        if (!strcmp(s->oformat->name, "mov")) {
            if (!strcmp(ost->target, "avcintra200")) {
                av_log(NULL, AV_LOG_ERROR, "Error, target %s only supports MXF file format\n",
                       ost->target);
                ffmpeg_exit(1);
            }
            if (ost->st->codec->height == 1080) {
                if (ost->st->codec->interlaced) {
                    if (norm == PAL)
                        ost->st->codec->codec_tag = AV_RL32("ai15");
                    else
                        ost->st->codec->codec_tag = AV_RL32("ai16");
                } else {
                    if (norm == PAL)
                        ost->st->codec->codec_tag = AV_RL32("ai12");
                    else
                        ost->st->codec->codec_tag = AV_RL32("ai13");
                }
            } else {
                if (norm == PAL || norm == HD50P)
                    ost->st->codec->codec_tag = AV_RL32("ai1q");
                else
                    ost->st->codec->codec_tag = AV_RL32("ai1p");
            }
        }
        if (ost->st->codec->pix_fmt != PIX_FMT_YUV422P10) {
            av_log(NULL, AV_LOG_ERROR, "Error, target %s only supports yuv422p10 "
                   "compile x264 with 10bit support\n", ost->target);
            ffmpeg_exit(1);
        }
    }
}

static const char *get_codec_type(enum AVMediaType type)
{
    switch (type) {
    case AVMEDIA_TYPE_VIDEO: return "video";
    case AVMEDIA_TYPE_AUDIO: return "audio";
    case AVMEDIA_TYPE_SUBTITLE: return "subtitle";
    case AVMEDIA_TYPE_DATA: return "data";
    default: return "unknown";
    }
}

/*
 * The following code is the main loop of the file converter
 */
static int transcode(AVFormatContext **output_files,
                     int nb_output_files,
                     InputFile *input_files,
                     int nb_input_files,
                     StreamMap *stream_maps, int nb_stream_maps)
{
    int ret = 0, i, j, k, n, nb_ostreams = 0, nb_max_ostreams = 0, step;

    AVFormatContext *is, *os;
    AVCodecContext *codec, *icodec;
    OutputStream *ost, **ost_table = NULL;
    InputStream *ist = NULL;
    int key;
    int want_sdp = 1;
    uint8_t no_packet[MAX_FILES]={0};
    int no_packet_count=0;
    int *audio_mapped_streams = NULL;
    int nb_audio_mapped_streams = nb_stream_maps;
    int nb_frame_threshold[AVMEDIA_TYPE_NB]={0};
    int nb_streams[AVMEDIA_TYPE_NB]={0};

    if (rate_emu)
        for (i = 0; i < nb_input_streams; i++)
            input_streams[i].start = av_gettime();

    /* output stream init */
    nb_ostreams = 0;
    for(i=0;i<nb_output_files;i++) {
        os = output_files[i];
        if (!os->nb_streams && !(os->oformat->flags & AVFMT_NOSTREAMS)) {
            av_dump_format(output_files[i], i, output_files[i]->filename, 1);
            av_log(NULL, AV_LOG_ERROR, "Output file #%d does not contain any stream\n", i);
            ret = AVERROR(EINVAL);
            goto fail;
        }
        nb_ostreams += os->nb_streams;
        nb_max_ostreams = FFMAX(nb_max_ostreams, os->nb_streams);
    }

    /* Track streams mapped in audio channel mapping so we can count them towards
       the total of mapped streams */
    audio_mapped_streams = av_mallocz(nb_output_files*nb_max_ostreams*sizeof(int));
    if (!audio_mapped_streams)
        goto fail;

    /* Sanity check audio channel mapping */
    for (i = 0; i < nb_audio_channel_maps; i++) {
        int fi = audio_channel_maps[i].file_index;
        int si = audio_channel_maps[i].stream_index;
        int ci = audio_channel_maps[i].channel_index;
        int fo = audio_channel_maps[i].out_file_index;
        int so = audio_channel_maps[i].out_stream_index;

        /* Build array of streams touched by audio_channel_mapping */
        if (!audio_mapped_streams[fo*nb_max_ostreams+so])
            nb_audio_mapped_streams++;
        audio_mapped_streams[fo*nb_max_ostreams+so] = 1;

        if (fi < 0 || fi > nb_input_files - 1 ||
            si < 0 || si > input_files[fi].ctx->nb_streams - 1) {
            av_log(NULL, AV_LOG_ERROR, "Could not find input stream #%d.%d\n", fi, si);
            exit(1);
        }
        if (ci < 0 || ci >= input_files[fi].ctx->streams[si]->codec->channels) {
            av_log(NULL, AV_LOG_ERROR, "Could not find audio channel #%d.%d:%d\n", fi, si, ci);
            exit(1);
        }
        if (fo < 0 || fo > nb_output_files - 1 ||
            so < 0 || so > output_files[fo]->nb_streams - 1) {
            av_log(NULL, AV_LOG_ERROR, "Could not find output stream #%d.%d\n", fo, so);
            exit(1);
        }
    }

    if (nb_stream_maps > 0 && nb_audio_mapped_streams != nb_ostreams) {
        av_log(NULL, AV_LOG_ERROR, "Number of stream maps must match number of output streams\n");
        ret = AVERROR(EINVAL);
        goto fail;
    }

    /* Sanity check the mapping args -- do the input files & streams exist? */
    for(i=0;i<nb_stream_maps;i++) {
        int fi = stream_maps[i].file_index;
        int si = stream_maps[i].stream_index;

        if (fi < 0 || fi > nb_input_files - 1 ||
            si < 0 || si > input_files[fi].ctx->nb_streams - 1) {
            av_log(NULL, AV_LOG_ERROR,"Could not find input stream #%d.%d\n", fi, si);
            ret = AVERROR(EINVAL);
            goto fail;
        }
        fi = stream_maps[i].sync_file_index;
        si = stream_maps[i].sync_stream_index;
        if (fi < 0 || fi > nb_input_files - 1 ||
            si < 0 || si > input_files[fi].ctx->nb_streams - 1) {
            av_log(NULL, AV_LOG_ERROR,"Could not find sync stream #%d.%d\n", fi, si);
            ret = AVERROR(EINVAL);
            goto fail;
        }
    }

    ost_table = av_mallocz(sizeof(OutputStream *) * nb_ostreams);
    if (!ost_table)
        goto fail;

    for(k=0;k<nb_output_files;k++) {
        os = output_files[k];
        for(i=0;i<os->nb_streams;i++) {
            nb_streams[os->streams[i]->codec->codec_type]++;
        }
    }
    for(step=1<<30; step; step>>=1){
        int found_streams[AVMEDIA_TYPE_NB]={0};
        for(j=0; j<AVMEDIA_TYPE_NB; j++)
            nb_frame_threshold[j] += step;

        for(j=0; j<nb_input_streams; j++) {
            int skip=0;
            ist = &input_streams[j];
            if(opt_programid){
                int pi,si;
                AVFormatContext *f= input_files[ ist->file_index ].ctx;
                skip=1;
                for(pi=0; pi<f->nb_programs; pi++){
                    AVProgram *p= f->programs[pi];
                    if(p->id == opt_programid)
                        for(si=0; si<p->nb_stream_indexes; si++){
                            if(f->streams[ p->stream_index[si] ] == ist->st)
                                skip=0;
                        }
                }
            }
            if (ist->discard && ist->st->discard != AVDISCARD_ALL && !skip
                && nb_frame_threshold[ist->st->codec->codec_type] <= ist->st->codec_info_nb_frames){
                found_streams[ist->st->codec->codec_type]++;
            }
        }
        for(j=0; j<AVMEDIA_TYPE_NB; j++)
            if(found_streams[j] < nb_streams[j])
                nb_frame_threshold[j] -= step;
    }
    n = 0;
    for(k=0;k<nb_output_files;k++) {
        os = output_files[k];
        for(i=0;i<os->nb_streams;i++,n++) {
            int found;
            ost = ost_table[n] = output_streams_for_file[k][i];
            if (nb_stream_maps > 0 &&
                /* if we're audio mapping the stream, don't go looking for an ordinary map */
                !audio_mapped_streams[ost->file_index*nb_max_ostreams+ost->index]) {
                ost->source_index[0] = input_files[stream_maps[n].file_index].ist_index +
                    stream_maps[n].stream_index;
                ost->nb_source_indexes = 1;

                /* Sanity check that the stream types match */
                if (input_streams[ost->source_index[0]].st->codec->codec_type != ost->st->codec->codec_type) {
                    int i= ost->file_index;
                    av_dump_format(output_files[i], i, output_files[i]->filename, 1);
                    av_log(NULL, AV_LOG_ERROR, "Codec type mismatch for mapping #%d.%d (%s) -> #%d.%d (%s)\n",
                            stream_maps[n].file_index, stream_maps[n].stream_index,
                            get_codec_type(input_streams[ost->source_index[0]].st->codec->codec_type),
                            ost->file_index, ost->index, get_codec_type(ost->st->codec->codec_type));
                    ffmpeg_exit(1);
                }
            } else if (audio_mapped_streams[ost->file_index*nb_max_ostreams+ost->index]) {
                for(j=0;j<nb_audio_channel_maps;j++) {
                    AudioChannelMap *m = &audio_channel_maps[j];
                    if (ost->file_index == m->out_file_index &&
                        ost->index == m->out_stream_index) {
                        ost->source_index[ost->nb_source_indexes++] =
                            input_files[m->file_index].ist_index + m->stream_index;
                        ist = &input_streams[ost->source_index[ost->nb_source_indexes-1]];
                        /* Sanity check that the stream types match */
                        if (ist->st->codec->codec_type != ost->st->codec->codec_type) {
                            int i = ost->file_index;
                            av_dump_format(output_files[i], i, output_files[i]->filename, 1);
                            av_log(NULL, AV_LOG_ERROR, "Codec type mismatch for audio mapping #%d.%d -> #%d.%d\n",
                                    ist->file_index, ist->st->index, ost->file_index, ost->index);
                            ffmpeg_exit(1);
                        }
                        ost->audio_channel_maps[ost->nb_audio_channel_maps++] = m;
                        if (m->out_channel_index == -1)
                            m->out_channel_index = ost->audiomerge.out_channels;
                        ost->audiomerge.out_channels++;
                    }
                }
                /* Sanity check out audio channel number */
                for(j=0;j<ost->nb_audio_channel_maps;j++) {
                    AudioChannelMap *m = ost->audio_channel_maps[j];
                    if (m->out_channel_index >= ost->audiomerge.out_channels) {
                        av_log(NULL, AV_LOG_ERROR, "Channel number %d does not exist: channels %d\n",
                                m->out_channel_index, ost->audiomerge.out_channels);
                        ffmpeg_exit(1);
                    }
                }
            } else {
                /* get corresponding input stream index : we select the first one with the right type */
                found = 0;
                for (j = 0; j < nb_input_streams; j++) {
                    int skip=0;
                    ist = &input_streams[j];
                    if(opt_programid){
                        int pi,si;
                        AVFormatContext *f = input_files[ist->file_index].ctx;
                        skip=1;
                        for(pi=0; pi<f->nb_programs; pi++){
                            AVProgram *p= f->programs[pi];
                            if(p->id == opt_programid)
                                for(si=0; si<p->nb_stream_indexes; si++){
                                    if(f->streams[ p->stream_index[si] ] == ist->st)
                                        skip=0;
                                }
                        }
                    }
                    if (ist->discard && ist->st->discard != AVDISCARD_ALL && !skip &&
                        ist->st->codec->codec_type == ost->st->codec->codec_type &&
                        nb_frame_threshold[ist->st->codec->codec_type] <= ist->st->codec_info_nb_frames) {
                            ost->source_index[0] = j;
                            ost->nb_source_indexes = 1;
                            found = 1;
                            break;
                    }
                }

                if (!found) {
                    if(! opt_programid) {
                        /* try again and reuse existing stream */
                        for (j = 0; j < nb_input_streams; j++) {
                            ist = &input_streams[j];
                            if (   ist->st->codec->codec_type == ost->st->codec->codec_type
                                && ist->st->discard != AVDISCARD_ALL) {
                                ost->source_index[0] = j;
                                ost->nb_source_indexes = 1;
                                found = 1;
                            }
                        }
                    }
                    if (!found) {
                        av_log(NULL, AV_LOG_ERROR, "Could not find any %s input stream for output stream #%d.%d\n",
                                get_codec_type(ost->st->codec->codec_type), ost->file_index, ost->index);
                        ffmpeg_exit(1);
                    }
                }
            }
            for(j=0;j<ost->nb_source_indexes;j++) {
                ist = &input_streams[ost->source_index[j]];
                ist->discard = 0;
            }
            if (!ist) {
                av_log(NULL, AV_LOG_ERROR, "Could not find input stream matching output stream #%d.%d\n",
                        ost->file_index, ost->index);
                ffmpeg_exit(1);
            }
            ost->sync_ist = (nb_stream_maps > 0 &&
                             /* if we're audio_mapping the stream, don't go looking for an ordinary map */
                             !audio_mapped_streams[ost->file_index*nb_max_ostreams+ost->index]) ?
                &input_streams[input_files[stream_maps[n].sync_file_index].ist_index +
                               stream_maps[n].sync_stream_index] : &input_streams[ost->source_index[0]];
        }
    }

    av_freep(&audio_mapped_streams);

    /* for each output stream, we compute the right encoding parameters */
    for(i=0;i<nb_ostreams;i++) {
        ost = ost_table[i];
        os = output_files[ost->file_index];
        ist = &input_streams[ost->source_index[0]];

        codec = ost->st->codec;
        icodec = ist->st->codec;

        if (metadata_streams_autocopy)
            av_dict_copy(&ost->st->metadata, ist->st->metadata,
                         AV_DICT_DONT_OVERWRITE);

        ost->st->disposition = ist->st->disposition;
        codec->bits_per_raw_sample= icodec->bits_per_raw_sample;
        codec->chroma_sample_location = icodec->chroma_sample_location;

        if (ost->st->stream_copy) {
            if (icodec->extradata_size > 0) {
                uint64_t extra_size = (uint64_t)icodec->extradata_size + FF_INPUT_BUFFER_PADDING_SIZE;
                if (extra_size > INT_MAX)
                    goto fail;
                codec->extradata = av_mallocz(extra_size);
                if (!codec->extradata)
                    goto fail;
                memcpy(codec->extradata, icodec->extradata, icodec->extradata_size);
                codec->extradata_size = icodec->extradata_size;
            }

            /* generate pts for source file */
            input_files[ist->file_index].ctx->flags |= AVFMT_FLAG_GENPTS;

            /* if stream_copy is selected, no need to decode or encode */
            codec->codec_id = icodec->codec_id;
            codec->codec_type = icodec->codec_type;

            codec->interlaced = icodec->interlaced;

            if(!codec->codec_tag){
                if(   !os->oformat->codec_tag
                   || av_codec_get_id (os->oformat->codec_tag, icodec->codec_tag) == codec->codec_id
                   || av_codec_get_tag(os->oformat->codec_tag, icodec->codec_id) <= 0)
                    codec->codec_tag = icodec->codec_tag;
            }

            codec->bit_rate = icodec->bit_rate;
            codec->rc_max_rate    = icodec->rc_max_rate;
            codec->rc_buffer_size = icodec->rc_buffer_size;

            codec->time_base = ist->st->time_base;
            if (!(os->oformat->flags & AVFMT_VARIABLE_FPS)) {
                if (!copy_tb) {
                    if (av_q2d(ist->st->r_frame_rate) <= 60) {
                        codec->time_base.num = ist->st->r_frame_rate.den;
                        codec->time_base.den = ist->st->r_frame_rate.num;
                    } else if (av_q2d(icodec->time_base)*icodec->ticks_per_frame > av_q2d(ist->st->time_base)) {
                        codec->time_base = icodec->time_base;
                        codec->time_base.num *= icodec->ticks_per_frame;
                    }
                }
            }
            av_reduce(&codec->time_base.num, &codec->time_base.den,
                        codec->time_base.num, codec->time_base.den, INT_MAX);

            switch(codec->codec_type) {
            case AVMEDIA_TYPE_AUDIO:
                if(audio_volume != 256) {
                    av_log(NULL, AV_LOG_ERROR,"-acodec copy and -vol are incompatible (frames are not decoded)\n");
                    ffmpeg_exit(1);
                }
                codec->channel_layout = icodec->channel_layout;
                codec->sample_rate = icodec->sample_rate;
                codec->channels = icodec->channels;
                codec->sample_fmt = icodec->sample_fmt;
                codec->frame_size = icodec->frame_size;
                codec->audio_service_type = icodec->audio_service_type;
                codec->block_align= icodec->block_align;
                if(codec->block_align == 1 && codec->codec_id == CODEC_ID_MP3)
                    codec->block_align= 0;
                if(codec->codec_id == CODEC_ID_AC3)
                    codec->block_align= 0;
                if (ost->target)
                    validate_audio_target(os, ost);
                break;
            case AVMEDIA_TYPE_VIDEO:
                codec->pix_fmt = icodec->pix_fmt;
                codec->color_primaries = icodec->color_primaries;
                codec->color_transfer = icodec->color_transfer;
                codec->color_matrix = icodec->color_matrix;
                codec->width = icodec->width;
                codec->height = icodec->height;
                codec->has_b_frames = icodec->has_b_frames;
                codec->bits_per_coded_sample = icodec->bits_per_coded_sample;
                if (!codec->sample_aspect_ratio.num) {
                    codec->sample_aspect_ratio =
                    ost->st->sample_aspect_ratio =
                        ist->st->sample_aspect_ratio.num ? ist->st->sample_aspect_ratio :
                        ist->st->codec->sample_aspect_ratio.num ?
                        ist->st->codec->sample_aspect_ratio : (AVRational){0, 1};
                }
                if (ost->target)
                    validate_video_target(os, ost);
                break;
            case AVMEDIA_TYPE_SUBTITLE:
                codec->width = icodec->width;
                codec->height = icodec->height;
                break;
            case AVMEDIA_TYPE_DATA:
                break;
            default:
                abort();
            }
        } else {
            if (!ost->enc)
                ost->enc = avcodec_find_encoder(ost->st->codec->codec_id);
            switch(codec->codec_type) {
            case AVMEDIA_TYPE_AUDIO:
                ost->fifo= av_fifo_alloc(1024);
                if(!ost->fifo)
                    goto fail;
                ost->reformat_pair = MAKE_SFMT_PAIR(AV_SAMPLE_FMT_NONE,AV_SAMPLE_FMT_NONE);
                if (!codec->sample_rate) {
                    codec->sample_rate = icodec->sample_rate;
                }
                choose_sample_rate(ost->st, ost->enc);
                codec->time_base = (AVRational){1, codec->sample_rate};
                if (codec->sample_fmt == AV_SAMPLE_FMT_NONE)
                    codec->sample_fmt = icodec->sample_fmt;
                choose_sample_fmt(ost->st, ost->enc);
                if (!codec->channels) {
                    codec->channels = icodec->channels;
                    codec->channel_layout = icodec->channel_layout;
                }
                if (av_get_channel_layout_nb_channels(codec->channel_layout) != codec->channels)
                    codec->channel_layout = 0;
                ost->audio_resample = codec->sample_rate != icodec->sample_rate || audio_sync_method > 1;
                icodec->request_channels = codec->channels;
                for (j = 0; j < ost->nb_source_indexes; j++) {
                    ist = &input_streams[ost->source_index[j]];
                    icodec = ist->st->codec;
                    ist->decoding_needed = 1;
                }
                ost->encoding_needed = 1;
                ost->is_start = 1;
                ost->resample_sample_fmt  = icodec->sample_fmt;
                ost->resample_sample_rate = icodec->sample_rate;
                ost->resample_channels    = icodec->channels;

                if (ost->audiomerge.out_channels > 0) {
                    codec->channels = ost->audiomerge.out_channels; // update channels to merged channels
                    if (audiomerge_init(&ost->audiomerge, ost->audiomerge.out_channels,
                                        av_get_bytes_per_sample(icodec->sample_fmt)) < 0) {
                        av_log(NULL, AV_LOG_ERROR, "Audiomerge initialization failed\n");
                        ffmpeg_exit(1);
                    }

                }
                if (av_get_channel_layout_nb_channels(codec->channel_layout) != codec->channels)
                    codec->channel_layout = 0;

                if (ost->target)
                    validate_audio_target(os, ost);
                break;
            case AVMEDIA_TYPE_VIDEO:
                if (codec->pix_fmt == PIX_FMT_NONE)
                    codec->pix_fmt = icodec->pix_fmt;
                choose_pixel_fmt(ost->st, ost->enc);

                if (ost->st->codec->pix_fmt == PIX_FMT_NONE) {
                    av_log(NULL, AV_LOG_ERROR, "Video pixel format is unknown, stream cannot be encoded\n");
                    ffmpeg_exit(1);
                }

                if (!codec->width || !codec->height) {
                    codec->width  = icodec->width;
                    codec->height = icodec->height;
                }

                ost->video_resample = codec->width   != icodec->width  ||
                                      codec->height  != icodec->height ||
                                      codec->pix_fmt != icodec->pix_fmt;
                if (ost->video_resample) {
                    codec->bits_per_raw_sample= frame_bits_per_raw_sample;
                }

                ost->resample_height = icodec->height;
                ost->resample_width  = icodec->width;
                ost->resample_pix_fmt= icodec->pix_fmt;
                ost->encoding_needed = 1;
                ist->decoding_needed = 1;

                if (!ost->frame_rate.num) {
                    if (ist->frame_rate.num) {
                        ost->frame_rate = ist->frame_rate;
                    } else if (av_q2d(ist->st->r_frame_rate) <= 60) {
                        /* update the current frame rate to match the stream frame rate */
                        ost->frame_rate = ist->st->r_frame_rate;
                    } else {
                        ost->frame_rate = ist->st->avg_frame_rate;
                        if (!ost->frame_rate.num) {
                            ost->frame_rate.num = ist->st->codec->time_base.den;
                            ost->frame_rate.den = ist->st->codec->time_base.num;
                        }
                    }
                }
                if (!ost->frame_rate.num) {
                    av_log(NULL, AV_LOG_ERROR, "Error: no frame rate specified\n");
                    exit(1);
                }
                if (!ost->force_fps && !ist->frame_rate.num) {
                    AVRational near_fps =
                        frame_rate_tab[av_find_nearest_q_idx(ost->frame_rate, frame_rate_tab)];
                    if (fabs(av_q2d(near_fps) - av_q2d(ost->frame_rate)) >= 0.01)
                        av_log(NULL, AV_LOG_WARNING, "Adjusting fps from %d/%d to %d/%d\n",
                                ost->frame_rate.num, ost->frame_rate.den, near_fps.num, near_fps.den);
                    ost->frame_rate = near_fps;
                    if (ost->enc && ost->enc->supported_framerates) {
                        int idx = av_find_nearest_q_idx(ost->frame_rate, ost->enc->supported_framerates);
                        ost->frame_rate = ost->enc->supported_framerates[idx];
                    }
                }
                codec->time_base = (AVRational){ost->frame_rate.den, ost->frame_rate.num};
                if(   av_q2d(codec->time_base) < 0.001 && video_sync_method
                   && (video_sync_method==1 || (video_sync_method<0 && !(os->oformat->flags & AVFMT_VARIABLE_FPS)))){
                    av_log(os, AV_LOG_WARNING, "Frame rate very high for a muxer not effciciently supporting it.\n"
                                               "Please consider specifiying a lower framerate, a different muxer or -vsync 2\n");
                }
#if CONFIG_AVFILTER
                if (configure_video_filters(ist, ost)) {
                    av_log(NULL, AV_LOG_ERROR, "Error opening filters!\n");
                    exit(1);
                }
#endif
                if (ost->target)
                    validate_video_target(os, ost);
                break;
            case AVMEDIA_TYPE_SUBTITLE:
                ost->encoding_needed = 1;
                ist->decoding_needed = 1;
                break;
            default:
                abort();
                break;
            }
            /* two pass mode */
            if (ost->encoding_needed && codec->codec_id != CODEC_ID_H264 &&
                (codec->flags & (CODEC_FLAG_PASS1 | CODEC_FLAG_PASS2))) {
                char logfilename[1024];
                FILE *f;

                snprintf(logfilename, sizeof(logfilename), "%s-%d.log",
                         pass_logfilename_prefix ? pass_logfilename_prefix : DEFAULT_PASS_LOGFILENAME_PREFIX,
                         i);
                if (codec->flags & CODEC_FLAG_PASS1) {
                    f = fopen(logfilename, "wb");
                    if (!f) {
                        av_log(NULL, AV_LOG_ERROR, "Cannot write log file '%s' for pass-1 encoding: %s\n", logfilename, strerror(errno));
                        ffmpeg_exit(1);
                    }
                    ost->logfile = f;
                } else {
                    char  *logbuffer;
                    size_t logbuffer_size;
                    if (read_file(logfilename, &logbuffer, &logbuffer_size) < 0) {
                        av_log(NULL, AV_LOG_ERROR, "Error reading log file '%s' for pass-2 encoding\n", logfilename);
                        ffmpeg_exit(1);
                    }
                    codec->stats_in = logbuffer;
                }
            }
        }
        if(codec->codec_type == AVMEDIA_TYPE_VIDEO){
            /* maximum video buffer size is 6-bytes per pixel, plus DPX header size */
            int size= codec->width * codec->height;
            bit_buffer_size= FFMAX(bit_buffer_size, 6*size + 1664);
        }
    }

    if (!bit_buffer)
        bit_buffer = av_malloc(bit_buffer_size);
    if (!bit_buffer) {
        av_log(NULL, AV_LOG_ERROR, "Cannot allocate %d bytes output buffer\n",
                bit_buffer_size);
        ret = AVERROR(ENOMEM);
        goto fail;
    }

    /* open each encoder */
    for(i=0;i<nb_ostreams;i++) {
        ost = ost_table[i];
        if (ost->encoding_needed) {
            AVCodec *codec = ost->enc;
            AVCodecContext *dec = input_streams[ost->source_index[0]].st->codec;
            if (!codec) {
                av_log(NULL, AV_LOG_ERROR, "Encoder (codec id %d) not found for output stream #%d.%d\n",
                         ost->st->codec->codec_id, ost->file_index, ost->index);
                ret = AVERROR(EINVAL);
                goto fail;
            }
            if (dec->subtitle_header) {
                ost->st->codec->subtitle_header = av_malloc(dec->subtitle_header_size);
                if (!ost->st->codec->subtitle_header) {
                    ret = AVERROR(ENOMEM);
                    goto fail;
                }
                memcpy(ost->st->codec->subtitle_header, dec->subtitle_header, dec->subtitle_header_size);
                ost->st->codec->subtitle_header_size = dec->subtitle_header_size;
            }
            if (avcodec_open2(ost->st->codec, codec, &ost->opts) < 0) {
                av_log(NULL, AV_LOG_ERROR, "Error while opening encoder for output stream #%d.%d - maybe incorrect parameters such as bit_rate, rate, width or height\n",
                        ost->file_index, ost->index);
                ret = AVERROR(EINVAL);
                goto fail;
            }
            // validate interlaced configuration
            if (ost->st->codec->codec_type == AVMEDIA_TYPE_VIDEO &&
                ost->st->codec->flags & (CODEC_FLAG_INTERLACED_DCT|CODEC_FLAG_INTERLACED_ME) &&
                ost->st->codec->interlaced < 1) {
                av_log(NULL, AV_LOG_ERROR, "Error: interlaced flags are set but interlacing is not specified, use -tff or -bff\n");
                ret = AVERROR(EINVAL);
                goto fail;
            }
            assert_codec_experimental(ost->st->codec, 1);
            assert_avoptions(ost->opts);
            if (ost->st->codec->bit_rate && ost->st->codec->bit_rate < 1000)
                av_log(NULL, AV_LOG_WARNING, "The bitrate parameter is set too low."
                                             "It takes bits/s as argument, not kbits/s\n");
            extra_size += ost->st->codec->extradata_size;
        }
    }

    /* open each decoder */
    for (i = 0; i < nb_input_streams; i++) {
        ist = &input_streams[i];
        if (ist->decoding_needed) {
            AVCodec *codec = ist->dec;
            if (!codec)
                codec = avcodec_find_decoder(ist->st->codec->codec_id);
            if (!codec) {
                av_log(NULL, AV_LOG_ERROR, "Decoder (codec id %d) not found for input stream #%d.%d\n",
                        ist->st->codec->codec_id, ist->file_index, ist->st->index);
                ret = AVERROR(EINVAL);
                goto fail;
            }
            if (avcodec_open2(ist->st->codec, codec, &ist->opts) < 0) {
                av_log(NULL, AV_LOG_ERROR, "Error while opening decoder for input stream #%d.%d\n",
                        ist->file_index, ist->st->index);
                ret = AVERROR(EINVAL);
                goto fail;
            }
            assert_codec_experimental(ist->st->codec, 0);
            //if (ist->st->codec->codec_type == AVMEDIA_TYPE_VIDEO)
            //    ist->st->codec->flags |= CODEC_FLAG_REPEAT_FIELD;
        }
    }

    /* init pts */
    for (i = 0; i < nb_input_streams; i++) {
        AVStream *st;
        ist = &input_streams[i];
        st= ist->st;
        is = input_files[ist->file_index].ctx;
        ist->pts = 0;
        ist->dts = ist->next_pts = AV_NOPTS_VALUE;
        if (st->codec->codec_type == AVMEDIA_TYPE_VIDEO &&
            strcmp(is->iformat->name, "mpeg") &&
            strcmp(is->iformat->name, "mpegts") &&
            strcmp(is->iformat->name, "ffm") &&
            strcmp(is->iformat->name, "nut") &&
            strcmp(is->iformat->name, "h264") &&
            strncmp(is->iformat->name, "matroska", 8) &&
            strncmp(is->iformat->name, "mov", 3)) {
            ist->dts_is_reordered_pts = 1;
        }
    }

    /* set meta data information from input file if required */
    for (i=0;i<nb_meta_data_maps;i++) {
        AVFormatContext *files[2];
        AVDictionary    **meta[2];
        int j;

#define METADATA_CHECK_INDEX(index, nb_elems, desc)\
        if ((index) < 0 || (index) >= (nb_elems)) {\
            av_log(NULL, AV_LOG_ERROR, "Invalid %s index %d while processing metadata maps\n",\
                     (desc), (index));\
            ret = AVERROR(EINVAL);\
            goto fail;\
        }

        int out_file_index = meta_data_maps[i][0].file;
        int in_file_index = meta_data_maps[i][1].file;
        if (in_file_index < 0 || out_file_index < 0)
            continue;
        METADATA_CHECK_INDEX(out_file_index, nb_output_files, "output file")
        METADATA_CHECK_INDEX(in_file_index, nb_input_files, "input file")

        files[0] = output_files[out_file_index];
        files[1] = input_files[in_file_index].ctx;

        for (j = 0; j < 2; j++) {
            MetadataMap *map = &meta_data_maps[i][j];

            switch (map->type) {
            case 'g':
                meta[j] = &files[j]->metadata;
                break;
            case 's':
                METADATA_CHECK_INDEX(map->index, files[j]->nb_streams, "stream")
                meta[j] = &files[j]->streams[map->index]->metadata;
                break;
            case 'c':
                METADATA_CHECK_INDEX(map->index, files[j]->nb_chapters, "chapter")
                meta[j] = &files[j]->chapters[map->index]->metadata;
                break;
            case 'p':
                METADATA_CHECK_INDEX(map->index, files[j]->nb_programs, "program")
                meta[j] = &files[j]->programs[map->index]->metadata;
                break;
            }
        }

        av_dict_copy(meta[0], *meta[1], AV_DICT_DONT_OVERWRITE);
    }

    /* copy chapters according to chapter maps */
    for (i = 0; i < nb_chapter_maps; i++) {
        int infile  = chapter_maps[i].in_file;
        int outfile = chapter_maps[i].out_file;

        if (infile < 0 || outfile < 0)
            continue;
        if (infile >= nb_input_files) {
            av_log(NULL, AV_LOG_ERROR, "Invalid input file index %d in chapter mapping.\n", infile);
            ret = AVERROR(EINVAL);
            goto fail;
        }
        if (outfile >= nb_output_files) {
            av_log(NULL, AV_LOG_ERROR, "Invalid output file index %d in chapter mapping.\n",outfile);
            ret = AVERROR(EINVAL);
            goto fail;
        }
        copy_chapters(infile, outfile);
    }

    /* open files and write file headers */
    for(i=0;i<nb_output_files;i++) {
        os = output_files[i];
        if (avformat_write_header(os, &output_opts[i]) < 0) {
            av_log(NULL, AV_LOG_ERROR, "Could not write header for output file #%d\n", i);
            ret = AVERROR(EINVAL);
            goto fail;
        }
//        assert_avoptions(output_opts[i]);
        if (strcmp(output_files[i]->oformat->name, "rtp")) {
            want_sdp = 0;
        }
    }

    /* dump the file output parameters - cannot be done before in case
       of stream copy */
    for(i=0;i<nb_output_files;i++) {
        av_dump_format(output_files[i], i, output_files[i]->filename, 1);
    }

    /* dump the stream mapping */
    if (verbose >= 0) {
        av_log(NULL, AV_LOG_INFO, "Stream mapping:\n");
        for(i=0;i<nb_ostreams;i++) {
            ost = ost_table[i];
            for(j=0;j<ost->nb_source_indexes;j++) {
            av_log(NULL, AV_LOG_INFO, "  Stream #%d.%d -> #%d.%d",
                    input_streams[ost->source_index[j]].file_index,
                    input_streams[ost->source_index[j]].st->index,
                    ost->file_index,
                    ost->index);
            if (ost->nb_audio_channel_maps > 0) {
                av_log(NULL, AV_LOG_INFO, " [channel: %d -> %d]",
                        ost->audio_channel_maps[j]->channel_index,
                        ost->audio_channel_maps[j]->out_channel_index);
            }
            if (ost->sync_ist != &input_streams[ost->source_index[0]])
                av_log(NULL, AV_LOG_INFO, " [sync #%d.%d]",
                        ost->sync_ist->file_index,
                        ost->sync_ist->st->index);
            av_log(NULL, AV_LOG_INFO, "\n");
            }
        }
    }

    if (want_sdp) {
        print_sdp(output_files, nb_output_files);
    }

    if (!using_stdin) {
        if(verbose >= 0)
            av_log(NULL, AV_LOG_INFO, "Press [q] to stop, [?] for help\n");
        avio_set_interrupt_cb(decode_interrupt_cb);
    }
    // if stderr is not a tty increase report interval
    if (!isatty(2))
        update_interval = 2000000;

    term_init();

    timer_start = av_gettime();

    for(; received_sigterm == 0;) {
        int file_index, ist_index;
        AVPacket pkt;
        double ipts_min;
        double opts_min;
        int64_t duration;

    redo:
        ipts_min= 1e100;
        opts_min= 1e100;
        /* if 'q' pressed, exits */
        if (!using_stdin) {
            if (q_pressed)
                break;
            /* read_key() returns 0 on EOF */
            key = read_key();
            if (key == 'q')
                break;
            if (key == '+') verbose++;
            if (key == '-') verbose--;
            if (key == 's') qp_hist     ^= 1;
            if (key == 'h'){
                if (do_hex_dump){
                    do_hex_dump = do_pkt_dump = 0;
                } else if(do_pkt_dump){
                    do_hex_dump = 1;
                } else
                    do_pkt_dump = 1;
                av_log_set_level(AV_LOG_DEBUG);
            }
            if (key == 'd' || key == 'D'){
                int debug=0;
                if(key == 'D') {
                    debug = input_streams[0].st->codec->debug<<1;
                    if(!debug) debug = 1;
                    while(debug & (FF_DEBUG_DCT_COEFF|FF_DEBUG_VIS_QP|FF_DEBUG_VIS_MB_TYPE)) //unsupported, would just crash
                        debug += debug;
                }else
                    scanf("%d", &debug);
                for(i=0;i<nb_input_streams;i++) {
                    input_streams[i].st->codec->debug = debug;
                }
                for(i=0;i<nb_ostreams;i++) {
                    ost = ost_table[i];
                    ost->st->codec->debug = debug;
                }
                if(debug) av_log_set_level(AV_LOG_DEBUG);
                av_log(NULL, AV_LOG_INFO,"debug=%d\n", debug);
            }
            if (key == '?'){
                av_log(NULL, AV_LOG_INFO, "key    function\n"
                                "?      show this help\n"
                                "+      increase verbosity\n"
                                "-      decrease verbosity\n"
                                "D      cycle through available debug modes\n"
                                "h      dump packets/hex press to cycle through the 3 states\n"
                                "q      quit\n"
                                "s      Show QP histogram\n"
                );
            }
        }

        /* select the stream that we must read now by looking at the
           smallest output pts */
        file_index = -1;
        for(i=0;i<nb_ostreams;i++) {
            double ipts, opts;
            ost = ost_table[i];
            os = output_files[ost->file_index];
            for(j=0;j<ost->nb_source_indexes;j++) {
            ist = &input_streams[ost->source_index[j]];
            if(ist->is_past_recording_time || no_packet[ist->file_index])
                continue;
            opts = ost->st->pts.val * av_q2d(ost->st->time_base);
            ipts = (double)ist->pts;
            if (!input_files[ist->file_index].eof_reached){
                if(ipts < ipts_min) {
                    ipts_min = ipts;
                    if(input_sync || ost->nb_source_indexes > 1) file_index = ist->file_index;
                }
                if(opts < opts_min) {
                    opts_min = opts;
                    if(!input_sync && ost->nb_source_indexes == 1) file_index = ist->file_index;
                }
            }
            if(ost->frame_number >= max_frames[ost->st->codec->codec_type]){
                file_index= -1;
                goto out;
            }
            }
        }
        /* if none, if is finished */
        if (file_index < 0) {
        out:
            if(no_packet_count){
                no_packet_count=0;
                memset(no_packet, 0, sizeof(no_packet));
                usleep(10000);
                continue;
            }
            break;
        }

        /* finish if limit size exhausted */
        if (limit_filesize != 0 && limit_filesize <= avio_tell(output_files[0]->pb))
            break;

        /* read a frame from it and output it in the fifo */
        is = input_files[file_index].ctx;
        ret= av_read_frame(is, &pkt);
        if(ret == AVERROR(EAGAIN)){
            no_packet[file_index]=1;
            no_packet_count++;
            continue;
        }
        if (ret < 0) {
            input_files[file_index].eof_reached = 1;
            if (opt_shortest)
                break;
            else
                continue;
        }

        // FIXME -shortest
        if (recording_time != INT64_MAX)
            duration = recording_time;
        else if (is->duration > 0)
            duration = is->duration;
        else
            duration = 0;

        no_packet_count=0;
        memset(no_packet, 0, sizeof(no_packet));

        if (do_pkt_dump) {
            av_pkt_dump_log2(NULL, AV_LOG_DEBUG, &pkt, do_hex_dump,
                             is->streams[pkt.stream_index]);
        }
        /* the following test is needed in case new streams appear
           dynamically in stream : we ignore them */
        if (pkt.stream_index >= input_files[file_index].ctx->nb_streams)
            goto discard_packet;
        ist_index = input_files[file_index].ist_index + pkt.stream_index;
        if (ist_index >= nb_input_streams)
            goto discard_packet;
        ist = &input_streams[ist_index];
        if (ist->discard)
            goto discard_packet;

        if (pkt.dts != AV_NOPTS_VALUE)
            pkt.dts += av_rescale_q(input_files[ist->file_index].ts_offset, AV_TIME_BASE_Q, ist->st->time_base);
        if (pkt.pts != AV_NOPTS_VALUE)
            pkt.pts += av_rescale_q(input_files[ist->file_index].ts_offset, AV_TIME_BASE_Q, ist->st->time_base);

        if (ist->ts_scale) {
            if(pkt.pts != AV_NOPTS_VALUE)
                pkt.pts *= ist->ts_scale;
            if(pkt.dts != AV_NOPTS_VALUE)
                pkt.dts *= ist->ts_scale;
        }

        //fprintf(stderr, "st:%d prevdts:%"PRId64" dts:%"PRId64" off:%"PRId64" %d\n", pkt.stream_index, ist->dts, pkt.dts, input_files_ts_offset[ist->file_index], ist->st->codec->codec_type);
        if (pkt.dts != AV_NOPTS_VALUE && ist->dts != AV_NOPTS_VALUE
            && (is->iformat->flags & AVFMT_TS_DISCONT)) {
            int64_t pkt_dts= av_rescale_q(pkt.dts, ist->st->time_base, AV_TIME_BASE_Q);
            int64_t delta = pkt_dts - av_rescale_q(ist->dts, ist->st->time_base, AV_TIME_BASE_Q);
            if(FFABS(delta) > dts_delta_threshold*AV_TIME_BASE && !copy_ts){
                input_files[ist->file_index].ts_offset -= delta;
                if (verbose > 2)
                    av_log(NULL, AV_LOG_INFO, "timestamp discontinuity %"PRId64", new offset= %"PRId64"\n",
                            delta, input_files[ist->file_index].ts_offset);
                pkt.dts-= av_rescale_q(delta, AV_TIME_BASE_Q, ist->st->time_base);
                if(pkt.pts != AV_NOPTS_VALUE)
                    pkt.pts-= av_rescale_q(delta, AV_TIME_BASE_Q, ist->st->time_base);
            }
        }
        ist->dts = pkt.dts;

        //fprintf(stderr,"read #%d.%d size=%d\n", ist->file_index, ist->index, pkt.size);
        if (output_packet(ist, ist_index, ost_table, nb_ostreams, &pkt) < 0) {
            if (verbose >= 0)
                av_log(NULL, AV_LOG_ERROR, "Error while decoding stream #%d.%d\n",
                        ist->file_index, ist->st->index);
            if (exit_on_error) {
                print_report(output_files, ost_table, nb_ostreams, 1, duration);
                ffmpeg_exit(1);
            }
            av_free_packet(&pkt);
            goto redo;
        }

    discard_packet:
        if (ist && ist->st->codec->codec_id == CODEC_ID_RAWVIDEO) {
            if (ist->pkt_data_to_free)
                av_free(ist->pkt_data_to_free);
            ist->pkt_data_to_free = pkt.data;
            pkt.destruct = NULL;
        }
        av_free_packet(&pkt);

        /* dump report by using the output first video and audio streams */
        print_report(output_files, ost_table, nb_ostreams, 0, duration);
    }

    /* at the end of stream, we must flush the decoder buffers */
    for (i = 0; i < nb_input_streams; i++) {
        ist = &input_streams[i];
        if (ist->decoding_needed) {
            output_packet(ist, i, ost_table, nb_ostreams, NULL);
        }
    }

    /* write the trailer if needed and close file */
    for(i=0;i<nb_output_files;i++) {
        os = output_files[i];
        av_write_trailer(os);
    }

    /* dump report by using the first video and audio streams */
    print_report(output_files, ost_table, nb_ostreams, 1, 0);

    term_exit();

    /* close each encoder */
    for(i=0;i<nb_ostreams;i++) {
        ost = ost_table[i];
        if (ost->encoding_needed) {
            av_freep(&ost->st->codec->stats_in);
            avcodec_close(ost->st->codec);
        }
#if CONFIG_AVFILTER
        avfilter_graph_free(&ost->graph);
#endif
    }

    /* close each decoder */
    for (i = 0; i < nb_input_streams; i++) {
        ist = &input_streams[i];
        if (ist->decoding_needed) {
            avcodec_close(ist->st->codec);
        }
    }

    /* finished ! */
    ret = 0;

 fail:
    av_freep(&bit_buffer);

    if (ost_table) {
        for(i=0;i<nb_ostreams;i++) {
            ost = ost_table[i];
            if (ost) {
                AVBitStreamFilterContext *bsfp;
                if (ost->st->stream_copy)
                    av_freep(&ost->st->codec->extradata);
                if (ost->logfile) {
                    fclose(ost->logfile);
                    ost->logfile = NULL;
                }
                av_fifo_free(ost->fifo); /* works even if fifo is not
                                             initialized but set to zero */
                av_freep(&ost->st->codec->subtitle_header);
                av_free(ost->resample_frame.data[0]);
                if (ost->prev_picref)
                    avfilter_unref_buffer(ost->prev_picref);
                if (ost->free_prev_frame)
                    av_free(ost->prev_frame.data[0]);
                av_free(ost->forced_kf_pts);
                av_free(ost->audiomerge.buf);
                if (ost->video_resample)
                    sws_freeContext(ost->img_resample_ctx);
                if (ost->resample)
                    audio_resample_close(ost->resample);
                for (bsfp = ost->bitstream_filters; bsfp;) {
                    AVBitStreamFilterContext *tmp = bsfp;
                    bsfp = bsfp->next;
                    av_bitstream_filter_close(tmp);
                }
                if (ost->reformat_ctx)
                    av_audio_convert_free(ost->reformat_ctx);
                av_dict_free(&ost->opts);
                av_free(ost);
            }
        }
        av_free(ost_table);
    }
    return ret;
}

static int opt_format(const char *opt, const char *arg)
{
    last_asked_format = arg;
    return 0;
}

static int opt_video_rc_override_string(const char *opt, const char *arg)
{
    video_rc_override_string = arg;
    return 0;
}

static int opt_verbose(const char *opt, const char *arg)
{
    verbose = parse_number_or_die(opt, arg, OPT_INT64, -10, 10);
    return 0;
}

static int opt_frame_rate(const char *opt, const char *arg)
{
    if (av_parse_video_rate(&frame_rate, arg) < 0) {
        av_log(NULL, AV_LOG_ERROR, "Incorrect value for %s: %s\n", opt, arg);
        ffmpeg_exit(1);
    }
    return 0;
}

static int opt_frame_crop(const char *opt, const char *arg)
{
    av_log(NULL, AV_LOG_ERROR, "Option '%s' has been removed, use the crop filter instead\n", opt);
    return AVERROR(EINVAL);
}

static int opt_frame_size(const char *opt, const char *arg)
{
    if (av_parse_video_size(&frame_width, &frame_height, arg) < 0) {
        av_log(NULL, AV_LOG_ERROR, "Incorrect frame size\n");
        return AVERROR(EINVAL);
    }
    return 0;
}

static int opt_pad(const char *opt, const char *arg) {
    av_log(NULL, AV_LOG_ERROR, "Option '%s' has been removed, use the pad filter instead\n", opt);
    return -1;
}

static int opt_frame_pix_fmt(const char *opt, const char *arg)
{
    if (strcmp(arg, "list")) {
        frame_pix_fmt = av_get_pix_fmt(arg);
        if (frame_pix_fmt == PIX_FMT_NONE) {
            av_log(NULL, AV_LOG_ERROR, "Unknown pixel format requested: %s\n", arg);
            return AVERROR(EINVAL);
        }
    } else {
        opt_pix_fmts(NULL, NULL);
        ffmpeg_exit(0);
    }
    return 0;
}

static int opt_frame_aspect_ratio(const char *opt, const char *arg)
{
    int x = 0, y = 0;
    const char *p;
    char *end;

    p = strchr(arg, ':');
    if (p) {
        x = strtol(arg, &end, 10);
        if (end == p)
            y = strtol(end+1, &end, 10);
        if (x > 0 && y > 0) {
            frame_aspect_ratio.num = x;
            frame_aspect_ratio.den = y;
        }
    } else
        frame_aspect_ratio = av_d2q(strtod(arg, NULL), 255);

    if (!frame_aspect_ratio.num || !frame_aspect_ratio.den) {
        av_log(NULL, AV_LOG_ERROR, "Incorrect aspect ratio specification.\n");
        return AVERROR(EINVAL);
    }
    return 0;
}

static int opt_cover_file(const char *opt, const char *arg)
{
    FILE *cover_file;
    uint8_t *buf;
    int file_size;
    const char *mime = NULL;
    AVDictionaryEntry *tag;
    char *ext = strrchr(arg, '.');
    if (!ext) {
        av_log(NULL, AV_LOG_ERROR, "Could not find file extension to guess mime type\n");
        ffmpeg_exit(1);
    }
    if (!strcmp(ext+1, "jpg"))
        mime = "image/jpeg";
    else if (!strcmp(ext+1, "png"))
        mime = "image/png";
    else if (!strcmp(ext+1, "bmp"))
        mime = "image/bmp";
    else {
        av_log(NULL, AV_LOG_ERROR, "Unknown extension, supported: .jpg, .png, .bmp\n");
        ffmpeg_exit(1);
    }

    cover_file = fopen(arg, "r");
    if (!cover_file) {
        av_log(NULL, AV_LOG_ERROR, "Could not open cover file\n");
        ffmpeg_exit(1);
    }
    file_size = fseek(cover_file, 0, SEEK_END);
    if (file_size < 0) {
        av_log(NULL, AV_LOG_ERROR, "Could not compute cover file size\n");
        ffmpeg_exit(1);
    }
    file_size = ftell(cover_file);
    fseek(cover_file, 0, SEEK_SET);

    buf = av_malloc(file_size);
    if (!buf)
        ffmpeg_exit(1);
    if (fread(buf, file_size, 1, cover_file) != 1) {
        av_log(NULL, AV_LOG_ERROR, "Could not read cover file\n");
        ffmpeg_exit(1);
    }

    if (av_dict_set_custom(&metadata, &tag, METADATA_BYTEARRAY, "cover",
                           buf, file_size, AV_DICT_DONT_STRDUP_VAL) < 0)
        ffmpeg_exit(1);
    av_metadata_set_attribute(tag, "mime", mime);

    return 0;
}

static int opt_metadata(const char *opt, char *arg)
{
    const char *p = arg;
    int n = 0;

    while (p = strchr(p, '=')) {
        p++;
        n++;
    }

    for (p = arg; p;) {
        char *comma = strchr(p, ',');
        char *split = strchr(p, '=');
        if (!split) {
            av_log(NULL, AV_LOG_ERROR, "Error, metadata item does not contain '='\n");
            ffmpeg_exit(1);
        }
        *split = 0;
        if (comma && n > 1)
            *comma++ = 0;
        av_dict_set(&metadata, p, split+1, 0);
        p = comma;
    }

    return 0;
}

static int opt_qscale(const char *opt, const char *arg)
{
    video_qscale = parse_number_or_die(opt, arg, OPT_FLOAT, 0, 255);
    if (video_qscale <= 0 || video_qscale > 255) {
        av_log(NULL, AV_LOG_ERROR, "qscale must be > 0.0 and <= 255\n");
        return AVERROR(EINVAL);
    }
    return 0;
}

static int opt_interlaced(const char *opt, const char *arg)
{
    if (!strcmp(opt, "tff"))
        interlaced = 1;
    else if (!strcmp(opt, "bff"))
        interlaced = 2;
    else {
        av_log(NULL, AV_LOG_ERROR, "Error, unrecognized parameter '%s', specify either 'tff' or 'bff'\n", opt);
        return -1;
    }
    return 0;
}

static int opt_top_field_first(const char *opt, const char *arg)
{
    av_log(NULL, AV_LOG_ERROR, "Option is deprecated, use -tff or -bff\n");
    return -1;
}

static int opt_thread_count(const char *opt, const char *arg)
{
    thread_count= parse_number_or_die(opt, arg, OPT_INT64, 0, INT_MAX);
#if !HAVE_THREADS
    if (verbose >= 0)
        av_log(NULL, AV_LOG_WARNING, "Warning: not compiled with thread support, using thread emulation\n");
#endif
    return 0;
}

static int opt_audio_sample_fmt(const char *opt, const char *arg)
{
    if (strcmp(arg, "list")) {
        audio_sample_fmt = av_get_sample_fmt(arg);
        if (audio_sample_fmt == AV_SAMPLE_FMT_NONE) {
            av_log(NULL, AV_LOG_ERROR, "Invalid sample format '%s'\n", arg);
            return AVERROR(EINVAL);
        }
    } else {
        int i;
        char fmt_str[128];
        for (i = -1; i < AV_SAMPLE_FMT_NB; i++)
            printf("%s\n", av_get_sample_fmt_string(fmt_str, sizeof(fmt_str), i));
        ffmpeg_exit(0);
    }
    return 0;
}

static int opt_audio_rate(const char *opt, const char *arg)
{
    audio_sample_rate = parse_number_or_die(opt, arg, OPT_INT64, 0, INT_MAX);
    return 0;
}

static int opt_audio_channels(const char *opt, const char *arg)
{
    audio_channels = parse_number_or_die(opt, arg, OPT_INT64, 0, INT_MAX);
    return 0;
}

static int opt_video_channel(const char *opt, const char *arg)
{
    av_log(NULL, AV_LOG_WARNING, "This option is deprecated, use -channel.\n");
    opt_default("channel", arg);
    return 0;
}

static int opt_video_standard(const char *opt, const char *arg)
{
    av_log(NULL, AV_LOG_WARNING, "This option is deprecated, use -standard.\n");
    opt_default("standard", arg);
    return 0;
}

static int opt_codec(const char *opt, const char *arg)
{
    int *pstream_copy = NULL; char **pcodec_name = NULL;

    if      (!strcmp(opt, "acodec")) { pstream_copy = &audio_stream_copy;    pcodec_name = &audio_codec_name;    }
    else if (!strcmp(opt, "vcodec")) { pstream_copy = &video_stream_copy;    pcodec_name = &video_codec_name;    }
    else if (!strcmp(opt, "scodec")) { pstream_copy = &subtitle_stream_copy; pcodec_name = &subtitle_codec_name; }
    else if (!strcmp(opt, "dcodec")) { pstream_copy = &data_stream_copy;     pcodec_name = &data_codec_name;     }

    av_freep(pcodec_name);
    if (pstream_copy && !strcmp(arg, "copy")) {
        *pstream_copy = 1;
    } else {
        *pcodec_name = av_strdup(arg);
    }
    return 0;
}

static int opt_codec_tag(const char *opt, const char *arg)
{
    char *tail;
    uint32_t *codec_tag;

    codec_tag = !strcmp(opt, "atag") ? &audio_codec_tag :
                !strcmp(opt, "vtag") ? &video_codec_tag :
                !strcmp(opt, "stag") ? &subtitle_codec_tag : NULL;
    if (!codec_tag)
        return -1;

    *codec_tag = strtol(arg, &tail, 0);
    if (!tail || *tail)
        *codec_tag = AV_RL32(arg);

    return 0;
}

static int opt_map_audio_channel(const char *opt, const char *arg)
{
    AudioChannelMap *m;
    char *p;

    if (nb_audio_channel_maps == MAX_AUDIO_CHANNEL_MAPS) {
        av_log(NULL, AV_LOG_ERROR, "Only %d channels mappings are supported\n", MAX_AUDIO_CHANNEL_MAPS);
        exit(1);
    }

    m = &audio_channel_maps[nb_audio_channel_maps++];

    m->file_index = strtol(arg, &p, 0);
    if (*p) p++;
    else goto syntax_error;

    m->stream_index = strtol(p, &p, 0);
    if (*p) {
        p++;
        m->channel_index = strtol(p, &p, 0);
        if (*p) {
            p++;
            m->out_file_index = strtol(p, &p, 0);
            if (*p) p++;
            else goto syntax_error;
            m->out_stream_index = strtol(p, &p, 0);
            if (*p) {
                p++;
                m->out_channel_index = strtol(p, &p, 0);
            } else
                m->out_channel_index = -1;
        } else
            goto syntax_error;
    } else {
    syntax_error:
        av_log(NULL, AV_LOG_ERROR, "Syntax error, usage: file.stream:channel:outfile.stream[:channel]\n");
        exit(1);
    }
    return 0;
}

static int opt_map(const char *opt, const char *arg)
{
    StreamMap *m;
    char *p;

    stream_maps = grow_array(stream_maps, sizeof(*stream_maps), &nb_stream_maps, nb_stream_maps + 1);
    m = &stream_maps[nb_stream_maps-1];

    m->file_index = strtol(arg, &p, 0);
    if (*p)
        p++;

    m->stream_index = strtol(p, &p, 0);
    if (*p) {
        p++;
        m->sync_file_index = strtol(p, &p, 0);
        if (*p)
            p++;
        m->sync_stream_index = strtol(p, &p, 0);
    } else {
        m->sync_file_index = m->file_index;
        m->sync_stream_index = m->stream_index;
    }
    return 0;
}

static void parse_meta_type(char *arg, char *type, int *index, char **endptr)
{
    *endptr = arg;
    if (*arg == ',') {
        *type = *(++arg);
        switch (*arg) {
        case 'g':
            break;
        case 's':
        case 'c':
        case 'p':
            *index = strtol(++arg, endptr, 0);
            break;
        default:
            av_log(NULL, AV_LOG_ERROR, "Invalid metadata type %c.\n", *arg);
            ffmpeg_exit(1);
        }
    } else
        *type = 'g';
}

static int opt_map_metadata(const char *opt, const char *arg)
{
    MetadataMap *m, *m1;
    char *p;

    meta_data_maps = grow_array(meta_data_maps, sizeof(*meta_data_maps),
                                &nb_meta_data_maps, nb_meta_data_maps + 1);

    m = &meta_data_maps[nb_meta_data_maps - 1][0];
    m->file = strtol(arg, &p, 0);
    parse_meta_type(p, &m->type, &m->index, &p);
    if (*p)
        p++;

    m1 = &meta_data_maps[nb_meta_data_maps - 1][1];
    m1->file = strtol(p, &p, 0);
    parse_meta_type(p, &m1->type, &m1->index, &p);

    return 0;
}

static int opt_map_meta_data(const char *opt, const char *arg)
{
    av_log(NULL, AV_LOG_ERROR, "-map_meta_data is deprecated and will be removed soon. "
                    "Use -map_metadata instead.\n");
    return opt_map_metadata(opt, arg);
}

static int opt_map_chapters(const char *opt, const char *arg)
{
    ChapterMap *c;
    char *p;

    chapter_maps = grow_array(chapter_maps, sizeof(*chapter_maps), &nb_chapter_maps,
                              nb_chapter_maps + 1);
    c = &chapter_maps[nb_chapter_maps - 1];
    c->out_file = strtol(arg, &p, 0);
    if (*p)
        p++;

    c->in_file = strtol(p, &p, 0);
    return 0;
}

static int opt_input_ts_scale(const char *opt, const char *arg)
{
    unsigned int stream;
    double scale;
    char *p;

    stream = strtol(arg, &p, 0);
    if (*p)
        p++;
    scale= strtod(p, &p);

    if(stream >= MAX_STREAMS)
        ffmpeg_exit(1);

    ts_scale = grow_array(ts_scale, sizeof(*ts_scale), &nb_ts_scale, stream + 1);
    ts_scale[stream] = scale;
    return 0;
}

static int opt_recording_time(const char *opt, const char *arg)
{
    recording_time = parse_time_or_die(opt, arg, 1);
    return 0;
}

static int opt_start_time(const char *opt, const char *arg)
{
    start_time = parse_time_or_die(opt, arg, 1);
    return 0;
}

static int opt_recording_timestamp(const char *opt, const char *arg)
{
    char buf[128];
    int64_t recording_timestamp = parse_time_or_die(opt, arg, 0) / 1E6;
    struct tm time = *gmtime((time_t*)&recording_timestamp);
    strftime(buf, sizeof(buf), "creation_time=%FT%T%z", &time);
    opt_metadata("metadata", buf);

    av_log(NULL, AV_LOG_WARNING, "%s is deprecated, set the 'creation_time' metadata "
                                 "tag instead.\n", opt);
    return 0;
}

static int opt_input_ts_offset(const char *opt, const char *arg)
{
    input_ts_offset = parse_time_or_die(opt, arg, 1);
    return 0;
}

static enum CodecID find_codec_or_die(const char *name, int type, int encoder)
{
    const char *codec_string = encoder ? "encoder" : "decoder";
    AVCodec *codec;

    if(!name)
        return CODEC_ID_NONE;
    codec = encoder ?
        avcodec_find_encoder_by_name(name) :
        avcodec_find_decoder_by_name(name);
    if(!codec) {
        av_log(NULL, AV_LOG_ERROR, "Unknown %s '%s'\n", codec_string, name);
        ffmpeg_exit(1);
    }
    if(codec->type != type) {
        av_log(NULL, AV_LOG_ERROR, "Invalid %s type '%s'\n", codec_string, name);
        ffmpeg_exit(1);
    }
    return codec->id;
}

static int opt_input_file(const char *opt, const char *filename)
{
    AVFormatContext *ic;
    AVInputFormat *file_iformat = NULL;
    int err, i, ret;
    int64_t timestamp;
    uint8_t buf[128];
    AVDictionary **opts;
    int orig_nb_streams;                     // number of streams before avformat_find_stream_info

    if (last_asked_format) {
        if (!(file_iformat = av_find_input_format(last_asked_format))) {
            av_log(NULL, AV_LOG_ERROR, "Unknown input format: '%s'\n", last_asked_format);
            ffmpeg_exit(1);
        }
        last_asked_format = NULL;
    }

    if (!strcmp(filename, "-"))
        filename = "pipe:";

    using_stdin |= !strncmp(filename, "pipe:", 5) ||
                    !strcmp(filename, "/dev/stdin");

    /* get default parameters from command line */
    ic = avformat_alloc_context();
    if (!ic) {
        print_error(filename, AVERROR(ENOMEM));
        ffmpeg_exit(1);
    }
    if (audio_sample_rate) {
        snprintf(buf, sizeof(buf), "%d", audio_sample_rate);
        av_dict_set(&format_opts, "sample_rate", buf, 0);
    }
    if (audio_channels) {
        snprintf(buf, sizeof(buf), "%d", audio_channels);
        av_dict_set(&format_opts, "channels", buf, 0);
    }
    if (frame_rate.num) {
        snprintf(buf, sizeof(buf), "%d/%d", frame_rate.num, frame_rate.den);
        av_dict_set(&format_opts, "framerate", buf, 0);
    }
    if (frame_width && frame_height) {
        snprintf(buf, sizeof(buf), "%dx%d", frame_width, frame_height);
        av_dict_set(&format_opts, "video_size", buf, 0);
    }
    if (frame_pix_fmt != PIX_FMT_NONE)
        av_dict_set(&format_opts, "pixel_format", av_get_pix_fmt_name(frame_pix_fmt), 0);

    ic->video_codec_id   =
        find_codec_or_die(video_codec_name   , AVMEDIA_TYPE_VIDEO   , 0);
    ic->audio_codec_id   =
        find_codec_or_die(audio_codec_name   , AVMEDIA_TYPE_AUDIO   , 0);
    ic->subtitle_codec_id=
        find_codec_or_die(subtitle_codec_name, AVMEDIA_TYPE_SUBTITLE, 0);
    ic->flags |= AVFMT_FLAG_NONBLOCK;

    /* open the input file with generic libav function */
    err = avformat_open_input(&ic, filename, file_iformat, &format_opts);
    if (err < 0) {
        print_error(filename, err);
        ffmpeg_exit(1);
    }
    assert_avoptions(format_opts);

    if (loop_input) {
        av_log(NULL, AV_LOG_WARNING, "-loop_input is deprecated, use -loop 1\n");
        ic->loop_input = loop_input;
    }

    /* Set AVCodecContext options for avformat_find_stream_info */
    opts = setup_find_stream_info_opts(ic);
    orig_nb_streams = ic->nb_streams;

    /* If not enough info to get the stream parameters, we decode the
       first frames to get it. (used in mpeg case for example) */
    ret = avformat_find_stream_info(ic, opts);
    if (ret < 0 && verbose >= 0) {
        av_log(NULL, AV_LOG_ERROR, "%s: could not find codec parameters\n", filename);
        av_close_input_file(ic);
        ffmpeg_exit(1);
    }

    if (ic->nb_programs > 1 && !opt_programid) {
        av_log(NULL, AV_LOG_WARNING, "Multiple programs detected, "
               "auto-selecting program %d\n", ic->programs[0]->id);
        opt_programid = ic->programs[0]->id;
    }

    if (opt_programid) {
        int i, j;
        int found = 0;
        for (i = 0; i < ic->nb_streams; i++)
            ic->streams[i]->discard = AVDISCARD_ALL;
        for (i = 0; i < ic->nb_programs; i++) {
            AVProgram *p = ic->programs[i];
            if (p->id != opt_programid) {
                p->discard = AVDISCARD_ALL;
            } else {
                found = 1;
                for (j = 0; j < p->nb_stream_indexes; j++)
                    ic->streams[p->stream_index[j]]->discard = AVDISCARD_DEFAULT;
            }
        }
        if (!found) {
            av_log(NULL, AV_LOG_ERROR, "Specified program id not found\n");
            ffmpeg_exit(1);
        }
        opt_programid = 0;
    }

    if (!strcmp(ic->iformat->name, "gxf"))
        audio_sync_method = 0;

    timestamp = start_time;
    /* sync on video stream */
    i = av_find_default_stream_index(ic);
    if (i >= 0 && ic->streams[i]->start_time != AV_NOPTS_VALUE &&
        ic->streams[i]->start_time > 0) {
        int64_t start = av_rescale_q(ic->streams[i]->start_time,
                                     ic->streams[i]->time_base, AV_TIME_BASE_Q);
        timestamp += start;
    }

    /* if seeking requested, we execute it */
    if (start_time != 0) {
        ret = av_seek_frame(ic, -1, timestamp, AVSEEK_FLAG_BACKWARD);
        if (ret < 0) {
            av_log(NULL, AV_LOG_ERROR, "%s: could not seek to position %0.3f\n",
                    filename, (double)timestamp / AV_TIME_BASE);
        }
        /* reset seek info */
        start_time = 0;
    }

    /* update the current parameters so that they match the one of the input stream */
    for(i=0;i<ic->nb_streams;i++) {
        AVStream *st = ic->streams[i];
        AVCodecContext *dec = st->codec;
        InputStream *ist;

        dec->thread_count = thread_count;

        input_streams = grow_array(input_streams, sizeof(*input_streams), &nb_input_streams, nb_input_streams + 1);
        ist = &input_streams[nb_input_streams - 1];
        ist->st = st;
        ist->file_index = nb_input_files;
        ist->discard = 1;
        ist->opts = filter_codec_opts(codec_opts, ist->st->codec->codec_id, 0);

        if (i < nb_ts_scale)
            ist->ts_scale = ts_scale[i];

        switch (dec->codec_type) {
        case AVMEDIA_TYPE_AUDIO:
            ist->dec = avcodec_find_decoder_by_name(audio_codec_name);
            if(!ist->dec)
                ist->dec = avcodec_find_decoder(dec->codec_id);
            if(audio_disable)
                st->discard= AVDISCARD_ALL;
            break;
        case AVMEDIA_TYPE_VIDEO:
            ist->dec= avcodec_find_decoder_by_name(video_codec_name);
            if(!ist->dec)
                ist->dec = avcodec_find_decoder(dec->codec_id);
            if (dec->lowres) {
                dec->flags |= CODEC_FLAG_EMU_EDGE;
            }
            ist->frame_rate = frame_rate;
            if(video_disable)
                st->discard= AVDISCARD_ALL;
            else if(video_discard)
                st->discard= video_discard;
            break;
        case AVMEDIA_TYPE_DATA:
            break;
        case AVMEDIA_TYPE_SUBTITLE:
            ist->dec = avcodec_find_decoder_by_name(subtitle_codec_name);
            if(!ist->dec)
                ist->dec = avcodec_find_decoder(dec->codec_id);
            if(subtitle_disable)
                st->discard = AVDISCARD_ALL;
            break;
        default:
        case AVMEDIA_TYPE_UNKNOWN:
            break;
        }
    }

    /* dump the file content */
    if (verbose >= 0)
        av_dump_format(ic, nb_input_files, filename, 0);

    input_files = grow_array(input_files, sizeof(*input_files), &nb_input_files, nb_input_files + 1);
    input_files[nb_input_files - 1].ctx        = ic;
    input_files[nb_input_files - 1].ist_index  = nb_input_streams - ic->nb_streams;
    input_files[nb_input_files - 1].ts_offset  = input_ts_offset - (copy_ts ? 0 : timestamp);

    interlaced = 0;
    frame_rate    = (AVRational){0, 0};
    frame_pix_fmt = PIX_FMT_NONE;
    frame_height = 0;
    frame_width  = 0;
    audio_sample_rate = 0;
    audio_channels    = 0;
    audio_sample_fmt  = AV_SAMPLE_FMT_NONE;
    av_freep(&ts_scale);
    nb_ts_scale = 0;

    for (i = 0; i < orig_nb_streams; i++)
        av_dict_free(&opts[i]);
    av_freep(&opts);
    av_freep(&video_codec_name);
    av_freep(&audio_codec_name);
    av_freep(&subtitle_codec_name);
    uninit_opts();
    init_opts();
    return 0;
}

static void check_inputs(int *has_video_ptr,
                         int *has_audio_ptr,
                         int *has_subtitle_ptr,
                         int *has_data_ptr)
{
    int has_video, has_audio, has_subtitle, has_data, i, j;
    AVFormatContext *ic;

    has_video = 0;
    has_audio = 0;
    has_subtitle = 0;
    has_data = 0;

    for(j=0;j<nb_input_files;j++) {
        ic = input_files[j].ctx;
        for(i=0;i<ic->nb_streams;i++) {
            AVCodecContext *enc = ic->streams[i]->codec;
            switch(enc->codec_type) {
            case AVMEDIA_TYPE_AUDIO:
                has_audio = 1;
                break;
            case AVMEDIA_TYPE_VIDEO:
                has_video = 1;
                break;
            case AVMEDIA_TYPE_SUBTITLE:
                has_subtitle = 1;
                break;
            case AVMEDIA_TYPE_DATA:
            case AVMEDIA_TYPE_UNKNOWN:
                has_data = 1;
                break;
            default:
                abort();
            }
        }
    }
    *has_video_ptr = has_video;
    *has_audio_ptr = has_audio;
    *has_subtitle_ptr = has_subtitle;
    *has_data_ptr = has_data;
}

static void new_video_stream(AVFormatContext *oc, int file_idx)
{
    AVStream *st;
    OutputStream *ost;
    AVCodecContext *video_enc;
    enum CodecID codec_id = CODEC_ID_NONE;
    AVCodec *codec= NULL;

    if(!video_stream_copy){
        if (video_codec_name) {
            codec_id = find_codec_or_die(video_codec_name, AVMEDIA_TYPE_VIDEO, 1);
            codec = avcodec_find_encoder_by_name(video_codec_name);
        } else {
            codec_id = av_guess_codec(oc->oformat, NULL, oc->filename, NULL, AVMEDIA_TYPE_VIDEO);
            codec = avcodec_find_encoder(codec_id);
        }
    }

    ost = new_output_stream(oc, file_idx, codec);
    st  = ost->st;
    if (!video_stream_copy) {
        ost->frame_aspect_ratio = frame_aspect_ratio;
        frame_aspect_ratio = (AVRational){0,0};
#if CONFIG_AVFILTER
        ost->avfilter = vfilters;
        vfilters = NULL;
#endif
    }

    ost->target = target;

    ost->bitstream_filters = video_bitstream_filters;
    video_bitstream_filters= NULL;

    st->codec->thread_count= thread_count;

    video_enc = st->codec;

    if(video_codec_tag)
        video_enc->codec_tag= video_codec_tag;

    if(oc->oformat->flags & AVFMT_GLOBALHEADER) {
        video_enc->flags |= CODEC_FLAG_GLOBAL_HEADER;
    }

    video_enc->codec_type = AVMEDIA_TYPE_VIDEO;
    if (video_stream_copy) {
        st->stream_copy = 1;
        video_enc->sample_aspect_ratio =
        st->sample_aspect_ratio =
            av_mul_q(frame_aspect_ratio, (AVRational){ frame_height, frame_width });
    } else {
        const char *p;
        int i;

        ost->frame_rate = frame_rate;
        ost->force_fps = force_fps;
        video_enc->codec_id = codec_id;

        video_enc->width = frame_width;
        video_enc->height = frame_height;
        video_enc->pix_fmt = frame_pix_fmt;
        video_enc->bits_per_raw_sample = frame_bits_per_raw_sample;
        st->sample_aspect_ratio = video_enc->sample_aspect_ratio;

        if (intra_only)
            video_enc->gop_size = 0;
        if (video_qscale || same_quality) {
            video_enc->flags |= CODEC_FLAG_QSCALE;
            video_enc->global_quality = FF_QP2LAMBDA * video_qscale;
        }

        if (interlaced > 0) {
            video_enc->flags |= CODEC_FLAG_INTERLACED_DCT|CODEC_FLAG_INTERLACED_ME;
            video_enc->interlaced = interlaced;
        }

        if(intra_matrix)
            video_enc->intra_matrix = intra_matrix;
        if(inter_matrix)
            video_enc->inter_matrix = inter_matrix;

        p= video_rc_override_string;
        for(i=0; p; i++){
            int start, end, q;
            int e=sscanf(p, "%d,%d,%d", &start, &end, &q);
            if(e!=3){
                av_log(NULL, AV_LOG_ERROR, "error parsing rc_override\n");
                ffmpeg_exit(1);
            }
            video_enc->rc_override=
                av_realloc(video_enc->rc_override,
                           sizeof(RcOverride)*(i+1));
            video_enc->rc_override[i].start_frame= start;
            video_enc->rc_override[i].end_frame  = end;
            if(q>0){
                video_enc->rc_override[i].qscale= q;
                video_enc->rc_override[i].quality_factor= 1.0;
            }
            else{
                video_enc->rc_override[i].qscale= 0;
                video_enc->rc_override[i].quality_factor= -q/100.0;
            }
            p= strchr(p, '/');
            if(p) p++;
        }
        video_enc->rc_override_count=i;
        if (!video_enc->rc_initial_buffer_occupancy)
            video_enc->rc_initial_buffer_occupancy = video_enc->rc_buffer_size*3/4;
        video_enc->intra_dc_precision= intra_dc_precision - 8;

        if (do_psnr)
            video_enc->flags|= CODEC_FLAG_PSNR;

        /* two pass mode */
        if (do_pass) {
            if (do_pass == 1) {
                video_enc->flags |= CODEC_FLAG_PASS1;
            } else {
                video_enc->flags |= CODEC_FLAG_PASS2;
            }
        }

        if (forced_key_frames)
            parse_forced_key_frames(forced_key_frames, ost, video_enc);
    }
    if (video_language) {
        av_dict_set(&st->metadata, "language", video_language, 0);
        av_freep(&video_language);
    }

    /* reset some key parameters */
    interlaced = 0;
    video_disable = 0;
    video_codec_tag = 0;
    av_freep(&forced_key_frames);
    frame_pix_fmt = PIX_FMT_NONE;
}

static void new_audio_stream(AVFormatContext *oc, int file_idx)
{
    AVStream *st;
    OutputStream *ost;
    AVCodec *codec= NULL;
    AVCodecContext *audio_enc;
    enum CodecID codec_id = CODEC_ID_NONE;

    if(!audio_stream_copy){
        if (target && !strncmp(target, "dvc", 3) &&
            !strcmp(oc->oformat->name, "dv")) { // hack because of target setting LE
            codec_id = CODEC_ID_PCM_S16BE;
        } else if (audio_codec_name) {
            codec_id = find_codec_or_die(audio_codec_name, AVMEDIA_TYPE_AUDIO, 1);
            codec = avcodec_find_encoder_by_name(audio_codec_name);
        } else {
            codec_id = av_guess_codec(oc->oformat, NULL, oc->filename, NULL, AVMEDIA_TYPE_AUDIO);
            codec = avcodec_find_encoder(codec_id);
        }
    }
    ost = new_output_stream(oc, file_idx, codec);
    st  = ost->st;

    ost->target = target;

    ost->bitstream_filters = audio_bitstream_filters;
    audio_bitstream_filters= NULL;

    st->codec->thread_count= thread_count;

    audio_enc = st->codec;
    audio_enc->codec_type = AVMEDIA_TYPE_AUDIO;

    if(audio_codec_tag)
        audio_enc->codec_tag= audio_codec_tag;

    if (oc->oformat->flags & AVFMT_GLOBALHEADER) {
        audio_enc->flags |= CODEC_FLAG_GLOBAL_HEADER;
    }
    if (audio_stream_copy) {
        st->stream_copy = 1;
    } else {
        audio_enc->codec_id = codec_id;

        if (audio_qscale > QSCALE_NONE) {
            audio_enc->flags |= CODEC_FLAG_QSCALE;
            audio_enc->global_quality = FF_QP2LAMBDA * audio_qscale;
        }
        if (audio_channels)
            audio_enc->channels = audio_channels;
        if (audio_sample_fmt != AV_SAMPLE_FMT_NONE)
            audio_enc->sample_fmt = audio_sample_fmt;
        if (audio_sample_rate)
            audio_enc->sample_rate = audio_sample_rate;
    }
    if (audio_language) {
        av_dict_set(&st->metadata, "language", audio_language, 0);
        av_freep(&audio_language);
    }

    /* reset some key parameters */
    audio_disable = 0;
    audio_codec_tag = 0;
}

static void new_data_stream(AVFormatContext *oc, int file_idx)
{
    AVStream *st;
    OutputStream *ost;
    AVCodecContext *data_enc;

    ost = new_output_stream(oc, file_idx, NULL);
    st  = ost->st;
    data_enc = st->codec;
    if (!data_stream_copy) {
        av_log(NULL, AV_LOG_ERROR, "Data stream encoding not supported yet (only streamcopy)\n");
        ffmpeg_exit(1);
    }

    data_enc->codec_type = AVMEDIA_TYPE_DATA;

    if (data_codec_tag)
        data_enc->codec_tag= data_codec_tag;

    if (oc->oformat->flags & AVFMT_GLOBALHEADER) {
        data_enc->flags |= CODEC_FLAG_GLOBAL_HEADER;
    }
    if (data_stream_copy) {
        st->stream_copy = 1;
    }

    data_disable = 0;
    data_codec_tag = 0;
}

static void new_subtitle_stream(AVFormatContext *oc, int file_idx)
{
    AVStream *st;
    OutputStream *ost;
    AVCodec *codec=NULL;
    AVCodecContext *subtitle_enc;
    enum CodecID codec_id = CODEC_ID_NONE;

    if(!subtitle_stream_copy){
        if (subtitle_codec_name) {
            codec_id = find_codec_or_die(subtitle_codec_name, AVMEDIA_TYPE_SUBTITLE, 1);
            codec = avcodec_find_encoder_by_name(subtitle_codec_name);
        } else {
            codec_id = av_guess_codec(oc->oformat, NULL, oc->filename, NULL, AVMEDIA_TYPE_SUBTITLE);
            codec = avcodec_find_encoder(codec_id);
        }
    }
    ost = new_output_stream(oc, file_idx, codec);
    st  = ost->st;
    subtitle_enc = st->codec;

    ost->bitstream_filters = subtitle_bitstream_filters;
    subtitle_bitstream_filters= NULL;

    subtitle_enc->codec_type = AVMEDIA_TYPE_SUBTITLE;

    if(subtitle_codec_tag)
        subtitle_enc->codec_tag= subtitle_codec_tag;

    if (oc->oformat->flags & AVFMT_GLOBALHEADER) {
        subtitle_enc->flags |= CODEC_FLAG_GLOBAL_HEADER;
    }
    if (subtitle_stream_copy) {
        st->stream_copy = 1;
    } else {
        subtitle_enc->codec_id = codec_id;
    }

    if (subtitle_language) {
        av_dict_set(&st->metadata, "language", subtitle_language, 0);
        av_freep(&subtitle_language);
    }

    subtitle_disable = 0;
    subtitle_codec_tag = 0;
}

static int opt_new_stream(const char *opt, const char *arg)
{
    AVFormatContext *oc;
    int file_idx = nb_output_files - 1;
    if (nb_output_files <= 0) {
        av_log(NULL, AV_LOG_ERROR, "At least one output file must be specified\n");
        ffmpeg_exit(1);
    }
    oc = output_files[file_idx];

    if      (!strcmp(opt, "newvideo"   )) new_video_stream   (oc, file_idx);
    else if (!strcmp(opt, "newaudio"   )) new_audio_stream   (oc, file_idx);
    else if (!strcmp(opt, "newsubtitle")) new_subtitle_stream(oc, file_idx);
    else if (!strcmp(opt, "newdata"    )) new_data_stream    (oc, file_idx);
    else av_assert0(0);
    return 0;
}

/* arg format is "output-stream-index:streamid-value". */
static int opt_streamid(const char *opt, const char *arg)
{
    int idx;
    char *p;
    char idx_str[16];

    av_strlcpy(idx_str, arg, sizeof(idx_str));
    p = strchr(idx_str, ':');
    if (!p) {
        av_log(NULL, AV_LOG_ERROR,
                "Invalid value '%s' for option '%s', required syntax is 'index:value'\n",
                arg, opt);
        ffmpeg_exit(1);
    }
    *p++ = '\0';
    idx = parse_number_or_die(opt, idx_str, OPT_INT, 0, MAX_STREAMS-1);
    streamid_map = grow_array(streamid_map, sizeof(*streamid_map), &nb_streamid_map, idx+1);
    streamid_map[idx] = parse_number_or_die(opt, p, OPT_INT, 0, INT_MAX);
    return 0;
}

static int opt_bsf(const char *opt, const char *arg)
{
    AVBitStreamFilterContext *bsfc;
    AVBitStreamFilterContext **bsfp;
    char *name = av_strdup(arg);
    char *args = strchr(name, '=');

    if (args) {
        *args = 0;
        args++;
    }

    bsfc = av_bitstream_filter_init(name, args);

    av_free(name);

    if(!bsfc){
        av_log(NULL, AV_LOG_ERROR, "Error opening bitstream filter %s\n", name);
        ffmpeg_exit(1);
    }

    bsfp= *opt == 'v' ? &video_bitstream_filters :
          *opt == 'a' ? &audio_bitstream_filters :
                        &subtitle_bitstream_filters;
    while(*bsfp)
        bsfp= &(*bsfp)->next;

    *bsfp= bsfc;

    return 0;
}

static int opt_output_file(const char *opt, const char *filename)
{
    AVFormatContext *oc;
    int err, use_video, use_audio, use_subtitle, use_data;
    int input_has_video, input_has_audio, input_has_subtitle, input_has_data;
    AVOutputFormat *file_oformat;

    if(nb_output_files >= FF_ARRAY_ELEMS(output_files)){
        av_log(NULL, AV_LOG_ERROR, "Too many output files\n");
        ffmpeg_exit(1);
    }

    if (!strcmp(filename, "-"))
        filename = "pipe:";

    if (target) {
        if (!strncmp(target, "imx", 3)) {
            if (av_match_ext(filename, "mxf"))
                last_asked_format = "mxf_d10";
        } else if (!strcmp(target, "dvd")) {
            if (av_match_ext(filename, "mpg"))
                last_asked_format = "dvd";
        }
    }

    err = avformat_alloc_output_context2(&oc, NULL, last_asked_format, filename);
    last_asked_format = NULL;
    if (!oc) {
        print_error(filename, err);
        ffmpeg_exit(1);
    }
    file_oformat= oc->oformat;

    {
        use_video = file_oformat->video_codec != CODEC_ID_NONE || video_stream_copy || video_codec_name;
        use_audio = file_oformat->audio_codec != CODEC_ID_NONE || audio_stream_copy || audio_codec_name;
        use_subtitle = file_oformat->subtitle_codec != CODEC_ID_NONE || subtitle_stream_copy || subtitle_codec_name;
        use_data = data_stream_copy ||  data_codec_name; /* XXX once generic data codec will be available add a ->data_codec reference and use it here */

        /* disable if no corresponding type found */
        check_inputs(&input_has_video,
                     &input_has_audio,
                     &input_has_subtitle,
                     &input_has_data);

        if (!input_has_video)
            use_video = 0;
        if (!input_has_audio)
            use_audio = 0;
        if (!input_has_subtitle)
            use_subtitle = 0;
        if (!input_has_data)
            use_data = 0;

        /* manual disable */
        if (audio_disable)    use_audio    = 0;
        if (video_disable)    use_video    = 0;
        if (subtitle_disable) use_subtitle = 0;
        if (data_disable)     use_data     = 0;

        if (use_video)    new_video_stream(oc, nb_output_files);
        if (use_audio)    new_audio_stream(oc, nb_output_files);
        if (use_subtitle) new_subtitle_stream(oc, nb_output_files);
        if (use_data)     new_data_stream(oc, nb_output_files);

        av_dict_copy(&oc->metadata, metadata, 0);
        av_dict_free(&metadata);
    }

    av_dict_copy(&output_opts[nb_output_files], format_opts, 0);
    output_files[nb_output_files++] = oc;

    /* check filename in case of an image number is expected */
    if (oc->oformat->flags & AVFMT_NEEDNUMBER) {
        if (!av_filename_number_test(oc->filename)) {
            print_error(oc->filename, AVERROR(EINVAL));
            ffmpeg_exit(1);
        }
    }

    if (!(oc->oformat->flags & AVFMT_NOFILE)) {
        /* test if it already exists to avoid loosing precious files */
        if (!file_overwrite &&
            (strchr(filename, ':') == NULL ||
             filename[1] == ':' ||
             av_strstart(filename, "file:", NULL))) {
            if (avio_check(filename, 0) == 0) {
                if (!using_stdin) {
                    av_log(NULL, AV_LOG_INFO, "File '%s' already exists. Overwrite ? [y/N] ", filename);
                    fflush(stderr);
                    if (!read_yesno()) {
                        av_log(NULL, AV_LOG_INFO, "Not overwriting - exiting\n");
                        ffmpeg_exit(1);
                    }
                    av_log(NULL, AV_LOG_INFO, "\r");
                }
                else {
                    av_log(NULL, AV_LOG_INFO, "File '%s' already exists. Exiting.\n", filename);
                    ffmpeg_exit(1);
                }
            }
        }

        /* open the file */
        if ((err = avio_open(&oc->pb, filename, AVIO_FLAG_WRITE)) < 0) {
            print_error(filename, err);
            ffmpeg_exit(1);
        }
    }

    oc->preload= (int)(mux_preload*AV_TIME_BASE);
    oc->max_delay= (int)(mux_max_delay*AV_TIME_BASE);

    if (loop_output >= 0) {
        av_log(NULL, AV_LOG_WARNING, "-loop_output is deprecated, use -loop\n");
        oc->loop_output = loop_output;
    }

    target = NULL;
    frame_rate    = (AVRational){0, 0};
    frame_width   = 0;
    frame_height  = 0;
    audio_sample_rate = 0;
    audio_channels    = 0;
    audio_sample_fmt  = AV_SAMPLE_FMT_NONE;

    av_freep(&forced_key_frames);
    av_freep(&video_codec_name);
    av_freep(&audio_codec_name);
    av_freep(&subtitle_codec_name);
    video_stream_copy = 0;
    audio_stream_copy = 0;
    subtitle_stream_copy = 0;
    data_stream_copy = 0;
    uninit_opts();
    init_opts();
    return 0;
}

/* same option as mencoder */
static int opt_pass(const char *opt, const char *arg)
{
    do_pass = parse_number_or_die(opt, arg, OPT_INT, 1, 2);
    return 0;
}

static int64_t getutime(void)
{
#if HAVE_GETRUSAGE
    struct rusage rusage;

    getrusage(RUSAGE_SELF, &rusage);
    return (rusage.ru_utime.tv_sec * 1000000LL) + rusage.ru_utime.tv_usec;
#elif HAVE_GETPROCESSTIMES
    HANDLE proc;
    FILETIME c, e, k, u;
    proc = GetCurrentProcess();
    GetProcessTimes(proc, &c, &e, &k, &u);
    return ((int64_t) u.dwHighDateTime << 32 | u.dwLowDateTime) / 10;
#else
    return av_gettime();
#endif
}

static int64_t getmaxrss(void)
{
#if HAVE_GETRUSAGE && HAVE_STRUCT_RUSAGE_RU_MAXRSS
    struct rusage rusage;
    getrusage(RUSAGE_SELF, &rusage);
    return (int64_t)rusage.ru_maxrss * 1024;
#elif HAVE_GETPROCESSMEMORYINFO
    HANDLE proc;
    PROCESS_MEMORY_COUNTERS memcounters;
    proc = GetCurrentProcess();
    memcounters.cb = sizeof(memcounters);
    GetProcessMemoryInfo(proc, &memcounters, sizeof(memcounters));
    return memcounters.PeakPagefileUsage;
#else
    return 0;
#endif
}

static void parse_matrix_coeffs(uint16_t *dest, const char *str)
{
    int i;
    const char *p = str;
    for(i = 0;; i++) {
        dest[i] = atoi(p);
        if(i == 63)
            break;
        p = strchr(p, ',');
        if(!p) {
            av_log(NULL, AV_LOG_ERROR, "Syntax error in matrix \"%s\" at coeff %d\n", str, i);
            ffmpeg_exit(1);
        }
        p++;
    }
}

static int opt_inter_matrix(const char *opt, const char *arg)
{
    inter_matrix = av_mallocz(sizeof(uint16_t) * 64);
    parse_matrix_coeffs(inter_matrix, arg);
    return 0;
}

static int opt_intra_matrix(const char *opt, const char *arg)
{
    intra_matrix = av_mallocz(sizeof(uint16_t) * 64);
    parse_matrix_coeffs(intra_matrix, arg);
    return 0;
}

static void show_usage(void)
{
    printf("\n");
    printf("usage: ffmbc [options] [[infile options] -i infile]... {[outfile options] outfile}...\n");
    printf("\n");
}

static int opt_help(const char *opt, const char *arg)
{
    AVCodec *c;
    AVOutputFormat *oformat = NULL;
    AVInputFormat  *iformat = NULL;

    av_log_set_callback(log_callback_help);
    show_usage();
    show_help_options(options, "Main options:\n",
                      OPT_EXPERT | OPT_AUDIO | OPT_VIDEO | OPT_SUBTITLE | OPT_GRAB, 0);
    show_help_options(options, "\nAdvanced options:\n",
                      OPT_EXPERT | OPT_AUDIO | OPT_VIDEO | OPT_SUBTITLE | OPT_GRAB,
                      OPT_EXPERT);
    show_help_options(options, "\nVideo options:\n",
                      OPT_EXPERT | OPT_AUDIO | OPT_VIDEO | OPT_GRAB,
                      OPT_VIDEO);
    show_help_options(options, "\nAdvanced Video options:\n",
                      OPT_EXPERT | OPT_AUDIO | OPT_VIDEO | OPT_GRAB,
                      OPT_VIDEO | OPT_EXPERT);
    show_help_options(options, "\nAudio options:\n",
                      OPT_EXPERT | OPT_AUDIO | OPT_VIDEO | OPT_GRAB,
                      OPT_AUDIO);
    show_help_options(options, "\nAdvanced Audio options:\n",
                      OPT_EXPERT | OPT_AUDIO | OPT_VIDEO | OPT_GRAB,
                      OPT_AUDIO | OPT_EXPERT);
    show_help_options(options, "\nSubtitle options:\n",
                      OPT_SUBTITLE | OPT_GRAB,
                      OPT_SUBTITLE);
    show_help_options(options, "\nAudio/Video grab options:\n",
                      OPT_GRAB,
                      OPT_GRAB);
    printf("\n");
    show_options("AVCodecContext", NULL, avcodec_opts[0], AV_OPT_FLAG_ENCODING_PARAM|AV_OPT_FLAG_DECODING_PARAM);
    printf("\n");

    show_options("AVFormatContext", NULL, avformat_opts, AV_OPT_FLAG_ENCODING_PARAM|AV_OPT_FLAG_DECODING_PARAM);
    printf("\n");

    show_options("SwsContext", NULL, sws_opts, AV_OPT_FLAG_ENCODING_PARAM);
    printf("\n");

    /* individual demuxer options */
    while ((iformat = av_iformat_next(iformat))) {
        if (iformat->priv_class) {
            show_options(iformat->name, "demuxer", &iformat->priv_class, AV_OPT_FLAG_DECODING_PARAM);
            printf("\n");
        }
    }

    /* individual codec options */
    c = NULL;
    while ((c = av_codec_next(c))) {
        if (c->priv_class) {
            show_options(c->name, c->encode ? "encoder" : "decoder", &c->priv_class,
                         AV_OPT_FLAG_DECODING_PARAM|AV_OPT_FLAG_ENCODING_PARAM);
            printf("\n");
        }
    }

    /* individual muxer options */
    while ((oformat = av_oformat_next(oformat))) {
        if (oformat->priv_class) {
            show_options(oformat->name, "muxer", &oformat->priv_class, AV_OPT_FLAG_ENCODING_PARAM);
            printf("\n");
        }
    }

    return 0;
}

static int opt_vf(const char *opt, const char *arg)
{
    if (vfilters) {
        vfilters = av_realloc(vfilters, strlen(vfilters)+strlen(arg)+2);
        strcat(vfilters, ",");
        strcat(vfilters, arg);
    } else {
        vfilters = strdup(arg);
    }
    return 0;
}

static int opt_target(const char *opt, const char *arg)
{
    enum { NTSC_FILM, FILM, PAL, NTSC, HD50P, HD60P, UNKNOWN } norm = UNKNOWN;
    int fr = (int)(frame_rate.num * 1000.0 / frame_rate.den);
    /* Calculate FR via float to avoid int overflow */
    if (!frame_rate.num) {
        int i;
        if (!nb_input_streams) {
            av_log(NULL, AV_LOG_ERROR, "Please put input file before -target\n");
            ffmpeg_exit(1);
        }
        for (i = 0; i < nb_input_streams; i++) {
            AVStream *st = input_streams[i].st;
            if (st->codec->codec_type != AVMEDIA_TYPE_VIDEO)
                continue;
            fr = (int)(st->r_frame_rate.num * 1000.0 / st->r_frame_rate.den);
            break;
        }
    }

    if (fr == 23976) {
        norm = NTSC_FILM;
    } else if (fr == 24000) {
        norm = FILM;
    } else if (fr == 25000) {
        norm = PAL;
    } else if (fr == 29970) {
        norm = NTSC;
    } else if (fr == 50000) {
        norm = HD50P;
    } else if (fr == 59940) {
        norm = HD60P;
    } else {
        av_log(NULL, AV_LOG_ERROR, "Could not determine frame rate "
                "(film/pal/ntsc/ntsc-film/50p/60p) for target\n");
        av_log(NULL, AV_LOG_ERROR, "Specify one with \"-r xxx\" before -target\n");
        ffmpeg_exit(1);
    }

    if(!strcmp(arg, "vcd")) {
        opt_codec("vcodec", "mpeg1video");
        opt_codec("acodec", "mp2");
        opt_format("f", "vcd");

        frame_rate = frame_rate_tab[norm];
        opt_default("g", norm == PAL ? "15" : "18");

        opt_default("b", "1150000");
        opt_default("maxrate", "1150000");
        opt_default("minrate", "1150000");
        opt_default("bufsize", "327680"); // 40*1024*8;

        opt_default("ab", "224000");
        audio_sample_rate = 44100;
        audio_channels = 2;

        opt_default("packetsize", "2324");
        opt_default("muxrate", "1411200"); // 2352 * 75 * 8;

        /* We have to offset the PTS, so that it is consistent with the SCR.
           SCR starts at 36000, but the first two packs contain only padding
           and the first pack from the other stream, respectively, may also have
           been written before.
           So the real data starts at SCR 36000+3*1200. */
        mux_preload= (36000+3*1200) / 90000.0; //0.44
    } else if(!strcmp(arg, "svcd")) {

        opt_codec("vcodec", "mpeg2video");
        opt_codec("acodec", "mp2");
        opt_format("f", "svcd");

        frame_rate = frame_rate_tab[norm];
        opt_frame_pix_fmt("pix_fmt", "yuv420p");
        opt_default("g", norm == PAL ? "15" : "18");

        opt_default("b", "2040000");
        opt_default("maxrate", "2516000");
        opt_default("minrate", "0"); //1145000;
        opt_default("bufsize", "1835008"); //224*1024*8;
        opt_default("flags", "+scan_offset");


        opt_default("ab", "224000");
        audio_sample_rate = 44100;

        opt_default("packetsize", "2324");

    } else if(!strcmp(arg, "dvd")) {
        opt_codec("vcodec", "mpeg2video");
        opt_codec("acodec", "ac3");

        frame_rate = frame_rate_tab[norm];
        opt_frame_pix_fmt("pix_fmt", "yuv420p");
        opt_default("g", norm == PAL ? "15" : "18");
        opt_default("bf", "2");

        opt_default("b", "7000000");
        opt_default("maxrate", "9000000");
        opt_default("minrate", "0"); //1500000;
        opt_default("bufsize", "1835008"); //224*1024*8;

        opt_default("packetsize", "2048");  // from www.mpucoder.com: DVD sectors contain 2048 bytes of data, this is also the size of one pack.
        opt_default("muxrate", "10080000"); // from mplex project: data_rate = 1260000. mux_rate = data_rate * 8

        opt_default("ab", "448000");
        audio_sample_rate = 48000;

    } else if(!strcmp(arg, "dvcpro") || !strcmp(arg, "dvcpro50") ||
              !strcmp(arg, "dvcam") || !strcmp(arg, "dvcprohd")) {
        if (!strcmp(arg, "dvcprohd")) {
            if (norm == FILM || norm == NTSC_FILM) {
                av_log(NULL, AV_LOG_ERROR, "Error, dvcprohd target only supports ntsc, pal, "
                        "50p or 60p frame rate\n");
                ffmpeg_exit(1);
            }
        } else if (norm != PAL && norm != NTSC) {
            av_log(NULL, AV_LOG_ERROR, "Error, dv targets only support ntsc or pal frame rate\n");
            ffmpeg_exit(1);
        }

        opt_codec("vcodec", "dvvideo");
        opt_codec("acodec", "pcm_s16le");

        if (norm == PAL && !strcmp(arg, "dvcam"))
            opt_frame_pix_fmt("pix_fmt", "yuv420p");
        else if (!strcmp(arg, "dvcpro") || !strcmp(arg, "dvcam"))
            opt_frame_pix_fmt("pix_fmt", "yuv411p");
        else
            opt_frame_pix_fmt("pix_fmt", "yuv422p");

        audio_sample_rate = 48000;
    } else if(!strcmp(arg, "imx30") || !strcmp(arg, "imx50")) {
        if (norm != PAL && norm != NTSC) {
            av_log(NULL, AV_LOG_ERROR, "Error, imx targets only support ntsc or pal frame rate\n");
            ffmpeg_exit(1);
        }

        opt_codec("vcodec", "mpeg2video");
        opt_codec("acodec", "pcm_s16le");
        opt_default("flags2", "+ivlc+non_linear_q");
        opt_default("flags", "+ildct+low_delay");
        opt_default("ps", "1");
        opt_default("qmin", "1");
        opt_default("rc_max_vbv_use", "1");
        opt_default("rc_min_vbv_use", "1");
        opt_default("g", "1");
        opt_qscale("qscale", "1");
        opt_frame_pix_fmt("pix_fmt", "yuv422p");

        interlaced = 1;
        intra_dc_precision = 10;

        if (!strncmp(arg+3, "50", 2)) {
            opt_default("b", "50000000");
            opt_default("maxrate", "50000000");
            opt_default("minrate", "50000000");
            if (norm == PAL) {
                opt_default("bufsize", "2000000");
                opt_default("rc_init_occupancy", "2000000");
                opt_codec_tag("vtag", "mx5p");
            } else {
                opt_default("bufsize", "1668334");
                opt_default("rc_init_occupancy", "1668334");
                opt_codec_tag("vtag", "mx5n");
            }
        } else if (!strncmp(arg+3, "30", 2)) {
            opt_default("b", "30000000");
            opt_default("maxrate", "30000000");
            opt_default("minrate", "30000000");
            if (norm == PAL) {
                opt_default("bufsize", "1200000");
                opt_default("rc_init_occupancy", "1200000");
                opt_codec_tag("vtag", "mx3p");
            } else {
                opt_default("bufsize", "1001000");
                opt_default("rc_init_occupancy", "1001000");
                opt_codec_tag("vtag", "mx3n");
            }
        }

        opt_default("color_primaries", norm == PAL ? "bt470bg" : "smpte170m");
        opt_default("color_transfer", "bt709");
        opt_default("color_matrix", "smpte170m");

        audio_sample_rate = 48000;
    } else if(!strcmp(arg, "xdcamhd422")) {
        opt_codec("vcodec", "mpeg2video");
        opt_codec("acodec", "pcm_s16le");
        opt_default("flags2", "+ivlc+non_linear_q");
        opt_default("bf", "2");
        opt_default("g", norm == NTSC ? "15" : "12");
        opt_frame_pix_fmt("pix_fmt", "yuv422p");

        opt_qscale("qscale", "1");
        opt_default("qmin", "1");
        opt_default("b", "50000000");
        opt_default("maxrate", "50000000");
        opt_default("minrate", "50000000");
        opt_default("bufsize", "17825792");
        opt_default("rc_init_occupancy", "17825792");
        opt_default("sc_threshold", "1000000000");

        intra_dc_precision = 10;

        opt_default("color_primaries", "bt709");
        opt_default("color_transfer", "bt709");
        opt_default("color_matrix", "bt709");

        audio_sample_rate = 48000;
    } else if(!strcmp(arg, "avcintra100") || !strcmp(arg, "avcintra200")) {
        opt_codec("vcodec", "libx264");
        opt_codec("acodec", "pcm_s16le");
        opt_frame_pix_fmt("pix_fmt", "yuv422p10");

        opt_default("color_primaries", "bt709");
        opt_default("color_transfer", "bt709");
        opt_default("color_matrix", "bt709");

        opt_default("avcintra_class", arg+8);

        audio_sample_rate = 48000;
    } else {
        av_log(NULL, AV_LOG_ERROR, "Unknown target: %s\n", arg);
        return AVERROR(EINVAL);
    }

    target = arg;

    return 0;
}

static int opt_vstats_file(const char *opt, const char *arg)
{
    av_free (vstats_filename);
    vstats_filename=av_strdup (arg);
    return 0;
}

static int opt_vstats(const char *opt, const char *arg)
{
    char filename[40];
    time_t today2 = time(NULL);
    struct tm *today = localtime(&today2);

    snprintf(filename, sizeof(filename), "vstats_%02d%02d%02d.log", today->tm_hour, today->tm_min,
             today->tm_sec);
    return opt_vstats_file(opt, filename);
}

static int opt_preset(const char *opt, const char *arg)
{
    FILE *f=NULL;
    char filename[1000], tmp[1000], tmp2[1000], line[1000];
    char *codec_name = *opt == 'v' ? video_codec_name :
                       *opt == 'a' ? audio_codec_name :
                                     subtitle_codec_name;

    if (!(f = get_preset_file(filename, sizeof(filename), arg, *opt == 'f', codec_name))) {
        av_log(NULL, AV_LOG_ERROR, "File for preset '%s' not found\n", arg);
        ffmpeg_exit(1);
    }

    while(!feof(f)){
        int e= fscanf(f, "%999[^\n]\n", line) - 1;
        if(line[0] == '#' && !e)
            continue;
        e|= sscanf(line, "%999[^=]=%999[^\n]\n", tmp, tmp2) - 2;
        if(e){
            av_log(NULL, AV_LOG_ERROR, "%s: Invalid syntax: '%s'\n", filename, line);
            ffmpeg_exit(1);
        }
        if (!strcmp(tmp, "acodec") ||
            !strcmp(tmp, "vcodec") ||
            !strcmp(tmp, "scodec") ||
            !strcmp(tmp, "dcodec")) {
            opt_codec(tmp, tmp2);
        }else if(opt_default(tmp, tmp2) < 0){
            av_log(NULL, AV_LOG_ERROR, "%s: Invalid option or argument: '%s', parsed as '%s' = '%s'\n", filename, line, tmp, tmp2);
            ffmpeg_exit(1);
        }
    }

    fclose(f);

    return 0;
}

static void log_callback_null(void* ptr, int level, const char* fmt, va_list vl)
{
}

static int opt_passlogfile(const char *opt, const char *arg)
{
    pass_logfilename_prefix = arg;
    opt_default("passlogfile", arg);
    return 0;
}

static int opt_deinterlace(const char *opt, const char *arg)
{
    av_log(NULL, AV_LOG_ERROR, "Please use vf=yadif\n");
    return -1;
}

static const OptionDef options[] = {
    /* main options */
#include "cmdutils_common_opts.h"
    { "f", HAS_ARG, {(void*)opt_format}, "force format", "fmt" },
    { "i", HAS_ARG, {(void*)opt_input_file}, "input file name", "filename" },
    { "y", OPT_BOOL, {(void*)&file_overwrite}, "overwrite output files" },
    { "map", HAS_ARG | OPT_EXPERT, {(void*)opt_map}, "set input stream mapping", "file.stream[:syncfile.syncstream]" },
    { "map_meta_data", HAS_ARG | OPT_EXPERT, {(void*)opt_map_meta_data}, "DEPRECATED set meta data information of outfile from infile",
      "outfile[,metadata]:infile[,metadata]" },
    { "map_metadata", HAS_ARG | OPT_EXPERT, {(void*)opt_map_metadata}, "set metadata information of outfile from infile",
      "outfile[,metadata]:infile[,metadata]" },
    { "map_chapters",  HAS_ARG | OPT_EXPERT, {(void*)opt_map_chapters},  "set chapters mapping", "outfile:infile" },
    { "t", HAS_ARG, {(void*)opt_recording_time}, "record or transcode \"duration\" seconds of audio/video", "duration" },
    { "map_audio_channel", HAS_ARG | OPT_EXPERT, {(void*)opt_map_audio_channel}, "set audio channel extraction on stream", "file.stream:channel:outfile.stream[:channel]" },
    { "copy_chapters_metadata", OPT_BOOL, {(void*)&metadata_chapters_autocopy}, "copy chapters metadata"},
    { "copy_streams_metadata", OPT_BOOL, {(void*)&metadata_streams_autocopy}, "copy streams metadata"},
    { "fs", HAS_ARG | OPT_INT64, {(void*)&limit_filesize}, "set the limit file size in bytes", "limit_size" }, //
    { "ss", HAS_ARG, {(void*)opt_start_time}, "set the start time offset", "time_off" },
    { "itsoffset", HAS_ARG, {(void*)opt_input_ts_offset}, "set the input ts offset", "time_off" },
    { "itsscale", HAS_ARG, {(void*)opt_input_ts_scale}, "set the input ts scale", "stream:scale" },
    { "timestamp", HAS_ARG, {(void*)opt_recording_timestamp}, "set the recording timestamp ('now' to set the current time)", "time" },
    { "metadata", HAS_ARG, {(void*)opt_metadata}, "add metadata", "string=string" },
    { "dframes", OPT_INT | HAS_ARG, {(void*)&max_frames[AVMEDIA_TYPE_DATA]}, "set the number of data frames to record", "number" },
    { "coverfile", HAS_ARG, {(void*)opt_cover_file}, "add cover artwork", "coverfilepath" },
    { "benchmark", OPT_BOOL | OPT_EXPERT, {(void*)&do_benchmark},
      "add timings for benchmarking" },
    { "timelimit", HAS_ARG, {(void*)opt_timelimit}, "set max runtime in seconds", "limit" },
    { "dump", OPT_BOOL | OPT_EXPERT, {(void*)&do_pkt_dump},
      "dump each input packet" },
    { "hex", OPT_BOOL | OPT_EXPERT, {(void*)&do_hex_dump},
      "when dumping packets, also dump the payload" },
    { "re", OPT_BOOL | OPT_EXPERT, {(void*)&rate_emu}, "read input at native frame rate", "" },
    { "loop_input", OPT_BOOL | OPT_EXPERT, {(void*)&loop_input}, "deprecated, use -loop" },
    { "loop_output", HAS_ARG | OPT_INT | OPT_EXPERT, {(void*)&loop_output}, "deprecated, use -loop", "" },
    { "v", HAS_ARG, {(void*)opt_verbose}, "set ffmpeg verbosity level", "number" },
    { "target", HAS_ARG, {(void*)opt_target}, "specify target file type (\"vcd\", \"svcd\", \"dvd\", \"dvcam\", \"dvcpro\", \"dvcpro50\", \"dvcprohd\", \"imx30\", \"imx50\", \"xdcamhd422\", \"avcintra100\", \"avcintra200\")", "type" },
    { "threads",  HAS_ARG | OPT_EXPERT, {(void*)opt_thread_count}, "thread count", "count" },
    { "vsync", HAS_ARG | OPT_INT | OPT_EXPERT, {(void*)&video_sync_method}, "video sync method", "" },
    { "async", HAS_ARG | OPT_INT | OPT_EXPERT, {(void*)&audio_sync_method}, "audio sync method", "" },
    { "adrift_threshold", HAS_ARG | OPT_FLOAT | OPT_EXPERT, {(void*)&audio_drift_threshold}, "audio drift threshold", "threshold" },
    { "copyts", OPT_BOOL | OPT_EXPERT, {(void*)&copy_ts}, "copy timestamps" },
    { "copytb", OPT_BOOL | OPT_EXPERT, {(void*)&copy_tb}, "copy input stream time base when stream copying" },
    { "shortest", OPT_BOOL | OPT_EXPERT, {(void*)&opt_shortest}, "finish encoding within shortest input" }, //
    { "dts_delta_threshold", HAS_ARG | OPT_FLOAT | OPT_EXPERT, {(void*)&dts_delta_threshold}, "timestamp discontinuity delta threshold", "threshold" },
    { "programid", HAS_ARG | OPT_INT | OPT_EXPERT, {(void*)&opt_programid}, "desired program number", "" },
    { "xerror", OPT_BOOL, {(void*)&exit_on_error}, "exit on error", "error" },
    { "copyinkf", OPT_BOOL | OPT_EXPERT, {(void*)&copy_initial_nonkeyframes}, "copy initial non-keyframes" },

    /* video options */
    { "vframes", OPT_INT | HAS_ARG | OPT_VIDEO, {(void*)&max_frames[AVMEDIA_TYPE_VIDEO]}, "set the number of video frames to record", "number" },
    { "r", HAS_ARG | OPT_VIDEO, {(void*)opt_frame_rate}, "set frame rate (Hz value, fraction or abbreviation)", "rate" },
    { "s", HAS_ARG | OPT_VIDEO, {(void*)opt_frame_size}, "set frame size (WxH or abbreviation)", "size" },
    { "aspect", HAS_ARG | OPT_VIDEO, {(void*)opt_frame_aspect_ratio}, "set aspect ratio (4:3, 16:9 or 1.3333, 1.7777)", "aspect" },
    { "pix_fmt", HAS_ARG | OPT_EXPERT | OPT_VIDEO, {(void*)opt_frame_pix_fmt}, "set pixel format, 'list' as argument shows all the pixel formats supported", "format" },
    { "bits_per_raw_sample", OPT_INT | HAS_ARG | OPT_VIDEO, {(void*)&frame_bits_per_raw_sample}, "set the number of bits per raw sample", "number" },
    { "croptop",  HAS_ARG | OPT_VIDEO, {(void*)opt_frame_crop}, "Removed, use the crop filter instead", "size" },
    { "cropbottom", HAS_ARG | OPT_VIDEO, {(void*)opt_frame_crop}, "Removed, use the crop filter instead", "size" },
    { "cropleft", HAS_ARG | OPT_VIDEO, {(void*)opt_frame_crop}, "Removed, use the crop filter instead", "size" },
    { "cropright", HAS_ARG | OPT_VIDEO, {(void*)opt_frame_crop}, "Removed, use the crop filter instead", "size" },
    { "padtop", HAS_ARG | OPT_VIDEO, {(void*)opt_pad}, "Removed, use the pad filter instead", "size" },
    { "padbottom", HAS_ARG | OPT_VIDEO, {(void*)opt_pad}, "Removed, use the pad filter instead", "size" },
    { "padleft", HAS_ARG | OPT_VIDEO, {(void*)opt_pad}, "Removed, use the pad filter instead", "size" },
    { "padright", HAS_ARG | OPT_VIDEO, {(void*)opt_pad}, "Removed, use the pad filter instead", "size" },
    { "padcolor", HAS_ARG | OPT_VIDEO, {(void*)opt_pad}, "Removed, use the pad filter instead", "color" },
    { "intra", OPT_BOOL | OPT_EXPERT | OPT_VIDEO, {(void*)&intra_only}, "use only intra frames"},
    { "vn", OPT_BOOL | OPT_VIDEO, {(void*)&video_disable}, "disable video" },
    { "vdt", OPT_INT | HAS_ARG | OPT_EXPERT | OPT_VIDEO, {(void*)&video_discard}, "discard threshold", "n" },
    { "qscale", HAS_ARG | OPT_EXPERT | OPT_VIDEO, {(void*)opt_qscale}, "use fixed video quantizer scale (VBR)", "q" },
    { "rc_override", HAS_ARG | OPT_EXPERT | OPT_VIDEO, {(void*)opt_video_rc_override_string}, "rate control override for specific intervals", "override" },
    { "vcodec", HAS_ARG | OPT_VIDEO, {(void*)opt_codec}, "force video codec ('copy' to copy stream)", "codec" },
    { "sameq", OPT_BOOL | OPT_VIDEO, {(void*)&same_quality},
      "use same quantizer as source (implies VBR)" },
    { "pass", HAS_ARG | OPT_VIDEO, {(void*)opt_pass}, "select the pass number (1 or 2)", "n" },
    { "passlogfile", HAS_ARG | OPT_VIDEO, {(void*)&opt_passlogfile}, "select two pass log file name prefix", "prefix" },
    { "deinterlace", OPT_VIDEO, {(void*)&opt_deinterlace}, "deinterlace pictures" },
    { "psnr", OPT_BOOL | OPT_EXPERT | OPT_VIDEO, {(void*)&do_psnr}, "calculate PSNR of compressed frames" },
    { "vstats", OPT_EXPERT | OPT_VIDEO, {(void*)&opt_vstats}, "dump video coding statistics to file" },
    { "vstats_file", HAS_ARG | OPT_EXPERT | OPT_VIDEO, {(void*)opt_vstats_file}, "dump video coding statistics to file", "file" },
#if CONFIG_AVFILTER
    { "vf", HAS_ARG, {(void*)&opt_vf}, "add video filter", "filter list" },
#endif
    { "intra_matrix", HAS_ARG | OPT_EXPERT | OPT_VIDEO, {(void*)opt_intra_matrix}, "specify intra matrix coeffs", "matrix" },
    { "inter_matrix", HAS_ARG | OPT_EXPERT | OPT_VIDEO, {(void*)opt_inter_matrix}, "specify inter matrix coeffs", "matrix" },
    { "top", HAS_ARG | OPT_EXPERT | OPT_VIDEO, {(void*)opt_top_field_first}, "top=1/bottom=0/auto=-1 field first", "Removed, use -tff or -bff instead" },
    { "dc", OPT_INT | HAS_ARG | OPT_EXPERT | OPT_VIDEO, {(void*)&intra_dc_precision}, "intra_dc_precision", "precision" },
    { "vtag", HAS_ARG | OPT_EXPERT | OPT_VIDEO, {(void*)opt_codec_tag}, "force video tag/fourcc", "fourcc/tag" },
    { "newvideo", OPT_VIDEO, {(void*)opt_new_stream}, "add a new video stream to the current output stream" },
    { "vlang", HAS_ARG | OPT_STRING | OPT_VIDEO, {(void *)&video_language}, "set the ISO 639 language code (3 letters) of the current video stream" , "code" },
    { "qphist", OPT_BOOL | OPT_EXPERT | OPT_VIDEO, { (void *)&qp_hist }, "show QP histogram" },
    { "force_fps", OPT_BOOL | OPT_EXPERT | OPT_VIDEO, {(void*)&force_fps}, "force the selected framerate, disable the best supported framerate selection" },
    { "streamid", HAS_ARG | OPT_EXPERT, {(void*)opt_streamid}, "set the value of an outfile streamid", "streamIndex:value" },
    { "force_key_frames", OPT_STRING | HAS_ARG | OPT_EXPERT | OPT_VIDEO, {(void *)&forced_key_frames}, "force key frames at specified timestamps", "timestamps" },
    { "tff", OPT_EXPERT | OPT_VIDEO, {(void*)opt_interlaced}, "set top field first interlaced encoding", "interlacing" },
    { "bff", OPT_EXPERT | OPT_VIDEO, {(void*)opt_interlaced}, "set bottom field first interlaced encoding", "interlacing" },

    /* audio options */
    { "aframes", OPT_INT | HAS_ARG | OPT_AUDIO, {(void*)&max_frames[AVMEDIA_TYPE_AUDIO]}, "set the number of audio frames to record", "number" },
    { "aq", OPT_FLOAT | HAS_ARG | OPT_AUDIO, {(void*)&audio_qscale}, "set audio quality (codec-specific)", "quality", },
    { "ar", HAS_ARG | OPT_AUDIO, {(void*)opt_audio_rate}, "set audio sampling rate (in Hz)", "rate" },
    { "ac", HAS_ARG | OPT_AUDIO, {(void*)opt_audio_channels}, "set number of audio channels", "channels" },
    { "an", OPT_BOOL | OPT_AUDIO, {(void*)&audio_disable}, "disable audio" },
    { "acodec", HAS_ARG | OPT_AUDIO, {(void*)opt_codec}, "force audio codec ('copy' to copy stream)", "codec" },
    { "atag", HAS_ARG | OPT_EXPERT | OPT_AUDIO, {(void*)opt_codec_tag}, "force audio tag/fourcc", "fourcc/tag" },
    { "vol", OPT_INT | HAS_ARG | OPT_AUDIO, {(void*)&audio_volume}, "change audio volume (256=normal)" , "volume" }, //
    { "newaudio", OPT_AUDIO, {(void*)opt_new_stream}, "add a new audio stream to the current output stream" },
    { "alang", HAS_ARG | OPT_STRING | OPT_AUDIO, {(void *)&audio_language}, "set the ISO 639 language code (3 letters) of the current audio stream" , "code" },
    { "sample_fmt", HAS_ARG | OPT_EXPERT | OPT_AUDIO, {(void*)opt_audio_sample_fmt}, "set sample format, 'list' as argument shows all the sample formats supported", "format" },

    /* subtitle options */
    { "sn", OPT_BOOL | OPT_SUBTITLE, {(void*)&subtitle_disable}, "disable subtitle" },
    { "scodec", HAS_ARG | OPT_SUBTITLE, {(void*)opt_codec}, "force subtitle codec ('copy' to copy stream)", "codec" },
    { "newsubtitle", OPT_SUBTITLE, {(void*)opt_new_stream}, "add a new subtitle stream to the current output stream" },
    { "slang", HAS_ARG | OPT_STRING | OPT_SUBTITLE, {(void *)&subtitle_language}, "set the ISO 639 language code (3 letters) of the current subtitle stream" , "code" },
    { "stag", HAS_ARG | OPT_EXPERT | OPT_SUBTITLE, {(void*)opt_codec_tag}, "force subtitle tag/fourcc", "fourcc/tag" },

    /* grab options */
    { "vc", HAS_ARG | OPT_EXPERT | OPT_VIDEO | OPT_GRAB, {(void*)opt_video_channel}, "deprecated, use -channel", "channel" },
    { "tvstd", HAS_ARG | OPT_EXPERT | OPT_VIDEO | OPT_GRAB, {(void*)opt_video_standard}, "deprecated, use -standard", "standard" },
    { "isync", OPT_BOOL | OPT_EXPERT | OPT_GRAB, {(void*)&input_sync}, "sync read on input", "" },

    /* muxer options */
    { "muxdelay", OPT_FLOAT | HAS_ARG | OPT_EXPERT, {(void*)&mux_max_delay}, "set the maximum demux-decode delay", "seconds" },
    { "muxpreload", OPT_FLOAT | HAS_ARG | OPT_EXPERT, {(void*)&mux_preload}, "set the initial demux-decode delay", "seconds" },

    { "absf", HAS_ARG | OPT_AUDIO | OPT_EXPERT, {(void*)opt_bsf}, "", "bitstream_filter" },
    { "vbsf", HAS_ARG | OPT_VIDEO | OPT_EXPERT, {(void*)opt_bsf}, "", "bitstream_filter" },
    { "sbsf", HAS_ARG | OPT_SUBTITLE | OPT_EXPERT, {(void*)opt_bsf}, "", "bitstream_filter" },

    { "apre", HAS_ARG | OPT_AUDIO | OPT_EXPERT, {(void*)opt_preset}, "set the audio options to the indicated preset", "preset" },
    { "vpre", HAS_ARG | OPT_VIDEO | OPT_EXPERT, {(void*)opt_preset}, "set the video options to the indicated preset", "preset" },
    { "spre", HAS_ARG | OPT_SUBTITLE | OPT_EXPERT, {(void*)opt_preset}, "set the subtitle options to the indicated preset", "preset" },
    { "fpre", HAS_ARG | OPT_EXPERT, {(void*)opt_preset}, "set options from indicated preset file", "filename" },
    /* data codec support */
    { "dcodec", HAS_ARG | OPT_DATA, {(void*)opt_codec}, "force data codec ('copy' to copy stream)", "codec" },

    { "default", HAS_ARG | OPT_AUDIO | OPT_VIDEO | OPT_EXPERT, {(void*)opt_default}, "generic catch all option", "" },
    { NULL, },
};

int main(int argc, char **argv)
{
    int64_t ti;

    av_log_set_flags(AV_LOG_SKIP_REPEATED);

    if(argc>1 && !strcmp(argv[1], "-d")){
        run_as_daemon=1;
        verbose=-1;
        av_log_set_callback(log_callback_null);
        argc--;
        argv++;
    }

    avcodec_register_all();
#if CONFIG_AVDEVICE
    avdevice_register_all();
#endif
#if CONFIG_AVFILTER
    avfilter_register_all();
#endif
    av_register_all();

#if HAVE_ISATTY
    if(isatty(STDIN_FILENO))
        avio_set_interrupt_cb(decode_interrupt_cb);
#endif

    init_opts();

    if(verbose>=0)
        show_banner();

    /* parse options */
    parse_options(argc, argv, options, opt_output_file);

    if(nb_output_files <= 0 && nb_input_files == 0) {
        show_usage();
        av_log(NULL, AV_LOG_ERROR, "Use -h to get full help or, even better, run 'man ffmbc'\n");
        ffmpeg_exit(1);
    }

    /* file converter / grab */
    if (nb_output_files <= 0) {
        av_log(NULL, AV_LOG_ERROR, "At least one output file must be specified\n");
        ffmpeg_exit(1);
    }

    if (nb_input_files == 0) {
        av_log(NULL, AV_LOG_ERROR, "At least one input file must be specified\n");
        ffmpeg_exit(1);
    }

    ti = getutime();
    if (transcode(output_files, nb_output_files, input_files, nb_input_files,
                  stream_maps, nb_stream_maps) < 0)
        ffmpeg_exit(1);
    ti = getutime() - ti;
    if (do_benchmark) {
        int maxrss = getmaxrss() / 1024;
        av_log(NULL, AV_LOG_INFO, "bench: utime=%0.3fs maxrss=%ikB\n", ti / 1000000.0, maxrss);
    }

    return ffmpeg_exit(0);
}
