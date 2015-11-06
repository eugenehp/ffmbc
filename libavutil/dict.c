/*
 * copyright (c) 2009 Michael Niedermayer
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
#include "avstring.h"
#include "dict.h"
#include "internal.h"
#include "mem.h"

static void av_dict_free_tag(AVDictionaryEntry *tag)
{
    av_freep(&tag->key);
    av_freep(&tag->value);
    if (tag->attributes) {
        while (tag->attributes->count--) {
            av_freep(&tag->attributes->elems[tag->attributes->count].key);
            av_freep(&tag->attributes->elems[tag->attributes->count].value);
        }
        av_freep(&tag->attributes->elems);
    }
    av_freep(&tag->attributes);
}

AVDictionaryEntry *
av_dict_get(AVDictionary *m, const char *key, const AVDictionaryEntry *prev, int flags)
{
    unsigned int i, j;

    if(!m)
        return NULL;

    if(prev) i= prev - m->elems + 1;
    else     i= 0;

    for(; i<m->count; i++){
        const char *s= m->elems[i].key;
        for (j = 0; toupper(s[j]) == toupper(key[j]) && key[j]; j++)
            ;
        if(key[j])
            continue;
        if(s[j] && !(flags & AV_DICT_IGNORE_SUFFIX))
            continue;
        return &m->elems[i];
    }
    return NULL;
}

int av_dict_unset(AVDictionary *m, const char *key)
{
    int i, j;

    for (i = 0; i < m->count; i++) {
        const char *s = m->elems[i].key;
        for (j = 0; toupper(s[j]) == toupper(key[j]) && key[j]; j++)
            ;
        if (key[j])
            continue;
        av_dict_free_tag(&m->elems[i]);
        memmove(m->elems + i, m->elems + i + 1, sizeof(*m->elems) * (m->count - i - 1));
        m->count--;
        return 0;
   }
    return -1;
}

int av_dict_set_custom(AVDictionary **pm, AVDictionaryEntry **ret,
                       enum AVMetadataType type, const char *key,
                       const char *value, unsigned len, int flags)
{
    AVDictionary      *m = *pm;
    AVDictionaryEntry *tag = av_dict_get(m, key, NULL, flags);

    if (!key || !pm || !len || !value)
        return -1;

    if (ret)
        *ret = NULL;

    if(!m)
        m = *pm = av_mallocz(sizeof(*m));
    if(!m)
        return AVERROR(ENOMEM);

    if(tag) {
        if (flags & AV_DICT_DONT_OVERWRITE)
            return 0;
        if (flags & AV_DICT_APPEND) {
            int new_len = tag->len+len+1;
            tag->value = av_realloc(tag->value, new_len);
            if (!tag->value)
                return AVERROR(ENOMEM);
            tag->len += len;
            av_strlcat(tag->value, value, new_len);
            goto out;
        } else
            av_dict_free_tag(tag);
    } else {
        AVDictionaryEntry *tmp;
        if (m->count >= UINT_MAX / sizeof(*m->elems))
            return -1;
        tmp = av_realloc(m->elems, (m->count+1) * sizeof(*m->elems));
        if(tmp) {
            m->elems = tmp;
        } else
            return AVERROR(ENOMEM);
        tag = &m->elems[m->count++];
    }

    if (flags & AV_DICT_DONT_STRDUP_KEY)
        tag->key = key;
    else
        tag->key = av_strdup(key);

    if (flags & AV_DICT_DONT_STRDUP_VAL) {
        tag->value = value;
    } else if (type == METADATA_BYTEARRAY) {
        tag->value = av_malloc(len);
        if (!tag->value)
            return AVERROR(ENOMEM);
        memcpy(tag->value, value, len);
    } else {
        tag->value = av_malloc(len+1);
        if (!tag->value)
            return AVERROR(ENOMEM);
        memcpy(tag->value, value, len);
        tag->value[len] = 0;
    }

    tag->len = len;
    tag->type = type;
    tag->attributes = NULL;

 out:

    if (ret)
        *ret = tag;

    return 0;
}

int av_dict_set(AVDictionary **pm, const char *key, const char *value, int flags)
{
    if (!value)
        return -1;
    return av_dict_set_custom(pm, NULL, METADATA_STRING, key, value, strlen(value), flags);
}

int av_dict_set_int(AVDictionary **pm, const char *key, int value)
{
    char buf[16];
    int len = snprintf(buf, sizeof(buf), "%d", value);
    return av_dict_set_custom(pm, NULL, METADATA_INT, key, buf, len, 0);
}

int av_dict_set_float(AVDictionary **pm, const char *key, double value)
{
    char buf[16];
    int len = snprintf(buf, sizeof(buf), "%f", value);
    return av_dict_set_custom(pm, NULL, METADATA_FLOAT, key, buf, len, 0);
}

void av_dict_free(AVDictionary **pm)
{
    AVDictionary *m = *pm;

    if (m) {
        while(m->count--)
            av_dict_free_tag(&m->elems[m->count]);
        av_free(m->elems);
    }
    av_freep(pm);
}

void av_dict_copy(AVDictionary **dst, AVDictionary *src, int flags)
{
    AVDictionaryEntry *t = NULL, *tag;

    while ((t = av_dict_get(src, "", t, AV_DICT_IGNORE_SUFFIX))) {
        av_dict_set_custom(dst, &tag, t->type, t->key,
                           t->value, t->len, flags);
        if (tag)
            av_metadata_copy_attributes(tag, t);
    }
}

const char *av_metadata_get_attribute(AVDictionaryEntry *tag, const char *key)
{
    int i, j;

    if (!tag->attributes)
        return NULL;

    for (i = 0; i < tag->attributes->count; i++) {
        char *s = tag->attributes->elems[i].key;
        for (j = 0; toupper(s[j]) == toupper(key[j]) && key[j]; j++)
            ;
        if (key[j])
            continue;
        return tag->attributes->elems[i].value;
    }
    return NULL;
}

int av_metadata_set_attribute(AVDictionaryEntry *tag, const char *key, const char *value)
{
    AVMetadataAttribute *attribute;

    if (!tag->attributes)
        tag->attributes = av_mallocz(sizeof(*tag->attributes));
    if (!tag->attributes)
        return AVERROR(ENOMEM);

    if (tag->attributes->count == UINT_MAX / sizeof(*attribute))
        return -1;

    attribute = av_realloc(tag->attributes->elems,
                           (tag->attributes->count+1)*sizeof(*attribute));
    if (!attribute)
        return AVERROR(ENOMEM);

    tag->attributes->elems = attribute;
    attribute = &tag->attributes->elems[tag->attributes->count];
    attribute->key = av_strdup(key);
    attribute->value = av_strdup(value);

    tag->attributes->count++;

    return 0;
}

int av_metadata_copy_attributes(AVDictionaryEntry *otag, AVDictionaryEntry *itag)
{
    int i;

    if (!itag->attributes)
        return 0;

    for (i = 0; i < itag->attributes->count; i++)
        av_metadata_set_attribute(otag, itag->attributes->elems[i].key,
                                  itag->attributes->elems[i].value);
    return 0;
}
