/*
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
 * Public dictionary API.
 * @deprecated
 *  AVDictionary is provided for compatibility with libav. It is both in
 *  implementation as well as API inefficient. It does not scale and is
 *  extremely slow with large dictionaries.
 *  It is recommended that new code uses our tree container from tree.c/h
 *  where applicable, which uses AVL trees to achieve O(log n) performance.
 */

#ifndef AVUTIL_DICT_H
#define AVUTIL_DICT_H

/**
 * @defgroup dict_api Public Dictionary API
 * @{
 * Dictionaries are used for storing key:value pairs. To create
 * an AVDictionary, simply pass an address of a NULL pointer to
 * av_dict_set(). NULL can be used as an empty dictionary wherever
 * a pointer to an AVDictionary is required.
 * Use av_dict_get() to retrieve an entry or iterate over all
 * entries and finally av_dict_free() to free the dictionary
 * and all its contents.
 *
 * @code
 * AVDictionary *d = NULL;                // "create" an empty dictionary
 * av_dict_set(&d, "foo", "bar", 0);      // add an entry
 *
 * char *k = av_strdup("key");            // if your strings are already allocated,
 * char *v = av_strdup("value");          // you can avoid copying them like this
 * av_dict_set(&d, k, v, AV_DICT_DONT_STRDUP_KEY | AV_DICT_DONT_STRDUP_VAL);
 *
 * AVDictionaryEntry *t = NULL;
 * while (t = av_dict_get(d, "", t, AV_DICT_IGNORE_SUFFIX)) {
 *     <....>                             // iterate over all entries in d
 * }
 *
 * av_dict_free(&d);
 * @endcode
 *
 * @}
 */

#define AV_DICT_MATCH_CASE      1
#define AV_DICT_IGNORE_SUFFIX   2
#define AV_DICT_DONT_STRDUP_KEY 4   /**< Take ownership of a key that's been
                                         allocated with av_malloc() and children. */
#define AV_DICT_DONT_STRDUP_VAL 8   /**< Take ownership of a value that's been
                                         allocated with av_malloc() and chilren. */
#define AV_DICT_DONT_OVERWRITE 16   ///< Don't overwrite existing entries.
#define AV_DICT_APPEND         32   /**< If the entry already exists, append to it.  Note that no
                                      delimiter is added, the strings are simply concatenated. */

/**
 * Used attributes: "language", "mime"
 */
typedef struct {
    char *key;
    char *value;
} AVMetadataAttribute;

typedef struct {
    unsigned count;
    AVMetadataAttribute *elems;
} AVMetadataAttributes;

enum AVMetadataType {
    METADATA_STRING, ///< UTF-8
    METADATA_INT,
    METADATA_FLOAT,
    METADATA_BYTEARRAY,
};

typedef struct {
    char *key;
    char *value;
    enum AVMetadataType type;
    unsigned len;
    AVMetadataAttributes *attributes;
} AVDictionaryEntry;

typedef struct AVDictionary AVDictionary;

/**
 * Get a dictionary entry with matching key.
 *
 * @param prev Set to the previous matching element to find the next.
 *             If set to NULL the first matching element is returned.
 * @param flags Allows case as well as suffix-insensitive comparisons.
 * @return Found entry or NULL, changing key or value leads to undefined behavior.
 */
AVDictionaryEntry *
av_dict_get(AVDictionary *m, const char *key, const AVDictionaryEntry *prev, int flags);

/**
 * Set the given entry in *pm, overwriting an existing entry.
 *
 * @param pm pointer to a pointer to a dictionary struct. If *pm is NULL
 * a dictionary struct is allocated and put in *pm.
 * @param key entry key to add to *pm (will be av_strduped depending on flags)
 * @param value entry value to add to *pm (will be av_strduped depending on flags).
 *        Passing a NULL value will cause an existing tag to be deleted.
 * @return >= 0 on success otherwise an error code <0
 */
int av_dict_set(AVDictionary **pm, const char *key, const char *value, int flags);
int av_dict_unset(AVDictionary *m, const char *key);

/**
 * Sets the given tag in m
 * @param pm pointer to a pointer to a metadata struct. If *pm is NULL
 * @param e point to the newly created entry
 * @param type tag type
 * @param key tag key to add to m (will be av_strduped depending on flags)
 * @param value tag value to add to m (will be av_strduped depending on flags)
 * @param value tag value len
 * @param flags flags regarding key and value parameters
 * @return >= 0 on success otherwise an error code <0
 */
int av_dict_set_custom(AVDictionary **pm, AVDictionaryEntry **e,
                       enum AVMetadataType type, const char *key,
                       const char *value, unsigned len, int flags);

int av_dict_set_int(AVDictionary **pm, const char *key, int value);
int av_dict_set_float(AVDictionary **pm, const char *key, double value);

/**
 * Get attribute of the given metadata with matching key.
 * @return Found tag or NULL, changing key or value leads to undefined behavior.
 */
const char *av_metadata_get_attribute(AVDictionaryEntry *tag, const char *key);

/**
 * Sets attribute to the given tag
 * @param key attribute key to add to tag (will be av_strduped)
 * @param value attribute value to add to tag (will be av_strduped)
 * @return >= 0 on success otherwise an error code <0
 */
int av_metadata_set_attribute(AVDictionaryEntry *tag, const char *key, const char *value);
int av_metadata_copy_attributes(AVDictionaryEntry *otag, AVDictionaryEntry *itag);

/**
 * Copy entries from one AVDictionary struct into another.
 * @param dst pointer to a pointer to a AVDictionary struct. If *dst is NULL,
 *            this function will allocate a struct for you and put it in *dst
 * @param src pointer to source AVDictionary struct
 * @param flags flags to use when setting entries in *dst
 * @note metadata is read using the AV_DICT_IGNORE_SUFFIX flag
 */
void av_dict_copy(AVDictionary **dst, AVDictionary *src, int flags);

/**
 * Free all the memory allocated for an AVDictionary struct
 * and all keys and values.
 */
void av_dict_free(AVDictionary **m);

#endif // AVUTIL_DICT_H
