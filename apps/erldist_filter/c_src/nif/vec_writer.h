/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef VEC_WRITER_H
#define VEC_WRITER_H

#ifdef __cplusplus
extern "C" {
#endif

#include "vec.h"

/* Type Definitions */

typedef struct vec_writer_s vec_writer_t;

struct vec_writer_s {
    uint8_t *buf;
    size_t len;
    size_t pos;
    vec_t *vec;
};

/* Function Declarations */

static int vec_writer_create(vec_writer_t *w, vec_t *vec, size_t skip);
static void vec_writer_destroy(vec_writer_t *w);
static int vec_writer_back_exact(vec_writer_t *w, size_t bytes_length);
static size_t vec_writer_offset(vec_writer_t *w);
static uint8_t *vec_writer_raw_bytes(vec_writer_t *w);
static int vec_writer_write_exact(vec_writer_t *w, const uint8_t *bytes, size_t bytes_length);
static int vec_writer_write_i32(vec_writer_t *w, int32_t value);
static int vec_writer_write_u8(vec_writer_t *w, uint8_t value);
static int vec_writer_write_u16(vec_writer_t *w, uint16_t value);
static int vec_writer_write_u32(vec_writer_t *w, uint32_t value);
static int vec_writer_write_u64(vec_writer_t *w, uint64_t value);
static size_t vec_writer_remaining_bytes(vec_writer_t *w);
static size_t vec_writer_remaining_capacity(vec_writer_t *w);
static int vec_writer_set_offset(vec_writer_t *w, size_t offset);
static int vec_writer_skip_exact(vec_writer_t *w, size_t skip);

/* Inline Function Definitions */

inline int
vec_writer_create(vec_writer_t *w, vec_t *vec, size_t skip)
{
    if (!vec_is_rewritable(vec) || vec_len(vec) < skip) {
        return 0;
    }
    if (vec_is_null(vec)) {
        w->buf = NULL;
        w->len = vec_capacity(vec);
        w->pos = skip;
        w->vec = vec;
        return 1;
    }
    w->buf = vec_buf(vec);
    if (w->buf == NULL) {
        return 0;
    }
    w->len = vec_capacity(vec);
    w->pos = skip;
    w->vec = vec;
    return 1;
}

inline void
vec_writer_destroy(vec_writer_t *w)
{
    w->buf = NULL;
    w->len = 0;
    w->pos = 0;
    w->vec = NULL;
}

inline int
vec_writer_back_exact(vec_writer_t *w, size_t bytes_length)
{
    if (w->pos < bytes_length) {
        return 0;
    }
    w->pos -= bytes_length;
    return 1;
}

inline void
vec_writer_clone(const vec_writer_t *src_writer, vec_writer_t *dst_writer)
{
    (void)memcpy(dst_writer, src_writer, sizeof(vec_writer_t));
}

inline size_t
vec_writer_offset(vec_writer_t *w)
{
    return w->pos;
}

inline uint8_t *
vec_writer_raw_bytes(vec_writer_t *w)
{
    if (w->buf == NULL || vec_writer_remaining_bytes(w) == 0) {
        return NULL;
    }
    return &w->buf[w->pos];
}

inline int
vec_writer_write_exact(vec_writer_t *w, const uint8_t *bytes, size_t bytes_length)
{
    if (vec_writer_remaining_bytes(w) < bytes_length) {
        return 0;
    }
    if (vec_is_null(w->vec)) {
        w->pos += bytes_length;
        if (w->vec->data.null.len < w->pos) {
            w->vec->data.null.len = w->pos;
        }
        return 1;
    }
    if (w->buf == NULL || bytes == NULL) {
        return 0;
    }
    (void)memcpy(&w->buf[w->pos], bytes, bytes_length);
    w->pos += bytes_length;
    if (vec_is_own_bin(w->vec) && w->vec->data.own_bin.len < w->pos) {
        w->vec->data.own_bin.len = w->pos;
    } else if (vec_is_own_mem(w->vec) && w->vec->data.own_mem.len < w->pos) {
        w->vec->data.own_mem.len = w->pos;
    }
    return 1;
}

inline int
vec_writer_write_i32(vec_writer_t *w, int32_t value)
{
    value = htobe32(value);
    if (!vec_writer_write_exact(w, (uint8_t *)&value, 4)) {
        return 0;
    }
    return 1;
}

inline int
vec_writer_write_u8(vec_writer_t *w, uint8_t value)
{
    if (!vec_writer_write_exact(w, (uint8_t *)&value, 1)) {
        return 0;
    }
    return 1;
}

inline int
vec_writer_write_u16(vec_writer_t *w, uint16_t value)
{
    value = htobe16(value);
    if (!vec_writer_write_exact(w, (uint8_t *)&value, 2)) {
        return 0;
    }
    return 1;
}

inline int
vec_writer_write_u32(vec_writer_t *w, uint32_t value)
{
    value = htobe32(value);
    if (!vec_writer_write_exact(w, (uint8_t *)&value, 4)) {
        return 0;
    }
    return 1;
}

inline int
vec_writer_write_u64(vec_writer_t *w, uint64_t value)
{
    value = htobe64(value);
    if (!vec_writer_write_exact(w, (uint8_t *)&value, 8)) {
        return 0;
    }
    return 1;
}

inline size_t
vec_writer_remaining_bytes(vec_writer_t *w)
{
    if (w->len <= w->pos) {
        return 0;
    }
    return (w->len - w->pos);
}

inline size_t
vec_writer_remaining_capacity(vec_writer_t *w)
{
    if (vec_capacity(w->vec) < vec_writer_offset(w)) {
        return 0;
    }
    return (vec_capacity(w->vec) - vec_writer_offset(w));
}

inline int
vec_writer_set_offset(vec_writer_t *w, size_t offset)
{
    if (w->len < offset) {
        return 0;
    }
    w->pos = offset;
    if (vec_is_null(w->vec) && w->vec->data.null.len < w->pos) {
        w->vec->data.null.len = w->pos;
    } else if (vec_is_own_bin(w->vec) && w->vec->data.own_bin.len < w->pos) {
        w->vec->data.own_bin.len = w->pos;
    } else if (vec_is_own_mem(w->vec) && w->vec->data.own_mem.len < w->pos) {
        w->vec->data.own_mem.len = w->pos;
    }
    return 1;
}

inline int
vec_writer_skip_exact(vec_writer_t *w, size_t skip)
{
    if (vec_writer_remaining_bytes(w) < skip) {
        return 0;
    }
    w->pos += skip;
    if (vec_is_null(w->vec) && w->vec->data.null.len < w->pos) {
        w->vec->data.null.len = w->pos;
    } else if (vec_is_own_bin(w->vec) && w->vec->data.own_bin.len < w->pos) {
        w->vec->data.own_bin.len = w->pos;
    } else if (vec_is_own_mem(w->vec) && w->vec->data.own_mem.len < w->pos) {
        w->vec->data.own_mem.len = w->pos;
    }
    return 1;
}

#ifdef __cplusplus
}
#endif

#endif
