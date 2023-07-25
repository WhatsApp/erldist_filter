/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef VEC_READER_H
#define VEC_READER_H

#ifdef __cplusplus
extern "C" {
#endif

#include "vec.h"

/* Type Definitions */

typedef struct vec_reader_s vec_reader_t;

struct vec_reader_s {
    const uint8_t *buf;
    size_t len;
    size_t pos;
    const vec_t *vec;
};

/* Function Declarations */

static int vec_reader_create(vec_reader_t *r, const vec_t *vec, size_t skip);
static void vec_reader_destroy(vec_reader_t *r);
static int vec_reader_back_exact(vec_reader_t *r, size_t bytes_length);
static void vec_reader_clone(vec_reader_t *dst_reader, const vec_reader_t *src_reader);
static size_t vec_reader_offset(const vec_reader_t *r);
static int vec_reader_peek_exact(const vec_reader_t *r, uint8_t *bytes, size_t bytes_length);
static int vec_reader_peek_i32(const vec_reader_t *r, int32_t *value);
static int vec_reader_peek_u8(const vec_reader_t *r, uint8_t *value);
static int vec_reader_peek_u16(const vec_reader_t *r, uint16_t *value);
static int vec_reader_peek_u32(const vec_reader_t *r, uint32_t *value);
static int vec_reader_peek_u64(const vec_reader_t *r, uint64_t *value);
static const uint8_t *vec_reader_raw_bytes(vec_reader_t *r);
static int vec_reader_read_exact(vec_reader_t *r, uint8_t *bytes, size_t bytes_length);
static int vec_reader_read_i32(vec_reader_t *r, int32_t *value);
static int vec_reader_read_u8(vec_reader_t *r, uint8_t *value);
static int vec_reader_read_u16(vec_reader_t *r, uint16_t *value);
static int vec_reader_read_u32(vec_reader_t *r, uint32_t *value);
static int vec_reader_read_u64(vec_reader_t *r, uint64_t *value);
static size_t vec_reader_remaining_bytes(const vec_reader_t *r);
static size_t vec_reader_remaining_capacity(const vec_reader_t *r);
static int vec_reader_set_offset(vec_reader_t *r, size_t offset);
static int vec_reader_skip_exact(vec_reader_t *r, size_t skip);

/* Inline Function Definitions */

inline int
vec_reader_create(vec_reader_t *r, const vec_t *vec, size_t skip)
{
    if (vec_len(vec) < skip) {
        return 0;
    }
    r->buf = vec_buf(vec);
    if (r->buf == NULL) {
        return 0;
    }
    r->len = vec_len(vec);
    r->pos = skip;
    r->vec = vec;
    return 1;
}

inline void
vec_reader_destroy(vec_reader_t *r)
{
    r->buf = NULL;
    r->len = 0;
    r->pos = 0;
    r->vec = NULL;
}

inline int
vec_reader_back_exact(vec_reader_t *r, size_t bytes_length)
{
    if (r->pos < bytes_length) {
        return 0;
    }
    r->pos -= bytes_length;
    return 1;
}

inline void
vec_reader_clone(vec_reader_t *dst_reader, const vec_reader_t *src_reader)
{
    (void)memcpy(dst_reader, src_reader, sizeof(vec_reader_t));
}

inline size_t
vec_reader_offset(const vec_reader_t *r)
{
    return r->pos;
}

inline int
vec_reader_peek_exact(const vec_reader_t *r, uint8_t *bytes, size_t bytes_length)
{
    if (vec_reader_remaining_bytes(r) < bytes_length) {
        return 0;
    }
    (void)memcpy(bytes, &r->buf[r->pos], bytes_length);
    return 1;
}

inline int
vec_reader_peek_i32(const vec_reader_t *r, int32_t *value)
{
    if (!vec_reader_peek_exact(r, (uint8_t *)value, 4)) {
        return 0;
    }
    *value = be32toh(*value);
    return 1;
}

inline int
vec_reader_peek_u8(const vec_reader_t *r, uint8_t *value)
{
    if (!vec_reader_peek_exact(r, (uint8_t *)value, 1)) {
        return 0;
    }
    return 1;
}

inline int
vec_reader_peek_u16(const vec_reader_t *r, uint16_t *value)
{
    if (!vec_reader_peek_exact(r, (uint8_t *)value, 2)) {
        return 0;
    }
    *value = be16toh(*value);
    return 1;
}

inline int
vec_reader_peek_u32(const vec_reader_t *r, uint32_t *value)
{
    if (!vec_reader_peek_exact(r, (uint8_t *)value, 4)) {
        return 0;
    }
    *value = be32toh(*value);
    return 1;
}

inline int
vec_reader_peek_u64(const vec_reader_t *r, uint64_t *value)
{
    if (!vec_reader_peek_exact(r, (uint8_t *)value, 8)) {
        return 0;
    }
    *value = be64toh(*value);
    return 1;
}

inline const uint8_t *
vec_reader_raw_bytes(vec_reader_t *r)
{
    if (r->buf == NULL || r->pos > r->len) {
        return NULL;
    }
    return &r->buf[r->pos];
}

inline int
vec_reader_read_exact(vec_reader_t *r, uint8_t *bytes, size_t bytes_length)
{
    if (vec_reader_remaining_bytes(r) < bytes_length) {
        return 0;
    }
    (void)memcpy(bytes, &r->buf[r->pos], bytes_length);
    r->pos += bytes_length;
    return 1;
}

inline int
vec_reader_read_i32(vec_reader_t *r, int32_t *value)
{
    if (!vec_reader_read_exact(r, (uint8_t *)value, 4)) {
        return 0;
    }
    *value = be32toh(*value);
    return 1;
}

inline int
vec_reader_read_u8(vec_reader_t *r, uint8_t *value)
{
    if (!vec_reader_read_exact(r, (uint8_t *)value, 1)) {
        return 0;
    }
    return 1;
}

inline int
vec_reader_read_u16(vec_reader_t *r, uint16_t *value)
{
    if (!vec_reader_read_exact(r, (uint8_t *)value, 2)) {
        return 0;
    }
    *value = be16toh(*value);
    return 1;
}

inline int
vec_reader_read_u32(vec_reader_t *r, uint32_t *value)
{
    if (!vec_reader_read_exact(r, (uint8_t *)value, 4)) {
        return 0;
    }
    *value = be32toh(*value);
    return 1;
}

inline int
vec_reader_read_u64(vec_reader_t *r, uint64_t *value)
{
    if (!vec_reader_read_exact(r, (uint8_t *)value, 8)) {
        return 0;
    }
    *value = be64toh(*value);
    return 1;
}

inline size_t
vec_reader_remaining_bytes(const vec_reader_t *r)
{
    if (r->len <= r->pos) {
        return 0;
    }
    return (r->len - r->pos);
}

inline size_t
vec_reader_remaining_capacity(const vec_reader_t *r)
{
    if (vec_capacity(r->vec) < vec_reader_offset(r)) {
        return 0;
    }
    return (vec_capacity(r->vec) - vec_reader_offset(r));
}

inline int
vec_reader_set_offset(vec_reader_t *r, size_t offset)
{
    if (r->len < offset) {
        return 0;
    }
    r->pos = offset;
    return 1;
}

inline int
vec_reader_skip_exact(vec_reader_t *r, size_t skip)
{
    if (vec_reader_remaining_bytes(r) < skip) {
        return 0;
    }
    r->pos += skip;
    return 1;
}

#ifdef __cplusplus
}
#endif

#endif
