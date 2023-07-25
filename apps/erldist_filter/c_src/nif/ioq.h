/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef IOQ_H
#define IOQ_H

#ifdef __cplusplus
extern "C" {
#endif

#include "core/portable_endian.h"

#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <erl_nif.h>

/* Type Definitions */

typedef struct ioq_s ioq_t;

struct ioq_s {
    ErlNifIOQueue *nif_ioq;
};

typedef struct ioq_iovec_s ioq_iovec_t;

struct ioq_iovec_s {
    int iovcnt;
    size_t size;
    SysIOVec *iov;
};

typedef struct ioq_reader_s ioq_reader_t;

struct ioq_reader_s {
    ioq_iovec_t iovec;
    ioq_t *ioq;
    int iovcnt_offset;
    size_t iov_base_offset;
    size_t byte_offset;
};

/* Function Declarations */

static void ioq_init_free(ioq_t *ioq);
static int ioq_is_free(const ioq_t *ioq);
static int ioq_create(ioq_t *ioq);
extern void ioq_destroy(ioq_t *ioq);
extern int ioq_compact(ErlNifEnv *env, ioq_t *src_ioq);
static int ioq_deq(ioq_t *ioq, size_t count, size_t *new_size);
extern int ioq_deq_into_bytes(ioq_t *ioq, uint8_t *bytes, size_t count, size_t *new_size);
static int ioq_peek(ioq_t *ioq, ioq_iovec_t *iovec);
static int ioq_peek_raw_head_binary_term(ErlNifEnv *env, ioq_t *ioq, size_t *size, ERL_NIF_TERM *bin_term);
static int ioq_peek_raw_head_inspect_as_binary(ErlNifEnv *env, ioq_t *ioq, size_t *size, ErlNifBinary *bin);
static size_t ioq_peek_raw_head_size(ioq_t *ioq);
static SysIOVec *ioq_peek_raw_head_sys_iov(ioq_t *ioq, int *iovcnt);
static size_t ioq_size(const ioq_t *ioq);
extern int ioq_inspect_iovec_and_enqv(ErlNifEnv *env, size_t max_elements, ERL_NIF_TERM iovec_term, ERL_NIF_TERM *tail, ioq_t *ioq);
extern int ioq_into_ioq(ErlNifEnv *env, ioq_t *src_ioq, ioq_t *dst_ioq);
extern int ioq_into_ioq_compact(ErlNifEnv *env, ioq_t *src_ioq, ioq_t *dst_ioq);
extern int ioq_into_iovec(ErlNifEnv *env, ioq_t *src_ioq, ERL_NIF_TERM *iovec_term);
extern int ioq_into_iovec_and_ioq(ErlNifEnv *env, ioq_t *src_ioq, ERL_NIF_TERM *iovec_term, ioq_t *dst_ioq);
extern int ioq_into_new_binary(ErlNifEnv *env, ioq_t *src_ioq, ErlNifBinary *dst_bin);

static int ioq_reader_create(ioq_reader_t *r, ioq_t *ioq);
extern void ioq_reader_destroy(ioq_reader_t *r);
extern int ioq_reader_back_exact(ioq_reader_t *r, size_t bytes_length);
static void ioq_reader_clone(const ioq_reader_t *src_reader, ioq_reader_t *dst_reader);
static int ioq_reader_consume(ioq_reader_t *r);
static int ioq_reader_consume_remaining(ioq_reader_t *r);
static size_t ioq_reader_offset(ioq_reader_t *r);
extern int ioq_reader_read_exact(ioq_reader_t *r, uint8_t *bytes, size_t bytes_length);
static int ioq_reader_read_u8(ioq_reader_t *r, uint8_t *value);
static int ioq_reader_read_u16(ioq_reader_t *r, uint16_t *value);
static int ioq_reader_read_u32(ioq_reader_t *r, uint32_t *value);
static int ioq_reader_read_u64(ioq_reader_t *r, uint64_t *value);
static size_t ioq_reader_remaining_bytes(ioq_reader_t *r);
static int ioq_reader_set_limit(ioq_reader_t *r, size_t limit);
static int ioq_reader_set_remaining(ioq_reader_t *r, size_t remaining);
static int ioq_reader_skip_exact(ioq_reader_t *r, size_t skip);
static int ioq_reader_skip_remaining(ioq_reader_t *r);
extern int ioq_reader_split_into_ioq(ErlNifEnv *env, ioq_reader_t *r, ioq_t *dst_ioq);
extern int ioq_reader_split_into_ioq_compact(ErlNifEnv *env, ioq_reader_t *r, ioq_t *dst_ioq);
extern int ioq_reader_split_into_iovec(ErlNifEnv *env, ioq_reader_t *r, ERL_NIF_TERM *iovec_term);
extern int ioq_reader_split_into_iovec_and_ioq(ErlNifEnv *env, ioq_reader_t *r, ERL_NIF_TERM *iovec_term, ioq_t *dst_ioq);
extern int ioq_reader_split_into_new_binary(ErlNifEnv *env, ioq_reader_t *r, ErlNifBinary *dst_bin);

/* Inline Function Definitions */

inline void
ioq_init_free(ioq_t *ioq)
{
    ioq->nif_ioq = NULL;
    return;
}

inline int
ioq_is_free(const ioq_t *ioq)
{
    return (ioq->nif_ioq == NULL);
}

inline int
ioq_create(ioq_t *ioq)
{
    ioq->nif_ioq = enif_ioq_create(ERL_NIF_IOQ_NORMAL);
    if (ioq->nif_ioq == NULL) {
        return 0;
    }
    return 1;
}

inline int
ioq_deq(ioq_t *ioq, size_t count, size_t *new_size)
{
    return enif_ioq_deq(ioq->nif_ioq, count, new_size);
}

inline int
ioq_peek(ioq_t *ioq, ioq_iovec_t *iovec)
{
    iovec->iov = ioq_peek_raw_head_sys_iov(ioq, &iovec->iovcnt);
    if (iovec->iov == NULL) {
        return 0;
    }
    iovec->size = ioq_size(ioq);
    return 1;
}

inline int
ioq_peek_raw_head_binary_term(ErlNifEnv *env, ioq_t *ioq, size_t *size, ERL_NIF_TERM *bin_term)
{
    return enif_ioq_peek_head(env, ioq->nif_ioq, size, bin_term);
}

inline int
ioq_peek_raw_head_inspect_as_binary(ErlNifEnv *env, ioq_t *ioq, size_t *size, ErlNifBinary *bin)
{
    ERL_NIF_TERM bin_term;
    if (!ioq_peek_raw_head_binary_term(env, ioq, size, &bin_term)) {
        return 0;
    }
    return enif_inspect_binary(env, bin_term, bin);
}

inline size_t
ioq_peek_raw_head_size(ioq_t *ioq)
{
    SysIOVec *sys_iov = NULL;
    int iovcnt = 0;

    sys_iov = ioq_peek_raw_head_sys_iov(ioq, &iovcnt);
    if (sys_iov != NULL && iovcnt > 0) {
        return (size_t)(sys_iov->iov_len);
    }
    return 0;
}

inline SysIOVec *
ioq_peek_raw_head_sys_iov(ioq_t *ioq, int *iovcnt)
{
    return enif_ioq_peek(ioq->nif_ioq, iovcnt);
}

inline size_t
ioq_size(const ioq_t *ioq)
{
    return enif_ioq_size(ioq->nif_ioq);
}

inline int
ioq_reader_create(ioq_reader_t *r, ioq_t *ioq)
{
    if (!ioq_peek(ioq, &r->iovec)) {
        r->iovec.iovcnt = 0;
        r->iovec.size = 0;
        r->iovec.iov = NULL;
    }
    r->ioq = ioq;
    r->iovcnt_offset = 0;
    r->iov_base_offset = 0;
    r->byte_offset = 0;
    return 1;
}

inline void
ioq_reader_clone(const ioq_reader_t *src_reader, ioq_reader_t *dst_reader)
{
    (void)memcpy(dst_reader, src_reader, sizeof(ioq_reader_t));
}

inline int
ioq_reader_consume(ioq_reader_t *r)
{
    if (r->byte_offset == 0) {
        return 1;
    }
    if (!ioq_deq(r->ioq, r->byte_offset, NULL)) {
        return 0;
    }
    return ioq_reader_create(r, r->ioq);
}

inline int
ioq_reader_consume_remaining(ioq_reader_t *r)
{
    if (!ioq_reader_skip_remaining(r)) {
        return 0;
    }
    return ioq_reader_consume(r);
}

inline size_t
ioq_reader_offset(ioq_reader_t *r)
{
    return r->byte_offset;
}

inline int
ioq_reader_read_u8(ioq_reader_t *r, uint8_t *value)
{
    if (!ioq_reader_read_exact(r, (uint8_t *)value, 1)) {
        return 0;
    }
    return 1;
}

inline int
ioq_reader_read_u16(ioq_reader_t *r, uint16_t *value)
{
    if (!ioq_reader_read_exact(r, (uint8_t *)value, 2)) {
        return 0;
    }
    *value = be16toh(*value);
    return 1;
}

inline int
ioq_reader_read_u32(ioq_reader_t *r, uint32_t *value)
{
    if (!ioq_reader_read_exact(r, (uint8_t *)value, 4)) {
        return 0;
    }
    *value = be32toh(*value);
    return 1;
}

inline int
ioq_reader_read_u64(ioq_reader_t *r, uint64_t *value)
{
    if (!ioq_reader_read_exact(r, (uint8_t *)value, 8)) {
        return 0;
    }
    *value = be64toh(*value);
    return 1;
}

inline size_t
ioq_reader_remaining_bytes(ioq_reader_t *r)
{
    if (r->byte_offset > r->iovec.size) {
        return 0;
    }
    return (r->iovec.size - r->byte_offset);
}

inline int
ioq_reader_set_limit(ioq_reader_t *r, size_t limit)
{
    if (r->iovec.size < limit) {
        return 0;
    }
    r->iovec.size = limit;
    return 1;
}

inline int
ioq_reader_set_remaining(ioq_reader_t *r, size_t remaining)
{
    return ioq_reader_set_limit(r, r->byte_offset + remaining);
}

inline int
ioq_reader_skip_exact(ioq_reader_t *r, size_t skip)
{
    return ioq_reader_read_exact(r, NULL, skip);
}

inline int
ioq_reader_skip_remaining(ioq_reader_t *r)
{
    r->iovcnt_offset = r->iovec.iovcnt;
    r->iov_base_offset = 0;
    r->byte_offset = r->iovec.size;
    return 1;
}

#ifdef __cplusplus
}
#endif

#endif
