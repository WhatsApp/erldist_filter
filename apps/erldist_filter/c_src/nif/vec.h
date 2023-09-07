/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef VEC_H
#define VEC_H

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

#include "ioq.h"
#include "slice.h"

#include "core/xnif_trace.h"

/* Type Definitions */

typedef struct vec_s vec_t;
typedef enum vec_tag_t vec_tag_t;
typedef struct vec_data_null_s vec_data_null_t;
typedef struct vec_data_own_bin_s vec_data_own_bin_t;
typedef struct vec_data_own_mem_s vec_data_own_mem_t;
typedef struct vec_data_ref_bin_s vec_data_ref_bin_t;
typedef struct vec_data_ref_ioq_s vec_data_ref_ioq_t;
typedef slice_t vec_data_slice_t;

struct vec_data_null_s {
    size_t len;
};

struct vec_data_own_bin_s {
    ErlNifBinary bin;
    size_t len;
};

struct vec_data_own_mem_s {
    size_t capacity;
    size_t len;
    uint8_t *mem;
};

struct vec_data_ref_bin_s {
    ErlNifEnv *env;
    ERL_NIF_TERM bin_term;
    ErlNifBinary bin;
};

struct vec_data_ref_ioq_s {
    ErlNifEnv *env;
    ioq_t *ioq;
    ErlNifBinary bin;
};

enum vec_tag_t {
    VEC_TAG_FREE = 0,
    VEC_TAG_NULL,
    VEC_TAG_OWN_BIN,
    VEC_TAG_OWN_MEM,
    VEC_TAG_REF_BIN,
    VEC_TAG_REF_IOQ,
    VEC_TAG_SLICE,
};

struct vec_s {
    vec_tag_t tag;
    union {
        vec_data_null_t null;
        vec_data_own_bin_t own_bin;
        vec_data_own_mem_t own_mem;
        vec_data_ref_bin_t ref_bin;
        vec_data_ref_ioq_t ref_ioq;
        vec_data_slice_t slice;
    } data;
};

/* Function Declarations */

static int vec_init_free(vec_t *vec);
static int vec_create_null(vec_t *vec);
extern int vec_create_owned(vec_t *vec, size_t capacity);
extern int vec_create_owned_mem(vec_t *vec, size_t capacity);
extern int vec_create_from_bin(vec_t *dst_vec, ERL_NIF_TERM src_bin_term);
extern int vec_create_from_ioq(vec_t *dst_vec, size_t dst_size, ioq_t *src_ioq);
extern int vec_create_from_ioq_mem(vec_t *dst_vec, size_t dst_size, ioq_t *src_ioq);
extern int vec_create_from_slice(vec_t *dst_vec, const uint8_t *head, const uint8_t *tail);
extern int vec_realloc_owned(vec_t *vec, size_t new_capacity);
extern void vec_destroy(vec_t *vec);

static uint8_t *vec_buf(const vec_t *vec);
static uint8_t *vec_buf_tail(const vec_t *vec);
static uint8_t *vec_buf_tail_unsafe(const vec_t *vec);
static size_t vec_capacity(const vec_t *vec);
static int vec_clone_slice(vec_t *dst_vec, const vec_t *src_vec);
extern int vec_clone_sub_binary(vec_t *dst_vec, const vec_t *src_vec, size_t pos, size_t len);
static int vec_contains_slice(const vec_t *vec, const uint8_t *head, const uint8_t *tail);
extern ERL_NIF_TERM vec_into_binary_term(ErlNifEnv *env, vec_t *vec);
extern int vec_into_owned(vec_t *dst_vec, vec_t *src_vec);
extern int vec_into_owned_mem(vec_t *dst_vec, vec_t *src_vec);
static int vec_is_free(const vec_t *vec);
static int vec_is_null(const vec_t *vec);
static int vec_is_own_bin(const vec_t *vec);
static int vec_is_own_mem(const vec_t *vec);
static int vec_is_owned(const vec_t *vec);
static int vec_is_readable(const vec_t *vec);
static int vec_is_ref_bin(const vec_t *vec);
static int vec_is_ref_ioq(const vec_t *vec);
static int vec_is_rewritable(const vec_t *vec);
static int vec_is_slice(const vec_t *vec);
static int vec_is_writable(const vec_t *vec);
static size_t vec_len(const vec_t *vec);
static int vec_move(vec_t *dst_vec, vec_t *src_vec);
static size_t vec_remaining_writable_bytes(const vec_t *vec);
extern int vec_write_from_ioq(vec_t *dst_vec, ioq_t *src_ioq);
static int vec_write_from_vec(vec_t *dst_vec, vec_t *src_vec, size_t skip);
extern int vec_write_from_vec_copy(vec_t *dst_vec, vec_t *src_vec, size_t skip);

/* Inline Function Definitions */

inline int
vec_init_free(vec_t *vec)
{
    XNIF_TRACE_F("%s:%d vec_init_free()\n", __FILE__, __LINE__);
    vec->tag = VEC_TAG_FREE;
    return 1;
}

inline int
vec_create_null(vec_t *vec)
{
    XNIF_TRACE_F("%s:%d vec_create_null()\n", __FILE__, __LINE__);
    if (!vec_is_free(vec)) {
        return 0;
    }
    vec->tag = VEC_TAG_NULL;
    vec->data.null.len = 0;
    return 1;
}

inline uint8_t *
vec_buf(const vec_t *vec)
{
    switch (vec->tag) {
    case VEC_TAG_OWN_BIN:
        return (uint8_t *)(vec->data.own_bin.bin.data);
    case VEC_TAG_OWN_MEM:
        return (uint8_t *)(vec->data.own_mem.mem);
    case VEC_TAG_REF_BIN:
        return (uint8_t *)(vec->data.ref_bin.bin.data);
    case VEC_TAG_REF_IOQ:
        return (uint8_t *)(vec->data.ref_ioq.bin.data);
    case VEC_TAG_SLICE:
        return (uint8_t *)(vec->data.slice.head);
    default:
        return NULL;
    }
}

inline uint8_t *
vec_buf_tail(const vec_t *vec)
{
    uint8_t *buf = NULL;
    if (vec->tag == VEC_TAG_SLICE) {
        return (uint8_t *)(vec->data.slice.tail);
    }
    buf = vec_buf(vec);
    if (buf == NULL) {
        return NULL;
    }
    return (uint8_t *)(buf + vec_len(vec));
}

inline uint8_t *
vec_buf_tail_unsafe(const vec_t *vec)
{
    uint8_t *buf = NULL;
    if (vec->tag == VEC_TAG_SLICE) {
        return (uint8_t *)(vec->data.slice.tail);
    }
    buf = vec_buf(vec);
    if (buf == NULL) {
        return NULL;
    }
    return (uint8_t *)(buf + vec_capacity(vec));
}

inline size_t
vec_capacity(const vec_t *vec)
{
    switch (vec->tag) {
    case VEC_TAG_NULL:
        return (size_t)(SIZE_MAX);
    case VEC_TAG_OWN_BIN:
        return (size_t)(vec->data.own_bin.bin.size);
    case VEC_TAG_OWN_MEM:
        return (size_t)(vec->data.own_mem.capacity);
    case VEC_TAG_REF_BIN:
        return (size_t)(vec->data.ref_bin.bin.size);
    case VEC_TAG_REF_IOQ:
        return (size_t)(vec->data.ref_ioq.bin.size);
    case VEC_TAG_SLICE:
        return (size_t)(vec->data.slice.tail - vec->data.slice.head);
    default:
        return 0;
    }
}

inline int
vec_clone_slice(vec_t *dst_vec, const vec_t *src_vec)
{
    if (!vec_is_free(dst_vec) || !vec_is_slice(src_vec)) {
        return 0;
    }
    return vec_create_from_slice(dst_vec, src_vec->data.slice.head, src_vec->data.slice.tail);
}

inline int
vec_contains_slice(const vec_t *vec, const uint8_t *head, const uint8_t *tail)
{
    const uint8_t *vec_head = vec_buf(vec);
    const uint8_t *vec_tail = vec_buf_tail_unsafe(vec);
    if (vec_head == NULL || vec_tail == NULL || head == NULL || tail == NULL || head > tail ||
        !(head >= vec_head && tail <= vec_tail)) {
        return 0;
    }
    return 1;
}

inline int
vec_is_free(const vec_t *vec)
{
    return (vec != NULL && vec->tag == VEC_TAG_FREE);
}

inline int
vec_is_null(const vec_t *vec)
{
    return (vec != NULL && vec->tag == VEC_TAG_NULL);
}

inline int
vec_is_own_bin(const vec_t *vec)
{
    return (vec != NULL && vec->tag == VEC_TAG_OWN_BIN);
}

inline int
vec_is_own_mem(const vec_t *vec)
{
    return (vec != NULL && vec->tag == VEC_TAG_OWN_MEM);
}

inline int
vec_is_owned(const vec_t *vec)
{
    return (vec != NULL && (vec->tag == VEC_TAG_OWN_BIN || vec->tag == VEC_TAG_OWN_MEM));
}

inline int
vec_is_readable(const vec_t *vec)
{
    return (!vec_is_free(vec) && !vec_is_null(vec));
}

inline int
vec_is_ref_bin(const vec_t *vec)
{
    return (vec != NULL && vec->tag == VEC_TAG_REF_BIN);
}

inline int
vec_is_ref_ioq(const vec_t *vec)
{
    return (vec != NULL && vec->tag == VEC_TAG_REF_IOQ);
}

inline int
vec_is_rewritable(const vec_t *vec)
{
    return (vec != NULL && (vec->tag == VEC_TAG_NULL || vec->tag == VEC_TAG_OWN_BIN || vec->tag == VEC_TAG_OWN_MEM));
}

inline int
vec_is_slice(const vec_t *vec)
{
    return (vec != NULL && vec->tag == VEC_TAG_SLICE);
}

inline int
vec_is_writable(const vec_t *vec)
{
    return (vec != NULL &&
            (vec->tag == VEC_TAG_NULL || (vec->tag == VEC_TAG_OWN_BIN && vec->data.own_bin.len < vec->data.own_bin.bin.size) ||
             (vec->tag == VEC_TAG_OWN_MEM && vec->data.own_mem.len < vec->data.own_mem.capacity)));
}

inline size_t
vec_len(const vec_t *vec)
{
    switch (vec->tag) {
    case VEC_TAG_NULL:
        return vec->data.null.len;
    case VEC_TAG_OWN_BIN:
        return vec->data.own_bin.len;
    case VEC_TAG_OWN_MEM:
        return vec->data.own_mem.len;
    case VEC_TAG_REF_BIN:
        return (size_t)(vec->data.ref_bin.bin.size);
    case VEC_TAG_REF_IOQ:
        return (size_t)(vec->data.ref_ioq.bin.size);
    case VEC_TAG_SLICE:
        return (size_t)(vec->data.slice.tail - vec->data.slice.head);
    default:
        return 0;
    }
}

inline int
vec_move(vec_t *dst_vec, vec_t *src_vec)
{
    if (!vec_is_free(dst_vec) || vec_is_free(src_vec)) {
        return 0;
    }
    *dst_vec = *src_vec;
    return 1;
}

inline size_t
vec_remaining_writable_bytes(const vec_t *vec)
{
    if (vec == NULL) {
        return 0;
    }
    switch (vec->tag) {
    case VEC_TAG_NULL:
        return (SIZE_MAX - vec->data.null.len);
    case VEC_TAG_OWN_BIN:
        return (vec->data.own_bin.bin.size - vec->data.own_bin.len);
    case VEC_TAG_OWN_MEM:
        return (vec->data.own_mem.capacity - vec->data.own_mem.len);
    default:
        return 0;
    }
}

inline int
vec_write_from_vec(vec_t *dst_vec, vec_t *src_vec, size_t skip)
{
    int retval = vec_write_from_vec_copy(dst_vec, src_vec, skip);
    if (retval) {
        (void)vec_destroy(src_vec);
    }
    return retval;
}

#include "vec_reader.h"
#include "vec_writer.h"

#ifdef __cplusplus
}
#endif

#endif
