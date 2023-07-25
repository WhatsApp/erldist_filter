/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "vec.h"

static void vec_data_null_destroy(vec_data_null_t *data);
static void vec_data_owned_destroy(vec_data_owned_t *data);
static void vec_data_ref_bin_destroy(vec_data_ref_bin_t *data);
static void vec_data_ref_ioq_destroy(vec_data_ref_ioq_t *data);
static void vec_data_slice_destroy(vec_data_slice_t *data);
static int vec_create_ref_bin(vec_t *vec, ERL_NIF_TERM bin_term);
static int vec_create_ref_ioq(vec_t *vec, ioq_t *ioq, size_t len);

void
vec_destroy(vec_t *vec)
{
    XNIF_TRACE_F("%s:%d vec_destroy()\n", __FILE__, __LINE__);
    switch (vec->tag) {
    case VEC_TAG_NULL:
        vec->tag = VEC_TAG_FREE;
        (void)vec_data_null_destroy(&vec->data.null);
        break;
    case VEC_TAG_OWNED:
        vec->tag = VEC_TAG_FREE;
        (void)vec_data_owned_destroy(&vec->data.owned);
        break;
    case VEC_TAG_REF_BIN:
        vec->tag = VEC_TAG_FREE;
        (void)vec_data_ref_bin_destroy(&vec->data.ref_bin);
        break;
    case VEC_TAG_REF_IOQ:
        vec->tag = VEC_TAG_FREE;
        (void)vec_data_ref_ioq_destroy(&vec->data.ref_ioq);
        break;
    case VEC_TAG_SLICE:
        vec->tag = VEC_TAG_FREE;
        (void)vec_data_slice_destroy(&vec->data.slice);
        break;
    default:
        break;
    }
}

inline void
vec_data_null_destroy(vec_data_null_t *data)
{
    XNIF_TRACE_F("%s:%d vec_data_null_destroy()\n", __FILE__, __LINE__);
    data->len = 0;
}

inline void
vec_data_owned_destroy(vec_data_owned_t *data)
{
    XNIF_TRACE_F("%s:%d vec_data_owned_destroy()\n", __FILE__, __LINE__);
    (void)enif_release_binary(&data->bin);
    data->bin.data = NULL;
    data->bin.size = 0;
    data->len = 0;
}

inline void
vec_data_ref_bin_destroy(vec_data_ref_bin_t *data)
{
    XNIF_TRACE_F("%s:%d vec_data_ref_bin_destroy()\n", __FILE__, __LINE__);
    (void)enif_free_env(data->env);
    data->env = NULL;
    data->bin_term = (ERL_NIF_TERM)0;
    data->bin.data = NULL;
    data->bin.size = 0;
}

inline void
vec_data_ref_ioq_destroy(vec_data_ref_ioq_t *data)
{
    XNIF_TRACE_F("%s:%d vec_data_ref_ioq_destroy()\n", __FILE__, __LINE__);
    (void)ioq_deq(data->ioq, data->bin.size, NULL);
    (void)enif_free_env(data->env);
    data->env = NULL;
    data->ioq = NULL;
    data->bin.data = NULL;
    data->bin.size = 0;
}

inline void
vec_data_slice_destroy(vec_data_slice_t *data)
{
    XNIF_TRACE_F("%s:%d vec_data_slice_destroy()\n", __FILE__, __LINE__);
    data->head = NULL;
    data->tail = NULL;
}

int
vec_create_owned(vec_t *vec, size_t capacity)
{
    vec_data_owned_t *owned = &vec->data.owned;

    XNIF_TRACE_F("%s:%d vec_create_owned()\n", __FILE__, __LINE__);

    if (!vec_is_free(vec)) {
        XNIF_TRACE_F("Call to vec_create_owned() failed: vec is not free (%d)\n", vec->tag);
        return 0;
    }
    if (!enif_alloc_binary(capacity, &owned->bin)) {
        XNIF_TRACE_F("Call to enif_alloc_binary() failed\n");
        return 0;
    }
    if (owned->bin.size != capacity) {
        XNIF_TRACE_F("Call to enif_alloc_binary() failed: owned->bin.size (%llu) != capacity (%llu)\n", owned->bin.size, capacity);
        (void)enif_release_binary(&owned->bin);
        return 0;
    }
    vec->tag = VEC_TAG_OWNED;
    owned->len = 0;
    return 1;
}

int
vec_realloc_owned(vec_t *vec, size_t new_capacity)
{
    XNIF_TRACE_F("%s:%d vec_realloc_owned(%u -> %u)\n", __FILE__, __LINE__, vec_capacity(vec), new_capacity);
    vec_data_owned_t *owned = &vec->data.owned;

    if (!vec_is_owned(vec)) {
        return 0;
    }

    if (!enif_realloc_binary(&owned->bin, new_capacity)) {
        return 0;
    }

    if (new_capacity < owned->len) {
        owned->len = new_capacity;
    }

    return 1;
}

int
vec_create_ref_bin(vec_t *vec, ERL_NIF_TERM bin_term)
{
    vec_data_ref_bin_t *ref = &vec->data.ref_bin;

    if (!vec_is_free(vec)) {
        return 0;
    }
    vec->tag = VEC_TAG_REF_BIN;
    ref->env = enif_alloc_env();
    if (ref->env == NULL) {
        vec->tag = VEC_TAG_FREE;
        return 0;
    }
    ref->bin_term = enif_make_copy(ref->env, bin_term);
    if (!enif_inspect_binary(ref->env, ref->bin_term, &ref->bin)) {
        (void)enif_free_env(ref->env);
        ref->env = NULL;
        ref->bin_term = (ERL_NIF_TERM)0;
        vec->tag = VEC_TAG_FREE;
        return 0;
    }
    return 1;
}

int
vec_create_ref_ioq(vec_t *vec, ioq_t *ioq, size_t len)
{
    vec_data_ref_ioq_t *ref = &vec->data.ref_ioq;
    ERL_NIF_TERM bin_term;
    size_t bin_size;

    if (!vec_is_free(vec)) {
        return 0;
    }
    vec->tag = VEC_TAG_REF_IOQ;
    ref->env = enif_alloc_env();
    if (ref->env == NULL) {
        vec->tag = VEC_TAG_FREE;
        return 0;
    }
    ref->ioq = ioq;
    if (!ioq_peek_raw_head_binary_term(ref->env, ref->ioq, &bin_size, &bin_term)) {
        (void)enif_free_env(ref->env);
        ref->env = NULL;
        ref->ioq = NULL;
        vec->tag = VEC_TAG_FREE;
        return 0;
    }
    if (bin_size < len) {
        (void)enif_free_env(ref->env);
        ref->env = NULL;
        ref->ioq = NULL;
        vec->tag = VEC_TAG_FREE;
        return 0;
    }
    if (len < bin_size) {
        bin_size = len;
        bin_term = enif_make_sub_binary(ref->env, bin_term, 0, bin_size);
    }
    if (!enif_inspect_binary(ref->env, bin_term, &ref->bin) || ref->bin.size != len) {
        (void)enif_free_env(ref->env);
        ref->env = NULL;
        ref->ioq = NULL;
        vec->tag = VEC_TAG_FREE;
        return 0;
    }
    return 1;
}

int
vec_create_from_bin(vec_t *dst_vec, ERL_NIF_TERM src_bin_term)
{
    XNIF_TRACE_F("%s:%d vec_create_from_bin()\n", __FILE__, __LINE__);

    return vec_create_ref_bin(dst_vec, src_bin_term);
}

int
vec_create_from_ioq(vec_t *dst_vec, size_t dst_size, ioq_t *src_ioq)
{
    XNIF_TRACE_F("%s:%d vec_create_from_ioq()\n", __FILE__, __LINE__);

    if (dst_size <= ioq_peek_raw_head_size(src_ioq)) {
        if (!vec_create_ref_ioq(dst_vec, src_ioq, dst_size)) {
            XNIF_TRACE_F("Call to vec_create_ref_ioq() failed: dst_size (%llu) <= ioq_peek_raw_head_size(src_ioq) (%llu)\n",
                         dst_size, ioq_peek_raw_head_size(src_ioq));
            return 0;
        }
        return 1;
    }
    if (!vec_create_owned(dst_vec, dst_size)) {
        XNIF_TRACE_F("Call to vec_create_owned() failed: dst_size (%llu)\n", dst_size);
        return 0;
    }
    if (ioq_size(src_ioq) < dst_size) {
        dst_size = ioq_size(src_ioq);
    }
    if (!ioq_deq_into_bytes(src_ioq, (uint8_t *)(dst_vec->data.owned.bin.data), dst_size, NULL)) {
        XNIF_TRACE_F("Call to ioq_deq_into_bytes() failed: dst_size (%llu)\n", dst_size);
        (void)vec_destroy(dst_vec);
        return 0;
    }
    dst_vec->data.owned.len = dst_size;
    return 1;
}

int
vec_create_from_slice(vec_t *dst_vec, const uint8_t *head, const uint8_t *tail)
{
    vec_data_slice_t *slice = &dst_vec->data.slice;

    XNIF_TRACE_F("%s:%d vec_create_from_slice()\n", __FILE__, __LINE__);

    if (!vec_is_free(dst_vec) || head == NULL || tail == NULL || tail < head) {
        return 0;
    }

    slice->head = head;
    slice->tail = tail;
    dst_vec->tag = VEC_TAG_SLICE;
    return 1;
}

int
vec_clone_sub_binary(vec_t *dst_vec, const vec_t *src_vec, size_t pos, size_t len)
{
    ERL_NIF_TERM dst_bin_term = (ERL_NIF_TERM)0;
    vec_data_ref_bin_t *dst_ref = NULL;
    const vec_data_ref_bin_t *src_ref = NULL;
    if (!vec_is_free(dst_vec) || !vec_is_ref_bin(src_vec)) {
        return 0;
    }
    dst_ref = &dst_vec->data.ref_bin;
    src_ref = &src_vec->data.ref_bin;
    if (pos > src_ref->bin.size || len > (src_ref->bin.size - pos)) {
        return 0;
    }
    dst_vec->tag = VEC_TAG_REF_BIN;
    dst_ref->env = enif_alloc_env();
    if (dst_ref->env == NULL) {
        dst_vec->tag = VEC_TAG_FREE;
        return 0;
    }
    dst_bin_term = enif_make_sub_binary(dst_ref->env, src_ref->bin_term, pos, len);
    dst_bin_term = enif_make_copy(dst_ref->env, dst_bin_term);
    dst_ref->bin_term = dst_bin_term;
    if (!enif_inspect_binary(dst_ref->env, dst_ref->bin_term, &dst_ref->bin)) {
        (void)enif_free_env(dst_ref->env);
        dst_ref->env = NULL;
        dst_ref->bin_term = (ERL_NIF_TERM)0;
        dst_vec->tag = VEC_TAG_FREE;
        return 0;
    }
    return 1;
}

ERL_NIF_TERM
vec_into_binary_term(ErlNifEnv *env, vec_t *vec)
{
    ERL_NIF_TERM bin_term = (ERL_NIF_TERM)0;

    if (vec->tag == VEC_TAG_OWNED) {
        vec_data_owned_t *data = &vec->data.owned;
        bin_term = enif_make_binary(env, &data->bin);
        vec->tag = VEC_TAG_FREE;
        data->bin.data = NULL;
        data->bin.size = 0;
        data->len = 0;
        return bin_term;
    } else if (vec->tag == VEC_TAG_REF_BIN) {
        vec_data_ref_bin_t *data = &vec->data.ref_bin;
        bin_term = enif_make_binary(env, &data->bin);
        (void)enif_free_env(data->env);
        vec->tag = VEC_TAG_FREE;
        data->env = NULL;
        data->bin_term = (ERL_NIF_TERM)0;
        data->bin.data = NULL;
        data->bin.size = 0;
        return bin_term;
    } else if (vec->tag == VEC_TAG_REF_IOQ) {
        vec_data_ref_ioq_t *data = &vec->data.ref_ioq;
        bin_term = enif_make_binary(env, &data->bin);
        (void)ioq_deq(data->ioq, data->bin.size, NULL);
        (void)enif_free_env(data->env);
        vec->tag = VEC_TAG_FREE;
        data->env = NULL;
        data->ioq = NULL;
        data->bin.data = NULL;
        data->bin.size = 0;
        return bin_term;
    } else if (vec->tag == VEC_TAG_SLICE) {
        vec_data_slice_t *data = &vec->data.slice;
        size_t size = (size_t)(data->tail - data->head);
        unsigned char *buf = enif_make_new_binary(env, size, &bin_term);
        (void)memcpy(buf, data->head, size);
        return bin_term;
    } else {
        (void)enif_make_new_binary(env, 0, &bin_term);
        return bin_term;
    }
}

int
vec_into_owned(vec_t *dst_vec, vec_t *src_vec)
{
    if (!vec_is_free(dst_vec)) {
        return 0;
    }
    if (src_vec->tag == VEC_TAG_OWNED) {
        vec_data_owned_t *dst_data = &dst_vec->data.owned;
        vec_data_owned_t *src_data = &src_vec->data.owned;
        dst_vec->tag = VEC_TAG_OWNED;
        dst_data->bin = src_data->bin;
        dst_data->len = src_data->len;
        src_vec->tag = VEC_TAG_FREE;
        src_data->bin.data = NULL;
        src_data->bin.size = 0;
        src_data->len = 0;
        return 1;
    } else if (src_vec->tag == VEC_TAG_REF_BIN) {
        vec_data_owned_t *dst_data = &dst_vec->data.owned;
        vec_data_ref_bin_t *src_data = &src_vec->data.ref_bin;
        if (!vec_create_owned(dst_vec, src_data->bin.size)) {
            return 0;
        }
        (void)memcpy(dst_data->bin.data, src_data->bin.data, src_data->bin.size);
        dst_data->len = src_data->bin.size;
        (void)vec_destroy(src_vec);
        return 1;
    } else if (src_vec->tag == VEC_TAG_REF_IOQ) {
        vec_data_owned_t *dst_data = &dst_vec->data.owned;
        vec_data_ref_ioq_t *src_data = &src_vec->data.ref_ioq;
        if (!vec_create_owned(dst_vec, src_data->bin.size)) {
            return 0;
        }
        (void)memcpy(dst_data->bin.data, src_data->bin.data, src_data->bin.size);
        dst_data->len = src_data->bin.size;
        (void)vec_destroy(src_vec);
        return 1;
    } else if (src_vec->tag == VEC_TAG_SLICE) {
        vec_data_owned_t *dst_data = &dst_vec->data.owned;
        vec_data_slice_t *src_data = &src_vec->data.slice;
        size_t src_size = (size_t)(src_data->tail - src_data->head);
        if (!vec_create_owned(dst_vec, src_size)) {
            return 0;
        }
        (void)memcpy(dst_data->bin.data, src_data->head, src_size);
        (void)vec_destroy(src_vec);
        return 1;
    } else {
        return 0;
    }
}

int
vec_write_from_ioq(vec_t *dst_vec, ioq_t *src_ioq)
{
    size_t writable_bytes = vec_remaining_writable_bytes(dst_vec);
    size_t readable_bytes = ioq_size(src_ioq);
    vec_data_owned_t *owned = &dst_vec->data.owned;
    uint8_t *bytes = NULL;

    if (!vec_is_owned(dst_vec)) {
        return 0;
    }

    if (writable_bytes == 0) {
        return 0;
    }
    if (readable_bytes == 0) {
        return 1;
    }
    if (readable_bytes < writable_bytes) {
        writable_bytes = readable_bytes;
    }
    bytes = (uint8_t *)(owned->bin.data);
    if (!ioq_deq_into_bytes(src_ioq, &bytes[owned->len], writable_bytes, NULL)) {
        return 0;
    }
    owned->len += writable_bytes;
    return 1;
}

int
vec_write_from_vec_copy(vec_t *dst_vec, vec_t *src_vec, size_t skip)
{
    size_t writable_bytes = vec_remaining_writable_bytes(dst_vec);
    size_t readable_bytes = vec_len(src_vec);
    vec_data_owned_t *owned = &dst_vec->data.owned;
    uint8_t *dst_buf = NULL;
    uint8_t *src_buf = NULL;

    if (!vec_is_owned(dst_vec) || !vec_is_readable(src_vec)) {
        return 0;
    }

    if (writable_bytes == 0) {
        return 0;
    }
    if (skip > readable_bytes) {
        return 0;
    }
    readable_bytes -= skip;
    if (readable_bytes == 0) {
        return 1;
    }
    if (readable_bytes > writable_bytes) {
        return 0;
    }
    if (readable_bytes < writable_bytes) {
        writable_bytes = readable_bytes;
    }
    dst_buf = (uint8_t *)(owned->bin.data);
    src_buf = vec_buf(src_vec);
    (void)memcpy(&dst_buf[owned->len], &src_buf[skip], writable_bytes);
    owned->len += writable_bytes;
    return 1;
}
