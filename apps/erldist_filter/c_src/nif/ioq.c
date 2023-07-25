/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "ioq.h"

#include "core/xnif_trace.h"

void
ioq_destroy(ioq_t *ioq)
{
    if (ioq->nif_ioq != NULL) {
        (void)enif_ioq_destroy(ioq->nif_ioq);
        ioq->nif_ioq = NULL;
    }
}

int
ioq_compact(ErlNifEnv *env, ioq_t *src_ioq)
{
    int iovcnt = 0;
    SysIOVec *sys_iov = NULL;

    sys_iov = ioq_peek_raw_head_sys_iov(src_ioq, &iovcnt);
    if (sys_iov != NULL && iovcnt > 1) {
        return ioq_into_ioq_compact(env, src_ioq, src_ioq);
    }
    return 1;
}

int
ioq_deq_into_bytes(ioq_t *ioq, uint8_t *bytes, size_t count, size_t *new_size)
{
    size_t bytes_copied = 0;
    size_t head_size = 0;
    ioq_iovec_t head_iovec;

    if (ioq_size(ioq) < count) {
        return 0;
    }

    while (bytes_copied < count) {
        if (!ioq_peek(ioq, &head_iovec)) {
            return 0;
        }
        if (head_iovec.iovcnt <= 0 || head_iovec.size == 0) {
            return 0;
        }
        head_size = head_iovec.iov[0].iov_len;
        if (count < (bytes_copied + head_size)) {
            head_size = count - bytes_copied;
        }
        (void)memcpy(&bytes[bytes_copied], head_iovec.iov[0].iov_base, head_size);
        if (!ioq_deq(ioq, head_size, NULL)) {
            return 0;
        }
        bytes_copied += head_size;
        head_size = 0;
    }

    if (new_size != NULL) {
        *new_size = ioq_size(ioq);
    }

    return 1;
}

int
ioq_inspect_iovec_and_enqv(ErlNifEnv *env, size_t max_elements, ERL_NIF_TERM iovec_term, ERL_NIF_TERM *tail, ioq_t *ioq)
{
    int retval;
    ErlNifIOVec *iovec = NULL;

    retval = enif_inspect_iovec(env, max_elements, iovec_term, tail, &iovec);
    if (!retval) {
        return retval;
    }

    retval = enif_ioq_enqv(ioq->nif_ioq, iovec, 0);
    if (!retval) {
        return retval;
    }

    return 1;
}

int
ioq_into_ioq(ErlNifEnv *env, ioq_t *src_ioq, ioq_t *dst_ioq)
{
    ioq_reader_t src_reader;

    if (!ioq_reader_create(&src_reader, src_ioq)) {
        return 0;
    }
    if (!ioq_reader_skip_remaining(&src_reader)) {
        (void)ioq_reader_destroy(&src_reader);
        return 0;
    }
    if (!ioq_reader_split_into_ioq(env, &src_reader, dst_ioq)) {
        (void)ioq_reader_destroy(&src_reader);
        return 0;
    }
    (void)ioq_reader_destroy(&src_reader);
    return 1;
}

int
ioq_into_ioq_compact(ErlNifEnv *env, ioq_t *src_ioq, ioq_t *dst_ioq)
{
    ioq_reader_t src_reader;

    if (!ioq_reader_create(&src_reader, src_ioq)) {
        return 0;
    }
    if (!ioq_reader_skip_remaining(&src_reader)) {
        (void)ioq_reader_destroy(&src_reader);
        return 0;
    }
    if (!ioq_reader_split_into_ioq_compact(env, &src_reader, dst_ioq)) {
        (void)ioq_reader_destroy(&src_reader);
        return 0;
    }
    (void)ioq_reader_destroy(&src_reader);
    return 1;
}

int
ioq_into_iovec(ErlNifEnv *env, ioq_t *src_ioq, ERL_NIF_TERM *iovec_term)
{
    ioq_reader_t src_reader;

    if (!ioq_reader_create(&src_reader, src_ioq)) {
        XNIF_TRACE_F("failed to create reader?\n");
        return 0;
    }
    if (!ioq_reader_skip_remaining(&src_reader)) {
        XNIF_TRACE_F("failed to remaining?\n");
        (void)ioq_reader_destroy(&src_reader);
        return 0;
    }
    if (!ioq_reader_split_into_iovec(env, &src_reader, iovec_term)) {
        XNIF_TRACE_F("failed to split into iovec?\n");
        (void)ioq_reader_destroy(&src_reader);
        return 0;
    }
    (void)ioq_reader_destroy(&src_reader);
    return 1;
}

int
ioq_into_iovec_and_ioq(ErlNifEnv *env, ioq_t *src_ioq, ERL_NIF_TERM *iovec_term, ioq_t *dst_ioq)
{
    ioq_reader_t src_reader;

    if (!ioq_reader_create(&src_reader, src_ioq)) {
        return 0;
    }
    if (!ioq_reader_skip_remaining(&src_reader)) {
        (void)ioq_reader_destroy(&src_reader);
        return 0;
    }
    if (!ioq_reader_split_into_iovec_and_ioq(env, &src_reader, iovec_term, dst_ioq)) {
        (void)ioq_reader_destroy(&src_reader);
        return 0;
    }
    (void)ioq_reader_destroy(&src_reader);
    return 1;
}

int
ioq_into_new_binary(ErlNifEnv *env, ioq_t *src_ioq, ErlNifBinary *dst_bin)
{
    ioq_reader_t src_reader;

    if (!ioq_reader_create(&src_reader, src_ioq)) {
        return 0;
    }
    if (!ioq_reader_skip_remaining(&src_reader)) {
        (void)ioq_reader_destroy(&src_reader);
        return 0;
    }
    if (!ioq_reader_split_into_new_binary(env, &src_reader, dst_bin)) {
        (void)ioq_reader_destroy(&src_reader);
        return 0;
    }
    (void)ioq_reader_destroy(&src_reader);
    return 1;
}

void
ioq_reader_destroy(ioq_reader_t *r)
{
    r->iovec.iovcnt = 0;
    r->iovec.size = 0;
    r->iovec.iov = NULL;
    r->ioq = NULL;
    r->iovcnt_offset = 0;
    r->iov_base_offset = 0;
    r->byte_offset = 0;
}

int
ioq_reader_back_exact(ioq_reader_t *r, size_t bytes_length)
{
    int orig_iovcnt_offset = r->iovcnt_offset;
    int orig_iov_base_offset = r->iov_base_offset;
    int orig_byte_offset = r->byte_offset;

    if (r->byte_offset == 0 || r->iovec.iov == NULL) {
        return 0;
    }

    if (r->byte_offset < bytes_length) {
        return 0;
    }

    if (bytes_length == 0) {
        return 1;
    }

    if (r->byte_offset == bytes_length) {
        r->iovcnt_offset = 0;
        r->iov_base_offset = 0;
        r->byte_offset = 0;
        bytes_length = 0;
        return 1;
    }

    while (bytes_length > 0) {
        if (bytes_length <= r->iov_base_offset) {
            r->iov_base_offset -= bytes_length;
            r->byte_offset -= bytes_length;
            bytes_length = 0;
            return 1;
        } else if (r->iov_base_offset == 0 && r->iovcnt_offset > 0) {
            r->iovcnt_offset -= 1;
            r->iov_base_offset = r->iovec.iov[r->iovcnt_offset].iov_len;
            continue;
        } else if (r->iov_base_offset > 0 && r->iov_base_offset < bytes_length) {
            bytes_length -= r->iov_base_offset;
            r->byte_offset -= r->iov_base_offset;
            r->iov_base_offset = 0;
            continue;
        } else {
            break;
        }
    }

    r->iovcnt_offset = orig_iovcnt_offset;
    r->iov_base_offset = orig_iov_base_offset;
    r->byte_offset = orig_byte_offset;
    return 0;
}

int
ioq_reader_read_exact(ioq_reader_t *r, uint8_t *bytes, size_t bytes_length)
{
    size_t bytes_offset = 0;
    size_t next_read_length = 0;

    if (r->byte_offset == r->iovec.size || r->iovec.iov == NULL) {
        XNIF_TRACE_F("byte_offset (%llu) == size (%llu)\n", r->byte_offset, r->iovec.size);
        return 0;
    }

    if (ioq_reader_remaining_bytes(r) < bytes_length) {
        XNIF_TRACE_F("remaining bytes (%llu) < bytes_length (%llu)\n", ioq_reader_remaining_bytes(r), bytes_length);
        return 0;
    }

    if (bytes_length == 0) {
        XNIF_TRACE_F("bytes_length == 0 (%llu)\n", bytes_length);
        return 1;
    }

    while (bytes_offset < bytes_length && r->byte_offset < r->iovec.size && r->iovcnt_offset < r->iovec.iovcnt) {
        while (bytes_offset < bytes_length && r->byte_offset < r->iovec.size &&
               r->iov_base_offset < r->iovec.iov[r->iovcnt_offset].iov_len) {
            next_read_length = r->iovec.iov[r->iovcnt_offset].iov_len - r->iov_base_offset;
            if ((bytes_length - bytes_offset) < next_read_length) {
                next_read_length = bytes_length - bytes_offset;
            }
            if (bytes != NULL) {
                (void)memcpy(&bytes[bytes_offset], &r->iovec.iov[r->iovcnt_offset].iov_base[r->iov_base_offset], next_read_length);
            }
            for (size_t i = 0; i < next_read_length; i++) {
                XNIF_TRACE_F("[% 3u] % 3u\n", r->byte_offset + i,
                             *(((uint8_t *)(&r->iovec.iov[r->iovcnt_offset].iov_base[r->iov_base_offset])) + i));
            }
            r->iov_base_offset += next_read_length;
            r->byte_offset += next_read_length;
            bytes_offset += next_read_length;
            next_read_length = 0;
        }
        if (bytes_offset == bytes_length) {
            return 1;
        }
        r->iovcnt_offset += 1;
        r->iov_base_offset = 0;
    }

    XNIF_TRACE_F("read error\n\tremaining_bytes = %llu\n\tr->iovec.iovcnt = %d\n\tr->iovec.size = "
                 "%llu\n\tr->iovcnt_offset = %d\n\tr->iov_base_offset = %llu\n\tr->byte_offset = %llu\n",
                 ioq_reader_remaining_bytes(r), r->iovec.iovcnt, r->iovec.size, r->iovcnt_offset, r->iov_base_offset,
                 r->byte_offset);

    return 0;
}

int
ioq_reader_split_into_ioq(ErlNifEnv *env, ioq_reader_t *r, ioq_t *dst_ioq)
{
    size_t bytes_split = 0;
    size_t head_size = 0;
    ERL_NIF_TERM head_term;
    ErlNifBinary head_bin;
    size_t reader_remaining_bytes = ioq_reader_remaining_bytes(r);

    if (r->byte_offset == 0) {
        return 1;
    }

    while (bytes_split < r->byte_offset) {
        if (!enif_ioq_peek_head(env, r->ioq->nif_ioq, &head_size, &head_term)) {
            return 0;
        }
        if (r->byte_offset < (bytes_split + head_size)) {
            head_size = r->byte_offset - bytes_split;
            head_term = enif_make_sub_binary(env, head_term, 0, head_size);
        }
        if (!ioq_deq(r->ioq, head_size, NULL)) {
            return 0;
        }
        if (!enif_inspect_binary(env, head_term, &head_bin)) {
            return 0;
        }
        if (!enif_ioq_enq_binary(dst_ioq->nif_ioq, &head_bin, 0)) {
            return 0;
        }
        bytes_split += head_size;
        head_size = 0;
    }

    if (!ioq_reader_create(r, r->ioq)) {
        return 0;
    }
    if (reader_remaining_bytes < r->iovec.size) {
        r->iovec.size = reader_remaining_bytes;
    }
    return 1;
}

int
ioq_reader_split_into_ioq_compact(ErlNifEnv *env, ioq_reader_t *r, ioq_t *dst_ioq)
{
    size_t bytes_split = 0;
    size_t head_size = 0;
    ERL_NIF_TERM head_term;
    ErlNifBinary head_bin;
    ioq_iovec_t head_iovec;
    size_t reader_remaining_bytes = ioq_reader_remaining_bytes(r);

    if (r->byte_offset == 0) {
        return 1;
    }

    // Check whether we can just use the head binary as-is (or a sub-binary).
    if (!enif_ioq_peek_head(env, r->ioq->nif_ioq, &head_size, &head_term)) {
        return 0;
    }
    if (r->byte_offset <= (bytes_split + head_size)) {
        if (r->byte_offset < (bytes_split + head_size)) {
            head_size = r->byte_offset - bytes_split;
            head_term = enif_make_sub_binary(env, head_term, 0, head_size);
        }
        if (!ioq_deq(r->ioq, head_size, NULL)) {
            return 0;
        }
        if (!enif_inspect_binary(env, head_term, &head_bin)) {
            return 0;
        }
        if (!enif_ioq_enq_binary(dst_ioq->nif_ioq, &head_bin, 0)) {
            return 0;
        }
        bytes_split += head_size;
        head_size = 0;
    } else {
        // Perform a full copy instead :-(
        head_bin.data = NULL;
        head_bin.size = 0;
        if (!enif_alloc_binary(r->byte_offset, &head_bin)) {
            return 0;
        }
        if (r->byte_offset != head_bin.size) {
            XNIF_TRACE_F("r->byte_offset (%llu) and head_bin.size (%llu) don't match\n", r->byte_offset, head_bin.size);
            (void)enif_release_binary(&head_bin);
            return 0;
        }
        while (bytes_split < r->byte_offset) {
            if (!ioq_peek(r->ioq, &head_iovec)) {
                (void)enif_release_binary(&head_bin);
                return 0;
            }
            if (head_iovec.iovcnt <= 0 || head_iovec.size == 0) {
                (void)enif_release_binary(&head_bin);
                return 0;
            }
            head_size = head_iovec.iov[0].iov_len;
            if (r->byte_offset < (bytes_split + head_size)) {
                head_size = r->byte_offset - bytes_split;
            }
            (void)memcpy(&head_bin.data[bytes_split], head_iovec.iov[0].iov_base, head_size);
            if (!ioq_deq(r->ioq, head_size, NULL)) {
                (void)enif_release_binary(&head_bin);
                return 0;
            }
            bytes_split += head_size;
            head_size = 0;
        }
        if (!enif_ioq_enq_binary(dst_ioq->nif_ioq, &head_bin, 0)) {
            (void)enif_release_binary(&head_bin);
            return 0;
        }
    }

    if (!ioq_reader_create(r, r->ioq)) {
        return 0;
    }
    if (reader_remaining_bytes < r->iovec.size) {
        r->iovec.size = reader_remaining_bytes;
    }
    return 1;
}

int
ioq_reader_split_into_iovec(ErlNifEnv *env, ioq_reader_t *r, ERL_NIF_TERM *iovec_term)
{
    size_t bytes_split = 0;
    size_t head_size = 0;
    ERL_NIF_TERM *iovec_terms = NULL;
    size_t iovec_terms_capacity = r->iovcnt_offset + 1;
    size_t iovec_terms_offset = 0;
    size_t reader_remaining_bytes = ioq_reader_remaining_bytes(r);

    if (r->byte_offset == 0) {
        *iovec_term = enif_make_list(env, 0);
        return 1;
    }

    iovec_terms = (void *)enif_alloc(sizeof(ERL_NIF_TERM) * iovec_terms_capacity);
    if (iovec_terms == NULL) {
        XNIF_TRACE_F("failed to allocate iovec_terms\n");
        return 0;
    }

    while (bytes_split < r->byte_offset) {
        if (iovec_terms_capacity < iovec_terms_offset) {
            ERL_NIF_TERM *new_iovec_terms = NULL;
            iovec_terms_capacity *= 2;
            new_iovec_terms = (void *)enif_realloc((void *)iovec_terms, sizeof(ERL_NIF_TERM) * iovec_terms_capacity);
            if (new_iovec_terms == NULL) {
                XNIF_TRACE_F("failed to allocate new_iovec_terms\n");
                (void)enif_free((void *)iovec_terms);
                return 0;
            }
            iovec_terms = new_iovec_terms;
        }
        if (!enif_ioq_peek_head(env, r->ioq->nif_ioq, &head_size, &iovec_terms[iovec_terms_offset])) {
            XNIF_TRACE_F("failed to peek at head\n");
            (void)enif_free((void *)iovec_terms);
            return 0;
        }
        if (r->byte_offset < (bytes_split + head_size)) {
            head_size = r->byte_offset - bytes_split;
            iovec_terms[iovec_terms_offset] = enif_make_sub_binary(env, iovec_terms[iovec_terms_offset], 0, head_size);
        }
        if (!ioq_deq(r->ioq, head_size, NULL)) {
            XNIF_TRACE_F("failed to ioq_deq\n");
            (void)enif_free((void *)iovec_terms);
            return 0;
        }
        bytes_split += head_size;
        head_size = 0;
        iovec_terms_offset += 1;
    }

    *iovec_term = enif_make_list_from_array(env, iovec_terms, iovec_terms_offset);
    (void)enif_free((void *)iovec_terms);
    iovec_terms = NULL;

    if (!ioq_reader_create(r, r->ioq)) {
        XNIF_TRACE_F("failed to recreate reader\n");
        return 0;
    }
    if (reader_remaining_bytes < r->iovec.size) {
        r->iovec.size = reader_remaining_bytes;
    }
    return 1;
}

int
ioq_reader_split_into_iovec_and_ioq(ErlNifEnv *env, ioq_reader_t *r, ERL_NIF_TERM *iovec_term, ioq_t *dst_ioq)
{
    ERL_NIF_TERM tail_term;

    if (!ioq_reader_split_into_iovec(env, r, iovec_term)) {
        return 0;
    }
    if (!ioq_inspect_iovec_and_enqv(env, ~(0ULL), *iovec_term, &tail_term, dst_ioq)) {
        return 0;
    }
    if (!enif_is_empty_list(env, tail_term)) {
        return 0;
    }
    return 1;
}

int
ioq_reader_split_into_new_binary(ErlNifEnv *env, ioq_reader_t *r, ErlNifBinary *dst_bin)
{
    ErlNifBinary bin;
    ioq_reader_t tmp_reader;
    size_t reader_remaining_bytes = ioq_reader_remaining_bytes(r);

    if (!ioq_reader_create(&tmp_reader, r->ioq)) {
        return 0;
    }
    if (!enif_alloc_binary(r->byte_offset, &bin)) {
        (void)ioq_reader_destroy(&tmp_reader);
        return 0;
    }
    if (!ioq_reader_read_exact(&tmp_reader, (void *)bin.data, bin.size)) {
        (void)enif_release_binary(&bin);
        (void)ioq_reader_destroy(&tmp_reader);
        return 0;
    }
    if (!ioq_reader_consume(&tmp_reader)) {
        (void)enif_release_binary(&bin);
        (void)ioq_reader_destroy(&tmp_reader);
        return 0;
    }
    if (!ioq_reader_create(r, r->ioq)) {
        (void)enif_release_binary(&bin);
        (void)ioq_reader_destroy(&tmp_reader);
        return 0;
    }
    if (reader_remaining_bytes < r->iovec.size) {
        r->iovec.size = reader_remaining_bytes;
    }
    *dst_bin = bin;

    return 1;
}
