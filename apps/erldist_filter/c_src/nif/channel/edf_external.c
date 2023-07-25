/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "edf_external.h"
#include "edf_external_sequence.h"
#include "edf_channel.h"
#include "../config/edf_config.h"
#include "../etf/etf_decode.h"
#include "../logger/edf_logger.h"

int
edf_external_create(edf_channel_t *channel, edf_external_mode_t mode, uint64_t sequence_id, uint64_t fragment_count, vec_t *src_vec,
                    size_t skip, edf_external_t **extp)
{
    edf_external_t *ext = NULL;
    edf_fragment_t *frag = NULL;
    int flags = EDF_EXTERNAL_FLAG_NONE;
    size_t fragment_capacity;
    size_t i;

    if (fragment_count < 1) {
        return 0;
    }
    if (mode != EDF_EXTERNAL_MODE_PASS_THROUGH && mode != EDF_EXTERNAL_MODE_NORMAL && mode != EDF_EXTERNAL_MODE_FRAGMENT) {
        return 0;
    }
    if ((mode == EDF_EXTERNAL_MODE_PASS_THROUGH || mode == EDF_EXTERNAL_MODE_NORMAL) && fragment_count != 1) {
        return 0;
    }

    fragment_capacity = (size_t)(fragment_count);

    ext = (void *)enif_alloc(sizeof(edf_external_t) + (fragment_capacity * sizeof(edf_fragment_t)));
    if (ext == NULL) {
        return 0;
    }

    (void)edf_external_sequence_init_empty(ext);
    ext->channel = channel;
    ext->emit = 0;
    ext->mode = mode;
    ext->flags = flags;
    (void)edf_atom_translation_table_init(&ext->attab);
    (void)edf_atom_translation_table_init(&ext->rollback);
    (void)vec_init_free(&ext->rollback_vec);
    (void)edf_external_slice_init_empty(&ext->slices.framing);
    (void)edf_external_slice_init_empty(&ext->slices.headers);
    (void)edf_external_slice_init_empty(&ext->slices.control);
    (void)edf_external_slice_init_empty(&ext->slices.payload);
    ext->slices.expected_payload_length = 0;
    ext->vtenv = NULL;
    (void)udist_init(ext->up);
    ext->control_heap_size = 0;
    ext->payload_heap_size = 0;
    ext->logging_to_action = false;
    ext->logger_event = NULL;
    ext->sequence_id = sequence_id;
    ext->fragment_id_next = fragment_count;
    ext->fragment_count = fragment_count;
    ext->fragments_compacted = false;
    ext->compact->sequence_id = ext->sequence_id;
    ext->compact->fragment_id = 1;
    (void)vec_init_free(&ext->compact->vec);
    ext->compact->skip = 0;
    ext->primary = NULL;
    ext->fragment_capacity = fragment_capacity;
    ext->fragments = (void *)(ext) + sizeof(edf_external_t);

    for (i = 0; i < ext->fragment_capacity; i++) {
        frag = &ext->fragments[i];
        frag->sequence_id = ext->sequence_id;
        frag->fragment_id = (uint64_t)(ext->fragment_capacity - i);
        (void)vec_init_free(&frag->vec);
        frag->skip = 0;
    }

    frag = &ext->fragments[0];
    ext->fragment_id_next -= 1;

    if (!vec_into_owned(&frag->vec, src_vec)) {
        (void)edf_external_destroy(ext);
        return 0;
    }
    frag->skip = skip;
    ext->primary = frag;
    ext->slices.framing.offset = 0;
    if (!edf_external_slice_framing_set(ext, vec_buf(&frag->vec), vec_buf(&frag->vec) + frag->skip)) {
        (void)edf_external_destroy(ext);
        return 0;
    }

    *extp = ext;

    return 1;
}

void
edf_external_destroy(edf_external_t *ext)
{
    XNIF_TRACE_F("%s:%d edf_external_destroy()\n", __FILE__, __LINE__);
    edf_fragment_t *frag = NULL;
    size_t i;

    if (ext == NULL) {
        return;
    }

    if (ext->channel != NULL && edf_external_sequence_is_linked(ext)) {
        (void)edf_external_sequence_unlink(&(ext->channel->rx.sequences), ext);
        (void)edf_external_sequence_init_empty(ext);
    }

    if (ext->logger_event != NULL) {
        (void)edf_logger_event_destroy(ext->logger_event);
        ext->logger_event = NULL;
    }

    if (ext->vtenv != NULL) {
        (void)vterm_env_free(ext->vtenv);
        ext->vtenv = NULL;
    }

    (void)edf_external_slice_destroy(&ext->slices.framing);
    (void)edf_external_slice_destroy(&ext->slices.headers);
    (void)edf_external_slice_destroy(&ext->slices.control);
    (void)edf_external_slice_destroy(&ext->slices.payload);

    if (!vec_is_free(&ext->compact->vec)) {
        (void)vec_destroy(&ext->compact->vec);
    }

    for (i = 0; i < ext->fragment_capacity; i++) {
        frag = &ext->fragments[i];
        if (!vec_is_free(&frag->vec)) {
            (void)vec_destroy(&frag->vec);
        }
    }

    (void)vec_destroy(&ext->rollback_vec);
    (void)edf_atom_translation_table_destroy(&ext->rollback);
    (void)edf_atom_translation_table_destroy(&ext->attab);

    (void)enif_free((void *)ext);

    return;
}

int
edf_external_add_fragment(edf_external_t *ext, uint64_t fragment_id, vec_t *src_vec, size_t skip)
{
    edf_fragment_t *frag = NULL;
    size_t frag_size;

    if (ext->mode != EDF_EXTERNAL_MODE_FRAGMENT) {
        return 0;
    }
    if (fragment_id < 1) {
        return 0;
    }
    if (fragment_id != ext->fragment_id_next) {
        return 0;
    }
    if (skip > vec_len(src_vec)) {
        return 0;
    }

    frag_size = vec_len(src_vec) - skip;

    frag = &ext->fragments[ext->fragment_count - ext->fragment_id_next];
    if (!vec_into_owned(&frag->vec, src_vec)) {
        return 0;
    }
    frag->skip = skip;

    ext->fragment_id_next -= 1;

    if (ext->up->info.dop != DOP_UNKNOWN && edf_external_has_payload(ext) && ext->mode == EDF_EXTERNAL_MODE_FRAGMENT &&
        ext->fragment_count > 1 && fragment_id != ext->fragment_count) {
        ext->slices.payload.length += frag_size;
        ext->slices.expected_payload_length += frag_size;
    }

    return 1;
}

int
edf_external_compact_start(edf_external_t *ext, size_t new_size)
{
    XNIF_TRACE_F("%s:%d edf_external_compact_start()\n", __FILE__, __LINE__);
    edf_fragment_t *dst_frag = NULL;
    edf_fragment_t *src_frag = NULL;
    size_t old_size;
    slice_t new_framing[1] = {SLICE_INIT_EMPTY()};
    slice_t new_headers[1] = {SLICE_INIT_EMPTY()};
    slice_t new_control[1] = {SLICE_INIT_EMPTY()};
    slice_t new_payload[1] = {SLICE_INIT_EMPTY()};

    if (ext == NULL || ext->fragment_capacity < 1) {
        return 0;
    }

    dst_frag = ext->compact;
    src_frag = &ext->fragments[0];
    old_size = vec_len(&src_frag->vec);
    if (new_size < old_size || ext->primary != src_frag) {
        return 0;
    }
    if (!vec_create_owned(&dst_frag->vec, new_size)) {
        return 0;
    }
    if (!vec_write_from_vec_copy(&dst_frag->vec, &src_frag->vec, 0)) {
        return 0;
    }
    new_framing->head = vec_buf(&dst_frag->vec) + ext->slices.framing.offset;
    new_framing->tail = new_framing->head + ext->slices.framing.length;
    new_headers->head = new_framing->tail;
    new_headers->tail = new_headers->head + ext->slices.headers.length;
    new_control->head = new_headers->tail;
    new_control->tail = new_control->head + ext->slices.control.length;
    if (edf_external_has_payload(ext)) {
        new_payload->head = new_control->tail;
        new_payload->tail = vec_buf_tail_unsafe(&dst_frag->vec);
    }
    dst_frag->sequence_id = src_frag->sequence_id;
    dst_frag->fragment_id = 1;
    dst_frag->skip = src_frag->skip;
    ext->primary = dst_frag;
    if (!edf_external_slice_framing_set(ext, new_framing->head, new_framing->tail)) {
        return 0;
    }
    if (!edf_external_slice_headers_set(ext, new_headers->head, new_headers->tail)) {
        return 0;
    }
    if (!edf_external_slice_control_set(ext, new_control->head, new_control->tail)) {
        return 0;
    }
    if (edf_external_has_payload(ext) && !edf_external_slice_payload_set(ext, new_payload->head, new_payload->tail)) {
        return 0;
    }
    return 1;
}

int
edf_external_slice_framing_get(edf_external_t *ext, vec_t *slice)
{
    if (ext == NULL || !vec_is_free(slice) || ext->fragment_capacity < 1) {
        return 0;
    }
    if (!vec_is_slice(&ext->slices.framing.vec)) {
        return vec_create_from_slice(slice, vec_buf(&ext->primary->vec), vec_buf(&ext->primary->vec) + ext->primary->skip);
    }
    return vec_clone_slice(slice, &ext->slices.framing.vec);
}

int
edf_external_slice_framing_set(edf_external_t *ext, const uint8_t *head, const uint8_t *tail)
{
    edf_fragment_t *frag = NULL;
    if (ext == NULL || head == NULL || tail == NULL || ext->fragment_capacity < 1) {
        return 0;
    }
    // Ensure that the head and tail pointers are within the primary fragment.
    frag = ext->primary;
    if (!vec_contains_slice(&frag->vec, head, tail)) {
        return 0;
    }
    if (vec_is_slice(&ext->slices.framing.vec)) {
        (void)vec_destroy(&ext->slices.framing.vec);
        ext->slices.framing.offset = 0;
        ext->slices.framing.length = 0;
    }
    if (!vec_create_from_slice(&ext->slices.framing.vec, head, tail)) {
        return 0;
    }
    ext->slices.framing.offset = vec_buf(&ext->slices.framing.vec) - vec_buf(&frag->vec);
    ext->slices.framing.length = vec_len(&ext->slices.framing.vec);
    return 1;
}

int
edf_external_slice_headers_get(edf_external_t *ext, vec_t *slice)
{
    if (ext == NULL || !vec_is_free(slice) || ext->fragment_capacity < 1) {
        return 0;
    }
    if (!vec_is_slice(&ext->slices.headers.vec)) {
        if (ext->mode == EDF_EXTERNAL_MODE_NORMAL || ext->mode == EDF_EXTERNAL_MODE_FRAGMENT) {
            return vec_create_from_slice(slice, vec_buf(&ext->primary->vec) + ext->primary->skip, vec_buf_tail(&ext->primary->vec));
        }
        return 0;
    }
    return vec_clone_slice(slice, &ext->slices.headers.vec);
}

int
edf_external_slice_headers_set(edf_external_t *ext, const uint8_t *head, const uint8_t *tail)
{
    edf_fragment_t *frag = NULL;
    if (ext == NULL || head == NULL || tail == NULL || ext->fragment_capacity < 1) {
        return 0;
    }
    // Ensure that the head and tail pointers are within the primary fragment.
    frag = ext->primary;
    if (!vec_contains_slice(&frag->vec, head, tail)) {
        return 0;
    }
    if (vec_is_slice(&ext->slices.headers.vec)) {
        (void)vec_destroy(&ext->slices.headers.vec);
        ext->slices.headers.offset = 0;
        ext->slices.headers.length = 0;
    }
    if (!vec_create_from_slice(&ext->slices.headers.vec, head, tail)) {
        return 0;
    }
    ext->slices.headers.offset = vec_buf(&ext->slices.headers.vec) - vec_buf(&frag->vec);
    ext->slices.headers.length = vec_len(&ext->slices.headers.vec);
    return 1;
}

int
edf_external_slice_control_get(edf_external_t *ext, bool *is_external_term, vec_t *slice)
{
    int retval;
    if (ext == NULL || !vec_is_free(slice) || ext->fragment_capacity < 1) {
        return 0;
    }
    if (!vec_is_slice(&ext->slices.control.vec)) {
        if (ext->mode == EDF_EXTERNAL_MODE_NORMAL || ext->mode == EDF_EXTERNAL_MODE_FRAGMENT) {
            if (!vec_is_slice(&ext->slices.headers.vec)) {
                return 0;
            }
            retval = vec_create_from_slice(slice, vec_buf_tail(&ext->slices.headers.vec), vec_buf_tail(&ext->primary->vec));
        } else {
            retval =
                vec_create_from_slice(slice, vec_buf(&ext->primary->vec) + ext->primary->skip, vec_buf_tail(&ext->primary->vec));
        }
    } else {
        retval = vec_clone_slice(slice, &ext->slices.control.vec);
    }
    if (retval != 0) {
        *is_external_term = edf_external_is_pass_through(ext);
    }
    return retval;
}

int
edf_external_slice_control_set(edf_external_t *ext, const uint8_t *head, const uint8_t *tail)
{
    edf_fragment_t *frag = NULL;
    if (ext == NULL || head == NULL || tail == NULL || ext->fragment_capacity < 1) {
        return 0;
    }
    // Ensure that the head and tail pointers are within the primary fragment.
    frag = ext->primary;
    if (!vec_contains_slice(&frag->vec, head, tail)) {
        return 0;
    }
    if (vec_is_slice(&ext->slices.control.vec)) {
        (void)vec_destroy(&ext->slices.control.vec);
        ext->slices.control.offset = 0;
        ext->slices.control.length = 0;
    }
    if (!vec_create_from_slice(&ext->slices.control.vec, head, tail)) {
        return 0;
    }
    ext->slices.control.offset = vec_buf(&ext->slices.control.vec) - vec_buf(&frag->vec);
    ext->slices.control.length = vec_len(&ext->slices.control.vec);
    if (edf_external_has_payload(ext)) {
        ext->slices.payload.offset = vec_buf_tail(&ext->slices.control.vec) - vec_buf(&frag->vec);
        ext->slices.payload.length = vec_buf_tail(&frag->vec) - vec_buf_tail(&ext->slices.control.vec);
        if (ext->slices.expected_payload_length == 0) {
            ext->slices.expected_payload_length = ext->slices.payload.length;
        }
    }
    return 1;
}

int
edf_external_slice_payload_get(edf_external_t *ext, bool *is_external_term, vec_t *slice)
{
    int retval;
    if (ext == NULL || !vec_is_free(slice) || ext->fragment_capacity < 1 || !edf_external_has_payload(ext)) {
        return 0;
    }
    if (!vec_is_slice(&ext->slices.payload.vec)) {
        if (!vec_is_slice(&ext->slices.control.vec) || (ext->fragment_capacity > 1 && !ext->fragments_compacted)) {
            return 0;
        }
        retval = vec_create_from_slice(slice, vec_buf_tail(&ext->slices.control.vec), vec_buf_tail(&ext->primary->vec));
    } else {
        retval = vec_clone_slice(slice, &ext->slices.payload.vec);
    }
    if (retval != 0) {
        *is_external_term = edf_external_is_pass_through(ext);
    }
    return retval;
}

int
edf_external_slice_payload_set(edf_external_t *ext, const uint8_t *head, const uint8_t *tail)
{
    edf_fragment_t *frag = NULL;
    if (ext == NULL || head == NULL || tail == NULL || ext->fragment_capacity < 1 || !edf_external_has_payload(ext)) {
        return 0;
    }
    // Ensure that the head and tail pointers are within the primary fragment.
    frag = ext->primary;
    if (!vec_contains_slice(&frag->vec, head, tail)) {
        return 0;
    }
    if (vec_is_slice(&ext->slices.payload.vec)) {
        (void)vec_destroy(&ext->slices.payload.vec);
        ext->slices.payload.offset = 0;
        ext->slices.payload.length = 0;
    }
    if (!vec_create_from_slice(&ext->slices.payload.vec, head, tail)) {
        return 0;
    }
    ext->slices.payload.offset = vec_buf(&ext->slices.payload.vec) - vec_buf(&frag->vec);
    ext->slices.payload.length = vec_len(&ext->slices.payload.vec);
    return 1;
}
