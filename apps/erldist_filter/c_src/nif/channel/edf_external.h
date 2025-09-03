/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_EXTERNAL_H
#define EDF_EXTERNAL_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../erldist_filter_nif.h"
#include "edf_atom_cache.h"
#include "../udist/udist.h"
#include "../vec.h"

/* Macro Definitions */

/* Type Definitions */

// Don't include "edf_channel.h", just reference the type here.
typedef struct edf_channel_s edf_channel_t;

// Don't include "edf_logger.h", just reference the type here.
typedef struct edf_logger_event_s edf_logger_event_t;

// Don't include "vterm_env.h", just reference the type here.
typedef struct vterm_env_s vterm_env_t;

enum edf_external_mode_t {
    EDF_EXTERNAL_MODE_PASS_THROUGH = 0,
    EDF_EXTERNAL_MODE_NORMAL,
    EDF_EXTERNAL_MODE_FRAGMENT,
};

typedef struct edf_fragment_s edf_fragment_t;
typedef enum edf_external_mode_t edf_external_mode_t;
typedef struct edf_external_sequence_s edf_external_sequence_t;
typedef struct edf_external_slice_s edf_external_slice_t;
typedef struct edf_external_s edf_external_t;

struct edf_fragment_s {
    uint64_t sequence_id;
    uint64_t fragment_id;
    vec_t vec;
    size_t skip;
};

struct edf_external_sequence_s {
    edf_external_t *parent;
    edf_external_t *left;
    edf_external_t *right;
    int is_red;
    int is_linked;
};

struct edf_external_slice_s {
    vec_t vec;
    size_t offset;
    size_t length;
};

#define EDF_EXTERNAL_FLAG_NONE (0)
#define EDF_EXTERNAL_FLAG_ATOM_CACHE_REFS (1 << 0)
#define EDF_EXTERNAL_FLAG_ATOM_CACHE_READ (1 << 1)
#define EDF_EXTERNAL_FLAG_ATOM_CACHE_WRITE (1 << 2)
#define EDF_EXTERNAL_FLAG_ATOM_CACHE_OVERWRITE (1 << 3)
#define EDF_EXTERNAL_FLAG_ATOM_CACHE_DIRTY (1 << 4)
#define EDF_EXTERNAL_FLAG_ATOM_CACHE_NEED_COMMIT (1 << 5)
#define EDF_EXTERNAL_FLAG_ATOM_CACHE_NEED_REWRITE (1 << 6)
#define EDF_EXTERNAL_FLAG_ATOM_CACHE_NEED_ROLLBACK (1 << 7)

struct edf_external_s {
    edf_external_sequence_t _sequence;
    edf_channel_t *channel;
    int emit;
    edf_external_mode_t mode;
    int flags;
    edf_atom_translation_table_t attab;
    edf_atom_translation_table_t rollback;
    vec_t rollback_vec;
    struct {
        edf_external_slice_t framing;
        edf_external_slice_t headers;
        edf_external_slice_t control;
        edf_external_slice_t payload;
        size_t expected_payload_length;
    } slices;
    vterm_env_t *vtenv;
    udist_t up[1];
    size_t control_heap_size;
    size_t payload_heap_size;
    bool logging_to_action;
    edf_logger_event_t *logger_event;
    uint64_t sequence_id;
    uint64_t fragment_id_next;
    uint64_t fragment_count;
    bool fragments_compacted;
    edf_fragment_t compact[1];
    edf_fragment_t *primary;
    size_t fragment_capacity;
    edf_fragment_t *fragments;
};

/* Function Declarations */

extern int edf_external_create(edf_channel_t *channel, edf_external_mode_t mode, uint64_t sequence_id, uint64_t fragment_count,
                               vec_t *src_vec, size_t skip, edf_external_t **extp);
extern void edf_external_destroy(edf_external_t *ext);
extern int edf_external_add_fragment(edf_external_t *ext, uint64_t fragment_id, vec_t *src_vec, size_t skip);
static bool edf_external_has_payload(const edf_external_t *ext);
static bool edf_external_is_pass_through(const edf_external_t *ext);
extern int edf_external_compact_start(edf_external_t *ext, size_t new_size);
static int edf_external_set_fragment_id(edf_external_t *ext, edf_fragment_t *frag, uint64_t fragment_id);
extern int edf_external_slice_framing_get(edf_external_t *ext, vec_t *slice);
extern int edf_external_slice_framing_set(edf_external_t *ext, const uint8_t *head, const uint8_t *tail);
extern int edf_external_slice_headers_get(edf_external_t *ext, vec_t *slice);
extern int edf_external_slice_headers_set(edf_external_t *ext, const uint8_t *head, const uint8_t *tail);
extern int edf_external_slice_control_get(edf_external_t *ext, bool *is_external_term, vec_t *slice);
extern int edf_external_slice_control_set(edf_external_t *ext, const uint8_t *head, const uint8_t *tail);
extern int edf_external_slice_payload_get(edf_external_t *ext, bool *is_external_term, vec_t *slice);
extern int edf_external_slice_payload_set(edf_external_t *ext, const uint8_t *head, const uint8_t *tail);

static void edf_external_slice_init_empty(edf_external_slice_t *slice);
static void edf_external_slice_destroy(edf_external_slice_t *slice);

/* Inline Function Definitions */

inline bool
edf_external_has_payload(const edf_external_t *ext)
{
    return (ext->up->info.payload);
}

inline bool
edf_external_is_pass_through(const edf_external_t *ext)
{
    return (ext->mode == EDF_EXTERNAL_MODE_PASS_THROUGH) ? true : false;
}

inline int
edf_external_set_fragment_id(edf_external_t *ext, edf_fragment_t *frag, uint64_t fragment_id)
{
    vec_writer_t vw[1];
    if (ext == NULL || ext->fragment_capacity < 1) {
        return 0;
    }
    if (!vec_writer_create(vw, &frag->vec, 1 + 1 + 8)) {
        return 0;
    }
    if (!vec_writer_write_u64(vw, fragment_id)) {
        (void)vec_writer_destroy(vw);
        return 0;
    }
    frag->fragment_id = fragment_id;
    (void)vec_writer_destroy(vw);
    return 1;
}

inline void
edf_external_slice_init_empty(edf_external_slice_t *slice)
{
    (void)vec_init_free(&slice->vec);
    slice->vec.data.slice.head = NULL;
    slice->vec.data.slice.tail = NULL;
    slice->offset = 0;
    slice->length = 0;
    return;
}

inline void
edf_external_slice_destroy(edf_external_slice_t *slice)
{
    (void)vec_destroy(&slice->vec);
    slice->vec.data.slice.head = NULL;
    slice->vec.data.slice.tail = NULL;
    slice->offset = 0;
    slice->length = 0;
    return;
}

#ifdef __cplusplus
}
#endif

#endif
