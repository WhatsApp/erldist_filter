/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_ATOM_CACHE_H
#define EDF_ATOM_CACHE_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../edf_common.h"
#include "../erts/external.h"
#include "edf_atom_text.h"

/* Macro Definitions */

/* Type Definitions */

typedef struct edf_atom_cache_s edf_atom_cache_t;
typedef struct edf_atom_cache_map_s edf_atom_cache_map_t;
typedef struct edf_atom_translation_table_s edf_atom_translation_table_t;
typedef struct edf_atom_translation_table_entry_s edf_atom_translation_table_entry_t;

struct edf_atom_cache_s {
    ERL_NIF_TERM entries[ERTS_ATOM_CACHE_SIZE];
};

struct edf_atom_cache_map_s {
    int hdr_sz;
    int sz;
    bool long_atoms;
    int cix[ERTS_ATOM_CACHE_SIZE];
    struct {
        ERL_NIF_TERM atom;
        int iix;
        bool new_entry;
    } cache[ERTS_ATOM_CACHE_SIZE];
};

struct edf_atom_translation_table_entry_s {
    ERL_NIF_TERM atom;
    int cache_index;
    bool new_entry;
    edf_atom_translation_table_entry_t *conflict;
};

struct edf_atom_translation_table_s {
    bool long_atoms;
    size_t size;
    edf_atom_translation_table_entry_t entries[ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES + 1];
};

/* Function Declarations */

extern int edf_atom_cache_index(ERL_NIF_TERM atom);
extern void edf_atom_cache_init(edf_atom_cache_t *cache);
extern void edf_atom_cache_destroy(edf_atom_cache_t *cache);
extern int edf_atom_cache_maybe_overwrite(edf_atom_cache_t *cache, int cache_index, ERL_NIF_TERM new_atom);

extern void edf_atom_cache_map_init(edf_atom_cache_map_t *acmp);
extern int edf_atom_cache_map_find_or_insert(edf_atom_cache_map_t *acmp, edf_atom_cache_t *cache, ERL_NIF_TERM atom);
extern int edf_atom_cache_map_set_entry(edf_atom_cache_map_t *acmp, int cache_index, int internal_index, ERL_NIF_TERM atom,
                                        bool new_entry);
extern int edf_atom_cache_map_set_size(edf_atom_cache_map_t *acmp, int size);

extern int edf_atom_translation_table_init(edf_atom_translation_table_t *attab);
extern void edf_atom_translation_table_destroy(edf_atom_translation_table_t *attab);
static int edf_atom_translation_table_get_entry(edf_atom_translation_table_t *attab, ERL_NIF_TERM atom, int *internal_index);
extern int edf_atom_translation_table_set_entry(edf_atom_translation_table_t *attab, int cache_index, int internal_index,
                                                ERL_NIF_TERM atom, bool new_entry);
extern int edf_atom_translation_table_set_size(edf_atom_translation_table_t *attab, size_t size);
extern int edf_atom_translation_table_fill_array(edf_atom_translation_table_t *attab, ERL_NIF_TERM *array, size_t array_length);

/* Inline Function Definitions */

inline int
edf_atom_translation_table_get_entry(edf_atom_translation_table_t *attab, ERL_NIF_TERM atom, int *internal_index)
{
    size_t i;
    if (atom == THE_NON_VALUE) {
        return 0;
    }
    for (i = 0; i < attab->size; i++) {
        if (atom == attab->entries[i].atom) {
            *internal_index = (int)(i);
            return 1;
        }
    }
    return 0;
}

#ifdef __cplusplus
}
#endif

#endif
