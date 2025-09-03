/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "edf_atom_cache.h"

#include "../erts/external.h"

extern int erts_debug_atom_to_out_cache_index(ERL_NIF_TERM atom);

int
edf_atom_cache_index(ERL_NIF_TERM atom)
{
    if (!enif_is_atom(NULL, atom)) {
        return -1;
    }
    return erts_debug_atom_to_out_cache_index(atom);
}

void
edf_atom_cache_init(edf_atom_cache_t *cache)
{
    int i;
    for (i = 0; i < ERTS_ATOM_CACHE_SIZE; i++) {
        cache->entries[i] = THE_NON_VALUE;
    }
    return;
}

void
edf_atom_cache_destroy(edf_atom_cache_t *cache)
{
    int i;
    if (cache == NULL) {
        return;
    }
    for (i = 0; i < ERTS_ATOM_CACHE_SIZE; i++) {
        if (cache->entries[i] != THE_NON_VALUE) {
            cache->entries[i] = THE_NON_VALUE;
        }
    }
    return;
}

int
edf_atom_cache_maybe_overwrite(edf_atom_cache_t *cache, int cache_index, ERL_NIF_TERM new_atom)
{
    ERL_NIF_TERM old_atom;
    if (cache == NULL || cache_index < 0 || cache_index >= ERTS_ATOM_CACHE_SIZE || new_atom == THE_NON_VALUE) {
        return 0;
    }
    old_atom = cache->entries[cache_index];
    if (old_atom == new_atom) {
        return 1;
    }
    cache->entries[cache_index] = new_atom;
    return 1;
}

void
edf_atom_cache_map_init(edf_atom_cache_map_t *acmp)
{
    int ix;
    if (acmp == NULL) {
        return;
    }
    acmp->hdr_sz = -1;
    acmp->sz = 0;
    acmp->long_atoms = false;
    for (ix = 0; ix < ERTS_ATOM_CACHE_SIZE; ix++) {
        acmp->cix[ix] = -1;
        acmp->cache[ix].atom = THE_NON_VALUE;
        acmp->cache[ix].iix = -1;
        acmp->cache[ix].new_entry = false;
    }
    return;
}

int
edf_atom_cache_map_find_or_insert(edf_atom_cache_map_t *acmp, edf_atom_cache_t *cache, ERL_NIF_TERM atom)
{
    int ix;
    if (acmp == NULL || !enif_is_atom(NULL, atom)) {
        return -1;
    }
    ix = edf_atom_cache_index(atom);
    if (acmp->cache[ix].iix >= 0) {
        if (acmp->cache[ix].atom != atom) {
            return -1;
        }
        return acmp->cache[ix].iix;
    }
    if (acmp->sz >= ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES) {
        return -1;
    }
    acmp->cache[ix].atom = atom;
    acmp->cache[ix].iix = acmp->sz;
    acmp->cache[ix].new_entry = (cache->entries[ix] == atom) ? false : true;
    acmp->cix[acmp->sz] = ix;
    acmp->sz += 1;
    return acmp->cache[ix].iix;
}

int
edf_atom_cache_map_set_entry(edf_atom_cache_map_t *acmp, int cache_index, int internal_index, ERL_NIF_TERM atom, bool new_entry)
{
    if (acmp == NULL || internal_index < 0 || internal_index >= acmp->sz || cache_index < 0 ||
        cache_index >= ERTS_ATOM_CACHE_SIZE) {
        return 0;
    }
    acmp->cache[cache_index].atom = atom;
    acmp->cache[cache_index].iix = internal_index;
    acmp->cache[cache_index].new_entry = new_entry;
    acmp->cix[internal_index] = cache_index;
    return 1;
}

int
edf_atom_cache_map_set_size(edf_atom_cache_map_t *acmp, int size)
{
    if (acmp == NULL || size > ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES) {
        return 0;
    }
    acmp->sz = size;
    return 1;
}

int
edf_atom_translation_table_init(edf_atom_translation_table_t *attab)
{
    int i;
    attab->long_atoms = false;
    attab->size = 0;
    for (i = 0; i < ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES + 1; i++) {
        attab->entries[i].atom = THE_NON_VALUE;
        attab->entries[i].cache_index = -1;
        attab->entries[i].new_entry = false;
        attab->entries[i].conflict = NULL;
    }
    return 1;
}

void
edf_atom_translation_table_destroy(edf_atom_translation_table_t *attab)
{
    int i;
    attab->long_atoms = false;
    attab->size = 0;
    for (i = 0; i < ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES + 1; i++) {
        attab->entries[i].atom = THE_NON_VALUE;
        attab->entries[i].cache_index = -1;
        attab->entries[i].new_entry = false;
        attab->entries[i].conflict = NULL;
    }
    return;
}

int
edf_atom_translation_table_set_entry(edf_atom_translation_table_t *attab, int cache_index, int internal_index, ERL_NIF_TERM atom,
                                     bool new_entry)
{
    if (attab == NULL || internal_index < 0 || internal_index >= (int)(attab->size) || cache_index < 0 ||
        cache_index >= ERTS_ATOM_CACHE_SIZE) {
        return 0;
    }
    attab->entries[internal_index].atom = atom;
    attab->entries[internal_index].cache_index = cache_index;
    attab->entries[internal_index].new_entry = new_entry;
    attab->entries[internal_index].conflict = NULL;
    return 1;
}

int
edf_atom_translation_table_set_size(edf_atom_translation_table_t *attab, size_t size)
{
    if (attab == NULL || size > ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES) {
        return 0;
    }
    attab->size = size;
    return 1;
}

int
edf_atom_translation_table_fill_array(edf_atom_translation_table_t *attab, ERL_NIF_TERM *array, size_t array_length)
{
    XNIF_TRACE_F("%s:%d edf_atom_translation_table_fill_array()\n", __FILE__, __LINE__);
    int i;
    if (attab == NULL || attab->size > ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES || attab->size != array_length) {
        return 0;
    }
    for (i = 0; i < attab->size; i++) {
        // Do we need to care if <TNV> happens here?
        if (attab->entries[i].atom == THE_NON_VALUE) {
            return 0;
        } else {
            array[i] = attab->entries[i].atom;
        }
    }
    return 1;
}
