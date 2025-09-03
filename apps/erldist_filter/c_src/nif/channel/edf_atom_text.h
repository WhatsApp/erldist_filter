/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_ATOM_TEXT_H
#define EDF_ATOM_TEXT_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../erldist_filter_nif.h"
#include "../erts/atom.h"

/* Function Declarations */

static void edf_atom_text_drop_name(const uint8_t **namep);
static int edf_atom_text_get_length(ERL_NIF_TERM atom, ErlNifCharEncoding enc, size_t *lenp);
static int edf_atom_text_get_name(ERL_NIF_TERM atom, ErlNifCharEncoding enc, const uint8_t **namep, size_t *lenp);
static int edf_atom_text_inspect_as_binary(ERL_NIF_TERM atom, ErlNifCharEncoding enc, ErlNifBinary *atom_text_bin);
static int edf_atom_text_put(const uint8_t *name, size_t len, ErlNifCharEncoding enc, ERL_NIF_TERM *atomp);

/* Inline Function Definitions */

inline void
edf_atom_text_drop_name(const uint8_t **namep)
{
    if (namep == NULL || *namep == NULL) {
        return;
    }
    (void)enif_free((void *)*namep);
    *namep = NULL;
    return;
}

inline int
edf_atom_text_get_length(ERL_NIF_TERM atom, ErlNifCharEncoding enc, size_t *lenp)
{
    unsigned len = 0;
    if (!enif_get_atom_length(NULL, atom, &len, enc)) {
        return 0;
    }
    if (lenp != NULL) {
        *lenp = (size_t)len;
    }
    return 1;
}

inline int
edf_atom_text_get_name(ERL_NIF_TERM atom, ErlNifCharEncoding enc, const uint8_t **namep, size_t *lenp)
{
    uint8_t *name = NULL;
    size_t len = 0;
    if (!edf_atom_text_get_length(atom, enc, &len)) {
        return 0;
    }
    if (namep != NULL) {
        name = (uint8_t *)enif_alloc(len + 1);
        if (name == NULL) {
            return 0;
        }
        if (enif_get_atom(NULL, atom, (char *)name, (unsigned)len + 1, enc) == 0) {
            (void)enif_free((void *)name);
            return 0;
        }
        *namep = name;
    }
    if (lenp != NULL) {
        *lenp = len;
    }
    return 1;
}

inline int
edf_atom_text_inspect_as_binary(ERL_NIF_TERM atom, ErlNifCharEncoding enc, ErlNifBinary *atom_text_bin)
{
    const uint8_t *name = NULL;
    size_t len = 0;

    if (!edf_atom_text_get_name(atom, enc, &name, &len)) {
        return 0;
    }
    if (!enif_alloc_binary(len, atom_text_bin)) {
        (void)edf_atom_text_drop_name(&name);
        return 0;
    }
    (void)memcpy(atom_text_bin->data, name, len);
    (void)edf_atom_text_drop_name(&name);
    return 1;
}

inline int
edf_atom_text_put(const uint8_t *name, size_t len, ErlNifCharEncoding enc, ERL_NIF_TERM *atomp)
{
    return enif_make_new_atom_len(NULL, (const char *)name, len, atomp, enc);
}

#ifdef __cplusplus
}
#endif

#endif
