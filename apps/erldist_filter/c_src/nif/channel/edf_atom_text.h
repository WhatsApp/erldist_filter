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

#include "../edf_common.h"
#include "../erts/atom.h"

/* Macro Definitions */

// NOTE: OTP 26 will have UTF-8 support for NIFs
#if !defined(ERL_NIF_UTF8)
#define EDF_ATOM_TEXT_FALLBACK 1
#endif

// #define EDF_ATOM_TEXT_GARBAGE_COLLECT 1

/* Type Definitions */

typedef struct edf_atom_text_table_s edf_atom_text_table_t;

extern edf_atom_text_table_t *edf_atom_text_table;

/* Function Declarations */

extern int edf_atom_text_table_init(void);
extern void edf_atom_text_table_destroy(void);
extern int edf_atom_text_table_size(void);
extern ERL_NIF_TERM edf_atom_text_table_list(ErlNifEnv *env);
#ifdef EDF_ATOM_TEXT_FALLBACK
extern int edf_atom_text_put_and_keep(const uint8_t *name, signed int len, ErtsAtomEncoding enc, ERL_NIF_TERM *atomp);
extern int edf_atom_text_get_length(ERL_NIF_TERM atom, ErtsAtomEncoding enc, size_t *lenp);
extern int edf_atom_text_get_name(ERL_NIF_TERM atom, ErtsAtomEncoding enc, const uint8_t **namep, size_t *lenp);
static void edf_atom_text_drop_name(const uint8_t **namep);
#else
static int edf_atom_text_put_and_keep(const uint8_t *name, signed int len, ErtsAtomEncoding enc, ERL_NIF_TERM *atomp);
static int edf_atom_text_get_length(ERL_NIF_TERM atom, ErtsAtomEncoding enc, size_t *lenp);
static int edf_atom_text_get_name(ERL_NIF_TERM atom, ErtsAtomEncoding enc, const uint8_t **namep, size_t *lenp);
static void edf_atom_text_drop_name(const uint8_t **namep);
#endif
static int edf_atom_text_inspect_as_binary(ERL_NIF_TERM atom, ErlNifBinary *atom_text_bin);
extern int edf_atom_text_keep_slow(ERL_NIF_TERM atom);
extern void edf_atom_text_release_slow(ERL_NIF_TERM atom);

#ifdef EDF_ATOM_TEXT_GARBAGE_COLLECT
#define edf_atom_text_keep(atom) edf_atom_text_keep_slow((atom))
#define edf_atom_text_release(atom) edf_atom_text_release_slow((atom))
#else
#define edf_atom_text_keep(atom) (((atom) == THE_NON_VALUE) ? 0 : 1)
#define edf_atom_text_release(atom) ((void)0)
#endif

/* Inline Function Definitions */

#ifdef EDF_ATOM_TEXT_FALLBACK

inline void
edf_atom_text_drop_name(const uint8_t **namep)
{
    if (namep == NULL) {
        return;
    }
    *namep = NULL;
    return;
}

#else

inline int
edf_atom_text_put_and_keep(const uint8_t *name, signed int len, ErtsAtomEncoding enc, ERL_NIF_TERM *atomp)
{
    ERL_NIF_TERM key;
    key = erts_atom_put(name, len, enc, 0);
    if (key == THE_NON_VALUE) {
        return 0;
    }
    if (atomp != NULL) {
        *atomp = key;
    }
    return 1;
}

inline int
edf_atom_text_get_length(ERL_NIF_TERM atom, ErtsAtomEncoding enc, size_t *lenp)
{
    ErlNifCharEncoding encoding = ((enc == ERTS_ATOM_ENC_UTF8) ? ERL_NIF_UTF8 : ERL_NIF_LATIN1);
    unsigned len = 0;
    if (!enif_get_atom_length(NULL, atom, &len, encoding)) {
        return 0;
    }
    if (lenp != NULL) {
        *lenp = (size_t)len;
    }
    return 1;
}

inline int
edf_atom_text_get_name(ERL_NIF_TERM atom, ErtsAtomEncoding enc, const uint8_t **namep, size_t *lenp)
{
    ErlNifCharEncoding encoding = ((enc == ERTS_ATOM_ENC_UTF8) ? ERL_NIF_UTF8 : ERL_NIF_LATIN1);
    uint8_t *name = NULL;
    size_t len = 0;
    if (!edf_atom_text_get_length(atom, enc, &len)) {
        return 0;
    }
    if (namep != NULL) {
        name = (void *)enif_alloc(len + 1);
        if (name == NULL) {
            return 0;
        }
        if (!enif_get_atom(NULL, atom, (char *)name, (unsigned)len, encoding)) {
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

inline void
edf_atom_text_drop_name(const uint8_t **namep)
{
    if (namep == NULL) {
        return;
    }
    (void)enif_free((void *)*namep);
    *namep = NULL;
    return;
}

#endif

inline int
edf_atom_text_inspect_as_binary(ERL_NIF_TERM atom, ErlNifBinary *atom_text_bin)
{
    const uint8_t *name = NULL;
    size_t len = 0;

    if (!edf_atom_text_keep(atom)) {
        return 0;
    }
    if (!edf_atom_text_get_name(atom, ERTS_ATOM_ENC_UTF8, &name, &len)) {
        (void)edf_atom_text_release(atom);
        return 0;
    }
    if (!enif_alloc_binary(len, atom_text_bin)) {
        (void)edf_atom_text_drop_name(&name);
        (void)edf_atom_text_release(atom);
        return 0;
    }
    (void)memcpy(atom_text_bin->data, name, len);
    (void)edf_atom_text_drop_name(&name);
    (void)edf_atom_text_release(atom);
    return 1;
}

#ifdef __cplusplus
}
#endif

#endif
