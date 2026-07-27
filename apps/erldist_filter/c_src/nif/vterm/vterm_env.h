/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef VTERM_ENV_H
#define VTERM_ENV_H

#ifdef __cplusplus
extern "C" {
#endif

#include "vterm_common.h"

/* Macro Definitions */

/* Type Definitions */

// Don't include "edf_atom_cache.h", just reference the type here.
typedef struct edf_atom_translation_table_s edf_atom_translation_table_t;

typedef struct vterm_env_s vterm_env_t;
typedef struct vterm_env_ctx_s vterm_env_ctx_t;

struct vterm_env_ctx_s {
    void *hp_start;
    void *hp_end;
    void *hp;
};

struct vterm_env_s {
    ErlNifEnv *nif_env;
    edf_atom_translation_table_t *attab;
};

/* Global Declarations */

extern ErlNifResourceType *vterm_env_resource_type;

/* Function Declarations */

extern int vterm_env_load(ErlNifEnv *env);
extern void vterm_env_unload(ErlNifEnv *env);

extern vterm_env_t *vterm_env_alloc(edf_atom_translation_table_t *attab);
extern vterm_env_t *vterm_env_prealloc(edf_atom_translation_table_t *attab, size_t prealloc_heap_size);
extern void vterm_env_free(vterm_env_t *vtenv);
extern void vterm_env_dump_s(FILE *stream, vterm_env_t *vtenv);
extern void vterm_env_dump_bin_s(FILE *stream, const uint8_t *buf, size_t len);
extern void vterm_env_dump_mem_s(FILE *stream, const uint8_t *buf, size_t len);
static void vterm_env_dump(vterm_env_t *vtenv);
static void vterm_env_dump_bin(const uint8_t *buf, size_t len);
static void vterm_env_dump_mem(const uint8_t *buf, size_t len);
extern void *vterm_env_heap_reserve(vterm_env_t *vtenv, size_t size);
extern void *vterm_env_heap_reserve_strict(vterm_env_t *vtenv, size_t size);
extern int vterm_env_resolve(vterm_env_t *vtenv, vterm_t *vtp, ERL_NIF_TERM *termp);
extern void vterm_env_ctx_swap(vterm_env_t *vtenv, vterm_env_ctx_t *new_ctx, vterm_env_ctx_t *old_ctx);

extern int vterm_env_dist_ext_to_term(vterm_env_t *vtenv, const uint8_t *buf, size_t len, ERL_NIF_TERM *termp);
extern ERL_NIF_TERM vterm_env_direct_dist_ext_to_term(ErlNifEnv *env, ERL_NIF_TERM atoms_tuple, ERL_NIF_TERM input_binary);

/* Inline Function Definitions */

inline void
vterm_env_dump(vterm_env_t *vtenv)
{
    (void)vterm_env_dump_s(stderr, vtenv);
    return;
}

inline void
vterm_env_dump_bin(const uint8_t *buf, size_t len)
{
    (void)vterm_env_dump_bin_s(stderr, buf, len);
    return;
}

inline void
vterm_env_dump_mem(const uint8_t *buf, size_t len)
{
    (void)vterm_env_dump_mem_s(stderr, buf, len);
    return;
}

#ifdef __cplusplus
}
#endif

#endif
