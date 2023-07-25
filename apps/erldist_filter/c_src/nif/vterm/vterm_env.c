/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "vterm_env.h"
#include "vterm.h"

#include "../core/khashl.h"

/* Danger Area */

typedef struct __vterm_erl_nif_env_s __vterm_erl_nif_env_t;
typedef struct __vterm_erl_process_s __vterm_erl_process_t;

extern ERL_NIF_TERM erts_debug_dist_ext_to_term_2(__vterm_erl_process_t *A__p, ERL_NIF_TERM *BIF__ARGS, const void *A__I);

struct __vterm_erl_process_s {
    void *_void;
};

struct __vterm_erl_nif_env_s {
    void *mod_nif;
    __vterm_erl_process_t *proc;
};

/* Type Definitions */

typedef struct __vterm_env_s __vterm_env_t;
typedef struct vterm_env_heap_direct_s vterm_env_heap_direct_t;
typedef struct vterm_resolved_table_s vterm_resolved_table_t;

struct vterm_env_heap_direct_s {
    vterm_env_heap_direct_t *next;
    vword_t _dummy; /* align to 2*sizeof(void*) */
    alignas(vword_t) vword_t data[1];
};

struct __vterm_env_s {
    vterm_env_t super;
    ErlNifEnv *tmp_env;
    void *resource;
    ERL_NIF_TERM atoms;
    vterm_resolved_table_t *resolved;
    void *hp_start;
    void *hp_end;
    void *hp;
    vterm_env_heap_direct_t *directs;
    alignas(vword_t) vword_t _prealloc[1];
};

/* Type Definitions (Resolved Table) */

typedef uintptr_t vterm_resolved_hm_key_t;
typedef struct vterm_resolved_hm_val_s vterm_resolved_hm_val_t;

struct vterm_resolved_hm_val_s {
    ERL_NIF_TERM term;
};

static khint_t vterm_resolved_hm_key_hash_fn(vterm_resolved_hm_key_t key);
static bool vterm_resolved_hm_key_hash_eq(vterm_resolved_hm_key_t k1, vterm_resolved_hm_key_t k2);
static void vterm_resolved_hm_val_destroy(__vterm_env_t *vtenv, vterm_resolved_table_t *table, vterm_resolved_hm_key_t *key,
                                          vterm_resolved_hm_val_t *val);

KHASHL_CMAP_INIT(KH_LOCAL, vterm_resolved_hm_t, vterm_resolved_hm, vterm_resolved_hm_key_t, vterm_resolved_hm_val_t,
                 vterm_resolved_hm_key_hash_fn, vterm_resolved_hm_key_hash_eq)

struct vterm_resolved_table_s {
    vterm_resolved_hm_t *hm;
};

static int vterm_resolved_table_create(__vterm_env_t *vtenv, vterm_resolved_table_t **tablep);
static void vterm_resolved_table_destroy(__vterm_env_t *vtenv, vterm_resolved_table_t *table);
static int vterm_resolved_table_get(__vterm_env_t *vtenv, vterm_resolved_table_t *table, vterm_t *vtp, ERL_NIF_TERM *termp);

/* Function Definitions (Resolved Table) */

int
vterm_resolved_table_create(__vterm_env_t *vtenv, vterm_resolved_table_t **tablep)
{
    vterm_resolved_table_t *table = NULL;

    (void)vtenv;

    table = (vterm_resolved_table_t *)enif_alloc(sizeof(vterm_resolved_table_t));
    if (table == NULL) {
        return 0;
    }
    table->hm = vterm_resolved_hm_init();
    if (table->hm == NULL) {
        (void)enif_free((void *)table);
        return 0;
    }
    *tablep = table;
    return 1;
}

void
vterm_resolved_table_destroy(__vterm_env_t *vtenv, vterm_resolved_table_t *table)
{
    khint_t i;
    vterm_resolved_hm_key_t *key = NULL;
    vterm_resolved_hm_val_t *val = NULL;

    if (table->hm != NULL) {
        // (void)enif_fprintf(stderr, "RESOLVED TABLE SIZE IS %d\n", kh_size(table->hm));
        // (void)fflush(stderr);
        for (i = 0; i != kh_end(table->hm); i++) {
            if (!kh_exist(table->hm, i)) {
                continue;
            }
            key = &(kh_key(table->hm, i));
            val = &(kh_val(table->hm, i));
            // (void)enif_fprintf(stderr, "RESOLVED TABLE CONTAINS %T\n", val->term);
            // (void)fflush(stderr);
            (void)vterm_resolved_hm_val_destroy(vtenv, table, key, val);
        }
        (void)vterm_resolved_hm_destroy(table->hm);
        table->hm = NULL;
    }
    (void)enif_free(table);
    return;
}

int
vterm_resolved_table_get(__vterm_env_t *vtenv, vterm_resolved_table_t *table, vterm_t *vtp, ERL_NIF_TERM *termp)
{
    vterm_resolved_hm_key_t key;
    int absent = -1;
    khint_t slot;
    vterm_resolved_hm_val_t *val = NULL;

    if (!vterm_maybe_decode_lazy_term(&vtenv->super, vtp)) {
        return 0;
    }

    key = (vterm_resolved_hm_key_t)(*vtp);
    slot = vterm_resolved_hm_get(table->hm, key);
    if (slot != kh_end(table->hm) && kh_exist(table->hm, slot)) {
        if (termp != NULL) {
            val = &(kh_val(table->hm, slot));
            *termp = val->term;
        }
        return 1;
    }
    slot = vterm_resolved_hm_put(table->hm, key, &absent);
    if (absent) {
        val = &(kh_val(table->hm, slot));
        val->term = THE_NON_VALUE;
        if (!vterm_encode_and_try_resolve(vtenv->super.nif_env, &vtenv->super, vtp, &val->term)) {
            (void)vterm_resolved_hm_val_destroy(vtenv, table, &key, val);
            (void)vterm_resolved_hm_del(table->hm, slot);
            return 0;
        }
    }
    if (termp != NULL) {
        val = &(kh_val(table->hm, slot));
        *termp = val->term;
    }
    return 1;
}

inline khint_t
vterm_resolved_hm_key_hash_fn(vterm_resolved_hm_key_t key)
{
    return kh_hash_uint64((khint64_t)key);
}

inline bool
vterm_resolved_hm_key_hash_eq(vterm_resolved_hm_key_t k1, vterm_resolved_hm_key_t k2)
{
    return (k1 == k2);
}

inline void
vterm_resolved_hm_val_destroy(__vterm_env_t *vtenv, vterm_resolved_table_t *table, vterm_resolved_hm_key_t *key,
                              vterm_resolved_hm_val_t *val)
{
    (void)vtenv;
    (void)table;
    (void)key;
    val->term = THE_NON_VALUE;
    return;
}

/* Function Definitions */

ErlNifResourceType *vterm_env_resource_type = NULL;

static void vterm_env_resource_type_dtor(ErlNifEnv *env, void *obj);

void
vterm_env_resource_type_dtor(ErlNifEnv *env, void *obj)
{
    (void)env;
    (void)obj;
    return;
}

int
vterm_env_load(ErlNifEnv *env)
{
    int retval = 0;

    static ErlNifResourceTypeInit vterm_env_resource_type_init = {
        .dtor = vterm_env_resource_type_dtor,
        .stop = NULL,
        .down = NULL,
        .members = 4,
        .dyncall = NULL,
    };
    vterm_env_resource_type = enif_init_resource_type(env, "erldist_filter_nif_vterm_env", &vterm_env_resource_type_init,
                                                      ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    if (vterm_env_resource_type == NULL) {
        retval = -1;
        return retval;
    }

    return retval;
}

void
vterm_env_unload(ErlNifEnv *env)
{
    (void)env;
    vterm_env_resource_type = NULL;
    return;
}

static ERL_NIF_TERM vterm_env_make_atoms_tuple(__vterm_env_t *vtenv);

vterm_env_t *
vterm_env_alloc(edf_atom_translation_table_t *attab)
{
    return vterm_env_prealloc(attab, 0);
}

vterm_env_t *
vterm_env_prealloc(edf_atom_translation_table_t *attab, size_t prealloc_heap_size)
{
    __vterm_env_t *vtenv = enif_alloc(offsetof(__vterm_env_t, _prealloc) + prealloc_heap_size);
    if (vtenv == NULL) {
        return NULL;
    }
    // (void)memset((void *)vtenv, 0, offsetof(__vterm_env_t, _prealloc) + prealloc_heap_size);
    vtenv->super.nif_env = enif_alloc_env();
    if (vtenv->super.nif_env == NULL) {
        (void)enif_free((void *)vtenv);
        return NULL;
    }
    vtenv->super.attab = attab;
    vtenv->tmp_env = NULL;
    vtenv->resource = NULL;
    if (attab == NULL) {
        vtenv->atoms = enif_make_tuple(vtenv->super.nif_env, 0);
    } else {
        vtenv->atoms = vterm_env_make_atoms_tuple(vtenv);
    }
    vtenv->resolved = NULL;
    vtenv->hp_start = (void *)&vtenv->_prealloc[0];
    vtenv->hp_end = vtenv->hp_start + prealloc_heap_size;
    vtenv->hp = vtenv->hp_start;
    vtenv->directs = NULL;
    vtenv->resource = enif_alloc_resource(vterm_env_resource_type, 0);
    if (vtenv->resource == NULL) {
        (void)vterm_env_free(&vtenv->super);
        return NULL;
    }
    (void)enif_make_resource(vtenv->super.nif_env, vtenv->resource);
    (void)enif_release_resource(vtenv->resource);
    return &vtenv->super;
}

ERL_NIF_TERM
vterm_env_make_atoms_tuple(__vterm_env_t *vtenv)
{
    edf_atom_cache_t atom_cache;
    unsigned int cnt = (unsigned)(vtenv->super.attab->size);
    unsigned int i;
    for (i = 0; i < cnt; i++) {
        atom_cache.entries[i] = vtenv->super.attab->entries[i].atom;
    }
    return enif_make_tuple_from_array(vtenv->super.nif_env, atom_cache.entries, cnt);
}

void
vterm_env_dump_s(FILE *stream, vterm_env_t *super)
{
    __vterm_env_t *vtenv = (void *)super;
    size_t head_byte_size = (size_t)(vtenv->hp - vtenv->hp_start);
    size_t tail_byte_size = (size_t)(vtenv->hp_end - vtenv->hp);
    (void)enif_fprintf(stream, "VTERM ENV HEAP HEAD\n");
    (void)vterm_env_dump_mem_s(stream, vtenv->hp_start, head_byte_size);
    (void)enif_fprintf(stream, "VTERM ENV HEAP TAIL\n");
    (void)vterm_env_dump_mem(vtenv->hp, tail_byte_size);
    return;
}

void
vterm_env_dump_bin_s(FILE *stream, const uint8_t *buf, size_t len)
{
    size_t i;
    (void)enif_fprintf(stream, "<<");
    for (i = 0; i < len; i++) {
        (void)enif_fprintf(stream, "%u", buf[i]);
        if (i + 1 < len) {
            (void)enif_fprintf(stream, ",");
        }
    }
    (void)enif_fprintf(stream, ">>");
    return;
}

void
vterm_env_dump_mem_s(FILE *stream, const uint8_t *buf, size_t len)
{
    size_t i;
    for (i = 0; i < len; i++) {
        uint8_t byte = buf[i];
        if (i % 8 == 0) {
            (void)enif_fprintf(stream, " -> %p\n[% 4d]", (void *)(buf + i), i);
        }
        (void)enif_fprintf(stream, " %02x", byte);
    }
    (void)enif_fprintf(stream, "\n");
    return;
}

void *
vterm_env_heap_reserve(vterm_env_t *super, size_t size)
{
    __vterm_env_t *vtenv = (void *)super;
    vterm_env_heap_direct_t *direct = NULL;

    if (size == 0) {
        return NULL;
    }

    direct = enif_alloc(offsetof(vterm_env_heap_direct_t, data) + size);
    direct->next = vtenv->directs;
    vtenv->directs = direct;

    return (void *)&direct->data[0];
}

void *
vterm_env_heap_reserve_strict(vterm_env_t *super, size_t size)
{
    __vterm_env_t *vtenv = (void *)super;

    if (size == 0) {
        return NULL;
    }

    if (vtenv->hp != NULL && (size_t)(vtenv->hp_end - vtenv->hp) >= size) {
        void *p = vtenv->hp;
        vtenv->hp += size;
        return p;
    }

    (void)enif_fprintf(stderr, "Attempted to reserve %llu bytes which is not allowed in strict mode.\n", size);
    (void)fflush(stderr);
    abort();
}

void
vterm_env_free(vterm_env_t *super)
{
    __vterm_env_t *vtenv = (void *)super;
    if (vtenv != NULL) {
        if (vtenv->resolved != NULL) {
            (void)vterm_resolved_table_destroy(vtenv, vtenv->resolved);
            vtenv->resolved = NULL;
        }
        while (vtenv->directs != NULL) {
            vterm_env_heap_direct_t *direct = vtenv->directs;
            vtenv->directs = direct->next;
            (void)enif_free((void *)direct);
        }
        if (vtenv->tmp_env != NULL) {
            (void)enif_free_env(vtenv->tmp_env);
            vtenv->tmp_env = NULL;
        }
        if (vtenv->super.nif_env != NULL) {
            (void)enif_free_env(vtenv->super.nif_env);
            vtenv->super.nif_env = NULL;
        }
        (void)enif_free((void *)vtenv);
    }
}

int
vterm_env_resolve(vterm_env_t *super, vterm_t *vtp, ERL_NIF_TERM *termp)
{
    __vterm_env_t *vtenv = (void *)super;
    if (vtenv->resolved == NULL && !vterm_resolved_table_create(vtenv, &vtenv->resolved)) {
        return 0;
    }
    return vterm_resolved_table_get(vtenv, vtenv->resolved, vtp, termp);
}

void
vterm_env_ctx_swap(vterm_env_t *super, vterm_env_ctx_t *new_ctx, vterm_env_ctx_t *old_ctx)
{
    __vterm_env_t *vtenv = (void *)super;
    old_ctx->hp_start = vtenv->hp_start;
    old_ctx->hp_end = vtenv->hp_end;
    old_ctx->hp = vtenv->hp;
    vtenv->hp_start = new_ctx->hp_start;
    vtenv->hp_end = new_ctx->hp_end;
    vtenv->hp = new_ctx->hp;
    return;
}

int
vterm_env_dist_ext_to_term(vterm_env_t *super, const uint8_t *buf, size_t len, ERL_NIF_TERM *termp)
{
    __vterm_env_t *vtenv = (void *)super;
    ERL_NIF_TERM bif_args[2];
    ERL_NIF_TERM bif_ret = THE_NON_VALUE;
    if (vtenv->tmp_env == NULL) {
        vtenv->tmp_env = enif_alloc_env();
        if (vtenv->tmp_env == NULL) {
            return 0;
        }
    } else {
        (void)enif_clear_env(vtenv->tmp_env);
    }
    bif_args[0] = vtenv->atoms;
    bif_args[1] = enif_make_resource_binary(vtenv->tmp_env, vtenv->resource, buf, len);
    bif_ret = erts_debug_dist_ext_to_term_2(((__vterm_erl_nif_env_t *)(vtenv->tmp_env))->proc, bif_args, NULL);
    if (bif_ret == THE_NON_VALUE) {
        {
            (void)enif_fprintf(stderr, "Atoms = %T\n", vtenv->atoms);
            (void)enif_fprintf(stderr, "Binary = ");
            (void)vterm_env_dump_bin(buf, len);
            (void)enif_fprintf(stderr, "\n");
            (void)fflush(stderr);
        };
        return 0;
    }
    *termp = bif_ret;
    return 1;
}

ERL_NIF_TERM
vterm_env_direct_dist_ext_to_term(ErlNifEnv *env, ERL_NIF_TERM atoms_tuple, ERL_NIF_TERM input_binary)
{
    ErlNifEnv *tmp_env = NULL;
    ERL_NIF_TERM bif_args[2];
    ERL_NIF_TERM bif_ret = THE_NON_VALUE;
    tmp_env = enif_alloc_env();
    if (tmp_env == NULL) {
        return enif_make_badarg(env);
    }
    bif_args[0] = atoms_tuple;
    bif_args[1] = input_binary;
    bif_ret = erts_debug_dist_ext_to_term_2(((__vterm_erl_nif_env_t *)(tmp_env))->proc, bif_args, NULL);
    if (bif_ret == THE_NON_VALUE) {
        (void)enif_free_env(tmp_env);
        return enif_make_badarg(env);
    }
    bif_ret = enif_make_copy(env, bif_ret);
    (void)enif_free_env(tmp_env);
    return bif_ret;
}
