/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "vterm_env.h"
#include "vterm.h"

// disable packed structs in khashl.h
#define kh_packed
#include "../core/khashl.h"

/* Type Definitions */

typedef struct __vterm_env_s __vterm_env_t;
typedef struct vterm_env_dist_ext_rewrite_s vterm_env_dist_ext_rewrite_t;
typedef struct vterm_env_heap_direct_s vterm_env_heap_direct_t;
typedef struct vterm_resolved_table_s vterm_resolved_table_t;

#define VTERM_ENV_DIST_EXT_MAX_DEPTH (1024)

/* enif_binary_to_term() rejects distribution atom-cache references. Walk the
 * ETF once to size their expanded atom encodings, then again to emit them. */
struct vterm_env_dist_ext_rewrite_s {
    ErlNifEnv *env;
    const ERL_NIF_TERM *atoms;
    int atom_count;
    ErlNifBinary atom_bins[ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES];
    bool atom_bin_initialized[ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES];
    const uint8_t *src;
    const uint8_t *src_end;
    uint8_t *dst;
    size_t dst_capacity;
    size_t dst_offset;
};

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
static int vterm_env_dist_ext_rewrite_append(vterm_env_dist_ext_rewrite_t *rewrite, const uint8_t *buf, size_t len);
static int vterm_env_dist_ext_rewrite_copy(vterm_env_dist_ext_rewrite_t *rewrite, size_t len);
static int vterm_env_dist_ext_rewrite_read_u8(vterm_env_dist_ext_rewrite_t *rewrite, uint8_t *valuep, bool copy);
static int vterm_env_dist_ext_rewrite_read_u16(vterm_env_dist_ext_rewrite_t *rewrite, uint16_t *valuep);
static int vterm_env_dist_ext_rewrite_read_u32(vterm_env_dist_ext_rewrite_t *rewrite, uint32_t *valuep);
static int vterm_env_dist_ext_rewrite_term(vterm_env_dist_ext_rewrite_t *rewrite, unsigned int depth);
static int vterm_env_dist_ext_rewrite_terms(vterm_env_dist_ext_rewrite_t *rewrite, uint32_t count, unsigned int depth);
static int vterm_env_dist_ext_rewrite_atom_cache_ref(vterm_env_dist_ext_rewrite_t *rewrite);
static int vterm_env_dist_ext_rewrite_patch_u32(vterm_env_dist_ext_rewrite_t *rewrite, size_t offset, size_t value);
static void vterm_env_dist_ext_rewrite_destroy(vterm_env_dist_ext_rewrite_t *rewrite);

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
vterm_env_dist_ext_rewrite_append(vterm_env_dist_ext_rewrite_t *rewrite, const uint8_t *buf, size_t len)
{
    if (len > SIZE_MAX - rewrite->dst_offset) {
        return 0;
    }
    if (rewrite->dst != NULL) {
        if (rewrite->dst_offset > rewrite->dst_capacity || len > rewrite->dst_capacity - rewrite->dst_offset) {
            return 0;
        }
        if (len > 0) {
            (void)memcpy(rewrite->dst + rewrite->dst_offset, buf, len);
        }
    }
    rewrite->dst_offset += len;
    return 1;
}

int
vterm_env_dist_ext_rewrite_copy(vterm_env_dist_ext_rewrite_t *rewrite, size_t len)
{
    if ((size_t)(rewrite->src_end - rewrite->src) < len) {
        return 0;
    }
    if (!vterm_env_dist_ext_rewrite_append(rewrite, rewrite->src, len)) {
        return 0;
    }
    rewrite->src += len;
    return 1;
}

int
vterm_env_dist_ext_rewrite_read_u8(vterm_env_dist_ext_rewrite_t *rewrite, uint8_t *valuep, bool copy)
{
    if (rewrite->src == rewrite->src_end) {
        return 0;
    }
    *valuep = rewrite->src[0];
    if (copy) {
        return vterm_env_dist_ext_rewrite_copy(rewrite, 1);
    }
    rewrite->src += 1;
    return 1;
}

int
vterm_env_dist_ext_rewrite_read_u16(vterm_env_dist_ext_rewrite_t *rewrite, uint16_t *valuep)
{
    const uint8_t *p = rewrite->src;
    if ((size_t)(rewrite->src_end - p) < 2) {
        return 0;
    }
    *valuep = (uint16_t)(((uint16_t)p[0] << 8) | (uint16_t)p[1]);
    return vterm_env_dist_ext_rewrite_copy(rewrite, 2);
}

int
vterm_env_dist_ext_rewrite_read_u32(vterm_env_dist_ext_rewrite_t *rewrite, uint32_t *valuep)
{
    const uint8_t *p = rewrite->src;
    if ((size_t)(rewrite->src_end - p) < 4) {
        return 0;
    }
    *valuep = ((uint32_t)p[0] << 24) | ((uint32_t)p[1] << 16) | ((uint32_t)p[2] << 8) | (uint32_t)p[3];
    return vterm_env_dist_ext_rewrite_copy(rewrite, 4);
}

int
vterm_env_dist_ext_rewrite_terms(vterm_env_dist_ext_rewrite_t *rewrite, uint32_t count, unsigned int depth)
{
    uint32_t i;
    if ((size_t)count > (size_t)(rewrite->src_end - rewrite->src)) {
        return 0;
    }
    for (i = 0; i < count; i++) {
        if (!vterm_env_dist_ext_rewrite_term(rewrite, depth)) {
            return 0;
        }
    }
    return 1;
}

int
vterm_env_dist_ext_rewrite_atom_cache_ref(vterm_env_dist_ext_rewrite_t *rewrite)
{
    ErlNifBinary *atom_bin;
    uint8_t index;
    if (!vterm_env_dist_ext_rewrite_read_u8(rewrite, &index, false) || (int)index >= rewrite->atom_count) {
        return 0;
    }
    atom_bin = &rewrite->atom_bins[index];
    if (!rewrite->atom_bin_initialized[index]) {
        if (!enif_is_atom(rewrite->env, rewrite->atoms[index]) ||
            !enif_term_to_binary(rewrite->env, rewrite->atoms[index], atom_bin)) {
            return 0;
        }
        rewrite->atom_bin_initialized[index] = true;
        if (atom_bin->size < 2 || atom_bin->data[0] != VERSION_MAGIC) {
            return 0;
        }
    }
    return vterm_env_dist_ext_rewrite_append(rewrite, atom_bin->data + 1, atom_bin->size - 1);
}

int
vterm_env_dist_ext_rewrite_patch_u32(vterm_env_dist_ext_rewrite_t *rewrite, size_t offset, size_t value)
{
    if (value > UINT32_MAX) {
        return 0;
    }
    if (rewrite->dst != NULL) {
        uint32_t u32 = (uint32_t)value;
        if (offset > rewrite->dst_capacity || 4 > rewrite->dst_capacity - offset) {
            return 0;
        }
        rewrite->dst[offset + 0] = (uint8_t)(u32 >> 24);
        rewrite->dst[offset + 1] = (uint8_t)(u32 >> 16);
        rewrite->dst[offset + 2] = (uint8_t)(u32 >> 8);
        rewrite->dst[offset + 3] = (uint8_t)u32;
    }
    return 1;
}

int
vterm_env_dist_ext_rewrite_term(vterm_env_dist_ext_rewrite_t *rewrite, unsigned int depth)
{
    uint8_t tag;
    uint8_t u8;
    uint16_t u16;
    uint32_t u32;

    if (depth >= VTERM_ENV_DIST_EXT_MAX_DEPTH || !vterm_env_dist_ext_rewrite_read_u8(rewrite, &tag, false)) {
        return 0;
    }
    if (tag == ATOM_CACHE_REF) {
        return vterm_env_dist_ext_rewrite_atom_cache_ref(rewrite);
    }
    if (!vterm_env_dist_ext_rewrite_append(rewrite, &tag, 1)) {
        return 0;
    }

    switch (tag) {
    case SMALL_INTEGER_EXT:
        return vterm_env_dist_ext_rewrite_copy(rewrite, 1);
    case INTEGER_EXT:
        return vterm_env_dist_ext_rewrite_copy(rewrite, 4);
    case FLOAT_EXT:
        return vterm_env_dist_ext_rewrite_copy(rewrite, 31);
    case ATOM_EXT:
        [[fallthrough]];
    case ATOM_UTF8_EXT:
        return vterm_env_dist_ext_rewrite_read_u16(rewrite, &u16) && vterm_env_dist_ext_rewrite_copy(rewrite, (size_t)u16);
    case SMALL_ATOM_EXT:
        [[fallthrough]];
    case SMALL_ATOM_UTF8_EXT:
        return vterm_env_dist_ext_rewrite_read_u8(rewrite, &u8, true) && vterm_env_dist_ext_rewrite_copy(rewrite, (size_t)u8);
    case REFERENCE_EXT:
        return vterm_env_dist_ext_rewrite_term(rewrite, depth + 1) && vterm_env_dist_ext_rewrite_copy(rewrite, 5);
    case NEW_REFERENCE_EXT:
        return vterm_env_dist_ext_rewrite_read_u16(rewrite, &u16) && vterm_env_dist_ext_rewrite_term(rewrite, depth + 1) &&
               vterm_env_dist_ext_rewrite_copy(rewrite, 1) && (size_t)u16 <= SIZE_MAX / 4 &&
               vterm_env_dist_ext_rewrite_copy(rewrite, (size_t)u16 * 4);
    case NEWER_REFERENCE_EXT:
        return vterm_env_dist_ext_rewrite_read_u16(rewrite, &u16) && vterm_env_dist_ext_rewrite_term(rewrite, depth + 1) &&
               vterm_env_dist_ext_rewrite_copy(rewrite, 4) && (size_t)u16 <= SIZE_MAX / 4 &&
               vterm_env_dist_ext_rewrite_copy(rewrite, (size_t)u16 * 4);
    case PORT_EXT:
        return vterm_env_dist_ext_rewrite_term(rewrite, depth + 1) && vterm_env_dist_ext_rewrite_copy(rewrite, 5);
    case NEW_PORT_EXT:
        return vterm_env_dist_ext_rewrite_term(rewrite, depth + 1) && vterm_env_dist_ext_rewrite_copy(rewrite, 8);
    case V4_PORT_EXT:
        return vterm_env_dist_ext_rewrite_term(rewrite, depth + 1) && vterm_env_dist_ext_rewrite_copy(rewrite, 12);
    case NEW_FLOAT_EXT:
        return vterm_env_dist_ext_rewrite_copy(rewrite, 8);
    case PID_EXT:
        return vterm_env_dist_ext_rewrite_term(rewrite, depth + 1) && vterm_env_dist_ext_rewrite_copy(rewrite, 9);
    case NEW_PID_EXT:
        return vterm_env_dist_ext_rewrite_term(rewrite, depth + 1) && vterm_env_dist_ext_rewrite_copy(rewrite, 12);
    case SMALL_TUPLE_EXT:
        return vterm_env_dist_ext_rewrite_read_u8(rewrite, &u8, true) &&
               vterm_env_dist_ext_rewrite_terms(rewrite, (uint32_t)u8, depth + 1);
    case LARGE_TUPLE_EXT:
        return vterm_env_dist_ext_rewrite_read_u32(rewrite, &u32) && vterm_env_dist_ext_rewrite_terms(rewrite, u32, depth + 1);
    case NIL_EXT:
        return 1;
    case STRING_EXT:
        return vterm_env_dist_ext_rewrite_read_u16(rewrite, &u16) && vterm_env_dist_ext_rewrite_copy(rewrite, (size_t)u16);
    case LIST_EXT:
        return vterm_env_dist_ext_rewrite_read_u32(rewrite, &u32) && vterm_env_dist_ext_rewrite_terms(rewrite, u32, depth + 1) &&
               vterm_env_dist_ext_rewrite_term(rewrite, depth + 1);
    case BINARY_EXT:
        return vterm_env_dist_ext_rewrite_read_u32(rewrite, &u32) && vterm_env_dist_ext_rewrite_copy(rewrite, (size_t)u32);
    case BIT_BINARY_EXT:
        return vterm_env_dist_ext_rewrite_read_u32(rewrite, &u32) && vterm_env_dist_ext_rewrite_copy(rewrite, 1) &&
               vterm_env_dist_ext_rewrite_copy(rewrite, (size_t)u32);
    case SMALL_BIG_EXT:
        return vterm_env_dist_ext_rewrite_read_u8(rewrite, &u8, true) && vterm_env_dist_ext_rewrite_copy(rewrite, 1) &&
               vterm_env_dist_ext_rewrite_copy(rewrite, (size_t)u8);
    case LARGE_BIG_EXT:
        return vterm_env_dist_ext_rewrite_read_u32(rewrite, &u32) && vterm_env_dist_ext_rewrite_copy(rewrite, 1) &&
               vterm_env_dist_ext_rewrite_copy(rewrite, (size_t)u32);
    case NEW_FUN_EXT: {
        const uint8_t *source_size_start = rewrite->src;
        size_t output_size_start = rewrite->dst_offset;
        uint32_t source_size;
        uint32_t num_free;
        if (!vterm_env_dist_ext_rewrite_read_u32(rewrite, &source_size) || !vterm_env_dist_ext_rewrite_copy(rewrite, 21) ||
            !vterm_env_dist_ext_rewrite_read_u32(rewrite, &num_free) || !vterm_env_dist_ext_rewrite_term(rewrite, depth + 1) ||
            !vterm_env_dist_ext_rewrite_term(rewrite, depth + 1) || !vterm_env_dist_ext_rewrite_term(rewrite, depth + 1) ||
            !vterm_env_dist_ext_rewrite_term(rewrite, depth + 1) ||
            !vterm_env_dist_ext_rewrite_terms(rewrite, num_free, depth + 1) ||
            (size_t)(rewrite->src - source_size_start) != (size_t)source_size) {
            return 0;
        }
        return vterm_env_dist_ext_rewrite_patch_u32(rewrite, output_size_start, rewrite->dst_offset - output_size_start);
    }
    case EXPORT_EXT:
        return vterm_env_dist_ext_rewrite_terms(rewrite, 3, depth + 1);
    case MAP_EXT:
        if (!vterm_env_dist_ext_rewrite_read_u32(rewrite, &u32) || u32 > UINT32_MAX / 2) {
            return 0;
        }
        return vterm_env_dist_ext_rewrite_terms(rewrite, u32 * 2, depth + 1);
    case FUN_EXT:
        if (!vterm_env_dist_ext_rewrite_read_u32(rewrite, &u32) || !vterm_env_dist_ext_rewrite_terms(rewrite, 4, depth + 1)) {
            return 0;
        }
        return vterm_env_dist_ext_rewrite_terms(rewrite, u32, depth + 1);
    case COMPRESSED:
        return vterm_env_dist_ext_rewrite_copy(rewrite, (size_t)(rewrite->src_end - rewrite->src));
    default:
        return 0;
    }
}

void
vterm_env_dist_ext_rewrite_destroy(vterm_env_dist_ext_rewrite_t *rewrite)
{
    int i;
    for (i = 0; i < ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES; i++) {
        if (rewrite->atom_bin_initialized[i]) {
            (void)enif_release_binary(&rewrite->atom_bins[i]);
            rewrite->atom_bin_initialized[i] = false;
        }
    }
    return;
}

int
vterm_env_dist_ext_to_term(vterm_env_t *super, const uint8_t *buf, size_t len, ERL_NIF_TERM *termp)
{
    __vterm_env_t *vtenv = (void *)super;
    vterm_env_dist_ext_rewrite_t rewrite;
    ErlNifBinary expanded_bin;
    const ERL_NIF_TERM *atoms = NULL;
    ERL_NIF_TERM term = THE_NON_VALUE;
    size_t bytes_read;
    size_t expanded_len;
    int atom_count;
    int retval = 0;

    if (vtenv == NULL || buf == NULL || len < 2 || termp == NULL || buf[0] != VERSION_MAGIC ||
        !enif_get_tuple(vtenv->super.nif_env, vtenv->atoms, &atom_count, &atoms) || atom_count < 0 ||
        atom_count > ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES) {
        return 0;
    }
    if (vtenv->tmp_env == NULL) {
        vtenv->tmp_env = enif_alloc_env();
        if (vtenv->tmp_env == NULL) {
            return 0;
        }
    } else {
        (void)enif_clear_env(vtenv->tmp_env);
    }

    bytes_read = enif_binary_to_term(vtenv->tmp_env, buf, len, &term, 0);
    if (bytes_read == len) {
        *termp = term;
        return 1;
    }
    (void)enif_clear_env(vtenv->tmp_env);

    (void)memset(&rewrite, 0, sizeof(rewrite));
    rewrite.env = vtenv->tmp_env;
    rewrite.atoms = atoms;
    rewrite.atom_count = atom_count;
    rewrite.src = buf;
    rewrite.src_end = buf + len;

    if (!vterm_env_dist_ext_rewrite_copy(&rewrite, 1) || !vterm_env_dist_ext_rewrite_term(&rewrite, 0) ||
        rewrite.src != rewrite.src_end) {
        goto done;
    }
    expanded_len = rewrite.dst_offset;
    if (!enif_alloc_binary(expanded_len, &expanded_bin)) {
        goto done;
    }

    rewrite.src = buf;
    rewrite.dst = expanded_bin.data;
    rewrite.dst_capacity = expanded_bin.size;
    rewrite.dst_offset = 0;
    if (!vterm_env_dist_ext_rewrite_copy(&rewrite, 1) || !vterm_env_dist_ext_rewrite_term(&rewrite, 0) ||
        rewrite.src != rewrite.src_end || rewrite.dst_offset != expanded_len) {
        (void)enif_release_binary(&expanded_bin);
        goto done;
    }

    bytes_read = enif_binary_to_term(vtenv->tmp_env, expanded_bin.data, expanded_bin.size, &term, 0);
    (void)enif_release_binary(&expanded_bin);
    if (bytes_read != expanded_len) {
        goto done;
    }
    *termp = term;
    retval = 1;

done:
    (void)vterm_env_dist_ext_rewrite_destroy(&rewrite);
    return retval;
}

ERL_NIF_TERM
vterm_env_direct_dist_ext_to_term(ErlNifEnv *env, ERL_NIF_TERM atoms_tuple, ERL_NIF_TERM input_binary)
{
    edf_atom_translation_table_t attab;
    ErlNifBinary input_bin;
    vterm_env_t *vtenv = NULL;
    ERL_NIF_TERM output_term = THE_NON_VALUE;
    const ERL_NIF_TERM *atoms = NULL;
    int atom_count;
    int i;

    if (!enif_get_tuple(env, atoms_tuple, &atom_count, &atoms) || atom_count < 0 ||
        atom_count > ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES || !enif_inspect_binary(env, input_binary, &input_bin)) {
        return enif_make_badarg(env);
    }
    (void)edf_atom_translation_table_init(&attab);
    if (!edf_atom_translation_table_set_size(&attab, (size_t)atom_count)) {
        return enif_make_badarg(env);
    }
    for (i = 0; i < atom_count; i++) {
        if (!enif_is_atom(env, atoms[i]) || !edf_atom_translation_table_set_entry(&attab, 0, i, atoms[i], false)) {
            (void)edf_atom_translation_table_destroy(&attab);
            return enif_make_badarg(env);
        }
    }

    vtenv = vterm_env_alloc(&attab);
    if (vtenv == NULL || !vterm_env_dist_ext_to_term(vtenv, input_bin.data, input_bin.size, &output_term)) {
        (void)vterm_env_free(vtenv);
        (void)edf_atom_translation_table_destroy(&attab);
        return enif_make_badarg(env);
    }
    output_term = enif_make_copy(env, output_term);
    (void)vterm_env_free(vtenv);
    (void)edf_atom_translation_table_destroy(&attab);
    return output_term;
}
