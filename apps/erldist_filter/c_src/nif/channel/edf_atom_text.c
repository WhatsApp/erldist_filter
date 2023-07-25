/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "edf_atom_text.h"

#include "../core/khashl.h"
#include "../core/rwlock.h"
#include "../vec.h"

#include <stdatomic.h>

/* Type Definitions */

typedef ERL_NIF_TERM edf_atom_text_hm_key_t;
typedef struct edf_atom_text_hm_val_s edf_atom_text_hm_val_t;

struct edf_atom_text_hm_val_s {
    ERL_NIF_TERM atom;
    uint8_t *name;
    size_t len;
    vec_t vec;
    atomic_size_t refc;
};

static khint_t edf_atom_text_hm_key_hash_fn(edf_atom_text_hm_key_t key);
static bool edf_atom_text_hm_key_hash_eq(edf_atom_text_hm_key_t k1, edf_atom_text_hm_key_t k2);
static void edf_atom_text_hm_val_destroy(edf_atom_text_hm_key_t *key, edf_atom_text_hm_val_t *val);

KHASHL_CMAP_INIT(KH_LOCAL, edf_atom_text_hm_t, edf_atom_text_hm, edf_atom_text_hm_key_t, edf_atom_text_hm_val_t,
                 edf_atom_text_hm_key_hash_fn, edf_atom_text_hm_key_hash_eq)

struct edf_atom_text_table_s {
    core_rwlock_t rwlock;
    edf_atom_text_hm_t *hm;
};

static edf_atom_text_table_t edf_atom_text_table_buf;
edf_atom_text_table_t *edf_atom_text_table = NULL;

/* Function Definitions */

int
edf_atom_text_table_init(void)
{
    if (edf_atom_text_table == NULL) {
        edf_atom_text_table = &edf_atom_text_table_buf;
        if (!core_rwlock_create(&edf_atom_text_table->rwlock, "erldist_filter.atom_text_table_rwlock")) {
            edf_atom_text_table = NULL;
            return 0;
        }
        edf_atom_text_table->hm = edf_atom_text_hm_init();
        if (edf_atom_text_table->hm == NULL) {
            (void)core_rwlock_destroy(&edf_atom_text_table->rwlock);
            edf_atom_text_table = NULL;
            return 0;
        }
    }
    return 1;
}

void
edf_atom_text_table_destroy(void)
{
    khint_t i;
    edf_atom_text_hm_key_t key;
    edf_atom_text_hm_val_t *val = NULL;

    (void)core_rwlock_write_lock(&edf_atom_text_table->rwlock);
    if (edf_atom_text_table->hm != NULL) {
        for (i = 0; i != kh_end(edf_atom_text_table->hm); i++) {
            if (!kh_exist(edf_atom_text_table->hm, i)) {
                continue;
            }
            key = kh_key(edf_atom_text_table->hm, i);
            val = &(kh_val(edf_atom_text_table->hm, i));
            (void)edf_atom_text_hm_val_destroy(&key, val);
        }
        (void)edf_atom_text_hm_destroy(edf_atom_text_table->hm);
        edf_atom_text_table->hm = NULL;
    }
    (void)core_rwlock_write_unlock(&edf_atom_text_table->rwlock);
    (void)core_rwlock_destroy(&edf_atom_text_table->rwlock);
    edf_atom_text_table = NULL;
    return;
}

int
edf_atom_text_table_size(void)
{
    int size;

    (void)core_rwlock_read_lock(&edf_atom_text_table->rwlock);
    size = (int)(kh_size(edf_atom_text_table->hm));
    (void)core_rwlock_read_unlock(&edf_atom_text_table->rwlock);

    return size;
}

ERL_NIF_TERM
edf_atom_text_table_list(ErlNifEnv *env)
{
    khint_t i;
    edf_atom_text_hm_key_t key;
    ERL_NIF_TERM list;

    list = enif_make_list(env, 0);
    (void)core_rwlock_read_lock(&edf_atom_text_table->rwlock);
    if (edf_atom_text_table->hm != NULL) {
        for (i = 0; i < kh_end(edf_atom_text_table->hm); i++) {
            if (!kh_exist(edf_atom_text_table->hm, i)) {
                continue;
            }
            key = kh_key(edf_atom_text_table->hm, i);
            list = enif_make_list_cell(env, key, list);
        }
    }
    (void)core_rwlock_read_unlock(&edf_atom_text_table->rwlock);
    return list;
}

int
edf_atom_text_put_and_keep(const uint8_t *name, signed int len, ErtsAtomEncoding enc, ERL_NIF_TERM *atomp)
{
    edf_atom_text_hm_key_t key;
    edf_atom_text_hm_val_t *val;
    khint_t slot;
    int absent = -1;

    key = erts_atom_put(name, len, enc, 0);
    if (key == THE_NON_VALUE) {
        return 0;
    }
    (void)core_rwlock_read_lock(&edf_atom_text_table->rwlock);
    slot = edf_atom_text_hm_get(edf_atom_text_table->hm, key);
    if (slot != kh_end(edf_atom_text_table->hm) && kh_exist(edf_atom_text_table->hm, slot)) {
        val = &(kh_val(edf_atom_text_table->hm, slot));
        if (atomp != NULL) {
            *atomp = val->atom;
        }
        (void)atomic_fetch_add_explicit(&val->refc, 1, memory_order_relaxed);
        (void)core_rwlock_read_unlock(&edf_atom_text_table->rwlock);
        return 1;
    }
    (void)core_rwlock_read_unlock(&edf_atom_text_table->rwlock);
    (void)core_rwlock_write_lock(&edf_atom_text_table->rwlock);
    slot = edf_atom_text_hm_put(edf_atom_text_table->hm, key, &absent);
    val = &(kh_val(edf_atom_text_table->hm, slot));
    if (absent) {
        val->atom = key;
        val->name = NULL;
        val->len = 0;
        (void)vec_init_free(&val->vec);
        if (!vec_create_owned(&val->vec, (size_t)len)) {
            val->atom = THE_NON_VALUE;
            (void)edf_atom_text_hm_val_destroy(&key, val);
            (void)edf_atom_text_hm_del(edf_atom_text_table->hm, slot);
            (void)core_rwlock_write_unlock(&edf_atom_text_table->rwlock);
            return 0;
        }
        (void)memcpy(vec_buf(&val->vec), name, (size_t)len);
        val->name = vec_buf(&val->vec);
        val->len = (size_t)len;
        (void)atomic_init(&val->refc, 1);
    }
    (void)atomic_fetch_add_explicit(&val->refc, 1, memory_order_relaxed);
    if (atomp != NULL) {
        *atomp = val->atom;
    }
    (void)core_rwlock_write_unlock(&edf_atom_text_table->rwlock);

    return 1;
}

int
edf_atom_text_get_length(ERL_NIF_TERM atom, ErtsAtomEncoding enc, size_t *lenp)
{
    edf_atom_text_hm_key_t key = atom;
    edf_atom_text_hm_val_t *val = NULL;
    khint_t slot;

    (void)enc;

    if (key == THE_NON_VALUE) {
        return 0;
    }
    (void)core_rwlock_read_lock(&edf_atom_text_table->rwlock);
    slot = edf_atom_text_hm_get(edf_atom_text_table->hm, key);
    if (slot == kh_end(edf_atom_text_table->hm) || !kh_exist(edf_atom_text_table->hm, slot)) {
        (void)core_rwlock_read_unlock(&edf_atom_text_table->rwlock);
        return 0;
    }
    val = &(kh_val(edf_atom_text_table->hm, slot));
    if (lenp != NULL) {
        *lenp = val->len;
    }
    (void)core_rwlock_read_unlock(&edf_atom_text_table->rwlock);

    return 1;
}

int
edf_atom_text_get_name(ERL_NIF_TERM atom, ErtsAtomEncoding enc, const uint8_t **namep, size_t *lenp)
{
    edf_atom_text_hm_key_t key = atom;
    edf_atom_text_hm_val_t *val = NULL;
    khint_t slot;

    (void)enc;

    if (key == THE_NON_VALUE) {
        return 0;
    }
    (void)core_rwlock_read_lock(&edf_atom_text_table->rwlock);
    slot = edf_atom_text_hm_get(edf_atom_text_table->hm, key);
    if (slot == kh_end(edf_atom_text_table->hm) || !kh_exist(edf_atom_text_table->hm, slot)) {
        (void)core_rwlock_read_unlock(&edf_atom_text_table->rwlock);
        return 0;
    }
    val = &(kh_val(edf_atom_text_table->hm, slot));
    if (namep != NULL) {
        *namep = val->name;
    }
    if (lenp != NULL) {
        *lenp = val->len;
    }
    (void)core_rwlock_read_unlock(&edf_atom_text_table->rwlock);

    return 1;
}

int
edf_atom_text_keep_slow(ERL_NIF_TERM atom)
{
    edf_atom_text_hm_key_t key = atom;
    edf_atom_text_hm_val_t *val = NULL;
    khint_t slot;

    if (key == THE_NON_VALUE) {
        return 0;
    }
    (void)core_rwlock_read_lock(&edf_atom_text_table->rwlock);
    slot = edf_atom_text_hm_get(edf_atom_text_table->hm, key);
    if (slot == kh_end(edf_atom_text_table->hm) || !kh_exist(edf_atom_text_table->hm, slot)) {
        (void)core_rwlock_read_unlock(&edf_atom_text_table->rwlock);
        return 0;
    }
    val = &(kh_val(edf_atom_text_table->hm, slot));
    (void)atomic_fetch_add_explicit(&val->refc, 1, memory_order_relaxed);
    (void)core_rwlock_read_unlock(&edf_atom_text_table->rwlock);
    return 1;
}

void
edf_atom_text_release_slow(ERL_NIF_TERM atom)
{
    edf_atom_text_hm_key_t key = atom;
    edf_atom_text_hm_val_t *val = NULL;
    khint_t slot;
    size_t refc;

    if (key == THE_NON_VALUE) {
        return;
    }
    (void)core_rwlock_read_lock(&edf_atom_text_table->rwlock);
    slot = edf_atom_text_hm_get(edf_atom_text_table->hm, key);
    if (slot == kh_end(edf_atom_text_table->hm) || !kh_exist(edf_atom_text_table->hm, slot)) {
        (void)core_rwlock_read_unlock(&edf_atom_text_table->rwlock);
        return;
    }
    val = &(kh_val(edf_atom_text_table->hm, slot));
    refc = atomic_fetch_sub_explicit(&val->refc, 1, memory_order_release);
    if (refc > 2) {
        (void)core_rwlock_read_unlock(&edf_atom_text_table->rwlock);
        return;
    }
    (void)core_rwlock_read_unlock(&edf_atom_text_table->rwlock);
    (void)core_rwlock_write_lock(&edf_atom_text_table->rwlock);
    slot = edf_atom_text_hm_get(edf_atom_text_table->hm, key);
    if (slot == kh_end(edf_atom_text_table->hm) || !kh_exist(edf_atom_text_table->hm, slot)) {
        (void)core_rwlock_write_unlock(&edf_atom_text_table->rwlock);
        return;
    }
    val = &(kh_val(edf_atom_text_table->hm, slot));
    refc = 1;
    if (!atomic_compare_exchange_strong(&val->refc, &refc, 0)) {
        (void)core_rwlock_write_unlock(&edf_atom_text_table->rwlock);
        return;
    }
    (void)edf_atom_text_hm_val_destroy(&key, val);
    (void)edf_atom_text_hm_del(edf_atom_text_table->hm, slot);
    (void)core_rwlock_write_unlock(&edf_atom_text_table->rwlock);
    return;
}

/* Static Function Definitions */

inline khint_t
edf_atom_text_hm_key_hash_fn(edf_atom_text_hm_key_t key)
{
    return kh_hash_uint64((khint64_t)key);
}

inline bool
edf_atom_text_hm_key_hash_eq(edf_atom_text_hm_key_t k1, edf_atom_text_hm_key_t k2)
{
    return (k1 == k2);
}

void
edf_atom_text_hm_val_destroy(edf_atom_text_hm_key_t *key, edf_atom_text_hm_val_t *val)
{
    (void)key;
    val->atom = THE_NON_VALUE;
    val->name = NULL;
    val->len = 0;
    (void)vec_destroy(&val->vec);
    return;
}
