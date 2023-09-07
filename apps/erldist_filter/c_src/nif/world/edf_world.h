/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_WORLD_H
#define EDF_WORLD_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../edf_common.h"
#include "../core/linklist.h"
#include "../core/rwlock.h"
#include "../channel/edf_channel_stats.h"

/* Macro Definitions */

#define WORLD_STATS_COUNT(group, field, inc)                                                                                       \
    do {                                                                                                                           \
        (edf_world_get())->stats.group.field += inc;                                                                               \
    } while (0)

/* Type Definitions */

typedef struct edf_world_stats_s edf_world_stats_t;
typedef struct edf_world_stat_group_channel_s edf_world_stat_group_channel_t;
typedef struct edf_world_stat_group_memory_s edf_world_stat_group_memory_t;
typedef struct edf_world_slot_s edf_world_slot_t;
typedef struct edf_world_table_s edf_world_table_t;

struct edf_world_stat_group_channel_s {
    uint64_t create;
    uint64_t destroy;
    edf_channel_stats_t rx_stats;
};

struct edf_world_stat_group_memory_s {
    uint64_t vec_own_bin_create;
    uint64_t vec_own_bin_create_capacity;
    uint64_t vec_own_bin_realloc;
    uint64_t vec_own_bin_realloc_capacity;
    uint64_t vec_own_bin_destroy;
    uint64_t vec_own_mem_create;
    uint64_t vec_own_mem_create_capacity;
    uint64_t vec_own_mem_realloc;
    uint64_t vec_own_mem_realloc_capacity;
    uint64_t vec_own_mem_destroy;
    uint64_t vec_ref_bin_create;
    uint64_t vec_ref_bin_destroy;
    uint64_t vec_ref_ioq_create;
    uint64_t vec_ref_ioq_destroy;
};

struct edf_world_stats_s {
    edf_world_stat_group_channel_t channel;
    edf_world_stat_group_memory_t memory;
};

struct edf_world_slot_s {
    linklist_t _link;
    edf_world_stats_t stats;
};

struct edf_world_table_s {
    linklist_t _link;
    ErlNifTSDKey key;
    core_rwlock_t rwlock;
};

/* Global Declarations */

extern edf_world_table_t *edf_world_table;

/* Function Declarations */

extern int edf_world_load(ErlNifEnv *env);
extern void edf_world_unload(ErlNifEnv *env);
static edf_world_slot_t *edf_world_get(void);
extern edf_world_slot_t *edf_world_get_slow(void);

/* Inline Function Definitions */

inline edf_world_slot_t *
edf_world_get(void)
{
    edf_world_slot_t *slot = (void *)enif_tsd_get(edf_world_table->key);
    if (slot == NULL) {
        return edf_world_get_slow();
    }
    return slot;
}

#ifdef __cplusplus
}
#endif

#endif
