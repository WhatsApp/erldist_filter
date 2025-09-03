/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "edf_world.h"
#include "../../primitive/unreachable.h"

static atomic_flag edf_world_initialized = ATOMIC_FLAG_INIT;
static edf_world_table_t edf_world_table_internal = {._link = {.next = NULL, .prev = NULL}};
edf_world_table_t *edf_world_table = &edf_world_table_internal;

static void edf_world_stats_init_empty(edf_world_stats_t *stats);

int
edf_world_load(ErlNifEnv *env)
{
    int retval = 0;

    if (!atomic_flag_test_and_set(&edf_world_initialized)) {
        if (!linklist_is_linked(&edf_world_table->_link)) {
            retval = enif_tsd_key_create("erldist_filter.world_table_tsd_key", &edf_world_table->key);
            if (retval != 0) {
                return retval;
            }
            (void)xnif_rwlock_create(&edf_world_table->rwlock, "erldist_filter.world_table_rwlock");
            (void)linklist_init_anchor(&edf_world_table->_link);
        }
    }

    return retval;
}

void
edf_world_unload(ErlNifEnv *env)
{
    (void)env;
    return;
}

edf_world_slot_t *
edf_world_get_slow(void)
{
    edf_world_slot_t *slot = NULL;
    slot = (void *)enif_tsd_get(edf_world_table->key);
    if (slot == NULL) {
        slot = enif_alloc(sizeof(edf_world_slot_t));
        if (slot == NULL) {
            unreachable();
            (void)perror("Too many processors or threads on this machine: OOM, unable to allocate edf_world_slot_t!");
            abort();
            return NULL;
        }
        slot->_link.prev = NULL;
        slot->_link.next = NULL;
        (void)edf_world_stats_init_empty(&slot->stats);
        (void)xnif_rwlock_write_lock(&edf_world_table->rwlock);
        (void)linklist_insert(&edf_world_table->_link, &slot->_link);
        (void)xnif_rwlock_write_unlock(&edf_world_table->rwlock);
        (void)enif_tsd_set(edf_world_table->key, (void *)slot);
        return slot;
    }
    return slot;
}

void
edf_world_stats_init_empty(edf_world_stats_t *stats)
{
    stats->channel.create = 0;
    stats->channel.destroy = 0;
    (void)edf_channel_stats_init_empty(&stats->channel.rx_stats);
    stats->memory.vec_own_bin_create = 0;
    stats->memory.vec_own_bin_create_capacity = 0;
    stats->memory.vec_own_bin_realloc = 0;
    stats->memory.vec_own_bin_realloc_capacity = 0;
    stats->memory.vec_own_bin_destroy = 0;
    stats->memory.vec_own_mem_create = 0;
    stats->memory.vec_own_mem_create_capacity = 0;
    stats->memory.vec_own_mem_realloc = 0;
    stats->memory.vec_own_mem_realloc_capacity = 0;
    stats->memory.vec_own_mem_destroy = 0;
    stats->memory.vec_ref_bin_create = 0;
    stats->memory.vec_ref_bin_destroy = 0;
    stats->memory.vec_ref_ioq_create = 0;
    stats->memory.vec_ref_ioq_destroy = 0;
    return;
}
