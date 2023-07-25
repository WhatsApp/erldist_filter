/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "edf_channel.h"
#include "../world/edf_world.h"

#include "../erts/dist.h"

#include "../core/unreachable.h"
#include "../core/xnif_trace.h"

/* Global Variables */

ErlNifResourceType *edf_channel_resource_type = NULL;
static edf_channel_resource_table_t edf_channel_resource_table_internal = {._link = {.next = NULL, .prev = NULL}};
edf_channel_resource_table_t *edf_channel_resource_table = &edf_channel_resource_table_internal;
static edf_channel_index_table_t edf_channel_index_table_internal = {._link = {.next = NULL, .prev = NULL}};
edf_channel_index_table_t *edf_channel_index_table = &edf_channel_index_table_internal;

/* Static Declarations */

static void edf_channel_resource_type_dtor(ErlNifEnv *env, void *obj);
static void edf_channel_resource_type_down(ErlNifEnv *env, void *obj, ErlNifPid *pid, ErlNifMonitor *mon);

static int edf_channel_create(ErlNifEnv *env, size_t packet_size, ERL_NIF_TERM sysname, uint32_t creation, uint32_t connection_id,
                              uint64_t dflags, edf_channel_resource_t *resource, edf_channel_t **channelp,
                              ERL_NIF_TERM *error_term);
static void edf_channel_stats_init_empty(edf_channel_stats_t *stats);
static void edf_channel_stats_dop_init_empty(edf_channel_stats_dop_t *dop);

/* Function Definitions */

int
edf_channel_load(ErlNifEnv *env)
{
    int retval = 0;

    static ErlNifResourceTypeInit edf_channel_resource_type_init = {
        .dtor = edf_channel_resource_type_dtor,
        .stop = NULL,
        .down = edf_channel_resource_type_down,
        .members = 4,
        .dyncall = NULL,
    };
    edf_channel_resource_type = enif_init_resource_type(env, "erldist_filter_nif_channel", &edf_channel_resource_type_init,
                                                        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    if (edf_channel_resource_type == NULL) {
        retval = -1;
        return retval;
    }

    if (!linklist_is_linked(&edf_channel_resource_table->_link)) {
        (void)core_mutex_create(&edf_channel_resource_table->mutex, "erldist_filter.channel_resource_table_mutex");
        (void)linklist_init_anchor(&edf_channel_resource_table->_link);
    }

    if (!linklist_is_linked(&edf_channel_index_table->_link)) {
        retval = enif_tsd_key_create("erldist_filter.channel_index_table_tsd_key", &edf_channel_index_table->key);
        if (retval != 0) {
            return retval;
        }
        (void)core_rwlock_create(&edf_channel_index_table->rwlock, "erldist_filter.channel_index_table_rwlock");
        (void)linklist_init_anchor(&edf_channel_index_table->_link);
    }

    return retval;
}

void
edf_channel_unload(ErlNifEnv *env)
{
    (void)env;
    edf_channel_resource_type = NULL;
    return;
}

ERL_NIF_TERM
edf_channel_resource_open(ErlNifEnv *env, size_t packet_size, ERL_NIF_TERM sysname, uint32_t creation, uint32_t connection_id,
                          uint64_t dflags, edf_channel_resource_t **resourcep, edf_channel_t **channelp)
{
    edf_channel_resource_t *resource = NULL;
    edf_channel_t *channel = NULL;
    ERL_NIF_TERM out_term;

    resource = enif_alloc_resource(edf_channel_resource_type, sizeof(edf_channel_resource_t));
    if (resource == NULL) {
        return EXCP_ERROR(env, "Can't allocate edf_channel_resource_t");
    }

    resource->_link.next = NULL;
    resource->_link.prev = NULL;
    if (!core_rwlock_create(&resource->rwlock, "erldist_filter.channel_resource_rwlock")) {
        (void)enif_release_resource((void *)resource);
        return EXCP_ERROR(env, "Call to core_rwlock_create() failed: Can't allocate core_rwlock_t");
    }
    resource->inner = NULL;

    if (!edf_channel_create(env, packet_size, sysname, creation, connection_id, dflags, resource, &channel, &out_term)) {
        (void)enif_release_resource((void *)resource);
        return EXCP_ERROR(env, "Call to edf_channel_create() failed: Can't allocate edf_channel_t");
    }

    resource->inner = channel;

    out_term = enif_make_resource(env, (void *)resource);
    (void)core_mutex_lock(&edf_channel_resource_table->mutex);
    (void)linklist_insert(&edf_channel_resource_table->_link, &resource->_link);
    (void)core_mutex_unlock(&edf_channel_resource_table->mutex);
    // Don't release the resource here, so a reference is kept on the root table.
    // (void)enif_release_resource((void *)resource);

    (void)atomic_fetch_add(&edf_world->channels_created, 1);

    if (resourcep != NULL) {
        *resourcep = resource;
    }
    if (channelp != NULL) {
        *channelp = channel;
    }

    return out_term;
}

void
edf_channel_resource_type_dtor(ErlNifEnv *env, void *obj)
{
    edf_channel_resource_t *resource = (void *)obj;
    edf_channel_t *channel = NULL;
    XNIF_TRACE_F("[channel] dtor callback\n");
    (void)core_rwlock_write_lock(&(resource->rwlock));
    channel = resource->inner;
    if (channel != NULL) {
        resource->inner = NULL;
        channel->resource = NULL;
        (void)edf_channel_destroy(env, resource, channel);
        channel = NULL;
    }
    (void)core_mutex_lock(&edf_channel_resource_table->mutex);
    if (linklist_is_linked(&resource->_link)) {
        (void)linklist_unlink(&resource->_link);
    }
    (void)core_mutex_unlock(&edf_channel_resource_table->mutex);
    (void)core_rwlock_write_unlock(&(resource->rwlock));
    (void)core_rwlock_destroy(&(resource->rwlock));
    return;
}

void
edf_channel_resource_type_down(ErlNifEnv *env, void *obj, ErlNifPid *pid, ErlNifMonitor *mon)
{
    edf_channel_resource_t *resource = (void *)obj;
    edf_channel_t *channel = NULL;
    XNIF_TRACE_F("[channel] down callback\n");
    (void)core_rwlock_write_lock(&(resource->rwlock));
    channel = resource->inner;
    if (channel == NULL) {
        (void)core_rwlock_write_unlock(&(resource->rwlock));
        return;
    }
    if (enif_compare_monitors(&channel->owner.mon, mon) == 0) {
        // Owner is down, close channel.
        (void)edf_mpid_set_undefined(&channel->owner);
        resource->inner = NULL;
        channel->resource = NULL;
        (void)edf_channel_destroy(env, resource, channel);
        (void)core_rwlock_write_unlock(&(resource->rwlock));
        return;
    } else if (enif_compare_monitors(&channel->trace.mon, mon) == 0) {
        // Trace is down, disable tracing.
        (void)edf_mpid_set_undefined(&channel->trace);
    } else {
        // Old monitor down event, ignore.
    }
    (void)core_rwlock_write_unlock(&(resource->rwlock));
    return;
}

int
edf_channel_create(ErlNifEnv *env, size_t packet_size, ERL_NIF_TERM sysname, uint32_t creation, uint32_t connection_id,
                   uint64_t dflags, edf_channel_resource_t *resource, edf_channel_t **channelp, ERL_NIF_TERM *error_term)
{
    edf_channel_t *channel = NULL;
    int retval;

    channel = (edf_channel_t *)enif_alloc(sizeof(edf_channel_t));
    if (channel == NULL) {
        return 0;
    }
    channel->resource = resource;
    (void)edf_mpid_set_undefined(&channel->owner);
    (void)edf_mpid_set_undefined(&channel->trace);
    channel->sysname = sysname;
    channel->creation = creation;
    channel->connection_id = connection_id;
    channel->dflags = dflags;
    channel->rx.packet_size = packet_size;
    channel->rx.state = EDF_CHANNEL_RX_STATE_PACKET_HEADER;
    (void)ioq_init_free(&channel->rx.ioq);
    (void)vec_init_free(&channel->rx.vec);
    channel->rx.cache = NULL;
    channel->rx.sequences = NULL;
    (void)edf_channel_stats_init_empty(&channel->rx.stats);

    if (!(packet_size == 0 || packet_size == 1 || packet_size == 2 || packet_size == 4 || packet_size == 8)) {
        (void)edf_channel_destroy(env, (void *)resource, channel);
        *error_term = EXCP_ERROR(env, "PacketSize must be an unsigned integer and one of {0, 1, 2, 4, 8}");
        return 0;
    }
    if (!enif_is_atom(env, sysname)) {
        (void)edf_channel_destroy(env, (void *)resource, channel);
        *error_term = EXCP_ERROR(env, "Sysname must be an atom");
        return 0;
    }
    if (!(dflags & DFLAG_DIST_MANDATORY)) {
        (void)edf_channel_destroy(env, (void *)resource, channel);
        *error_term = EXCP_BADARG(env, "DistributionFlags must have DFLAG_DIST_MANDATORY flags set");
        return 0;
    }
    if ((dflags & (DFLAG_DIST_HDR_ATOM_CACHE | DFLAG_FRAGMENTS)) == 0) {
        channel->rx.cache = NULL;
    } else {
        channel->rx.cache = (void *)enif_alloc(sizeof(edf_atom_cache_t));
        if (channel->rx.cache == NULL) {
            (void)edf_channel_destroy(env, (void *)resource, channel);
            *error_term = EXCP_BADARG(env, "Call to enif_alloc() failed: unable to allocate edf_atom_cache_t");
            return 0;
        }
        (void)edf_atom_cache_init(channel->rx.cache);
    }

    retval = edf_mpid_monitor_self(env, (void *)resource, &channel->owner);
    if (retval < 0) {
        (void)edf_channel_destroy(env, (void *)resource, channel);
        *error_term = EXCP_ERROR(env, "Call to enif_monitor_process() failed: no `down' callback provided");
        return 0;
    } else if (retval > 0) {
        (void)edf_channel_destroy(env, (void *)resource, channel);
        *error_term = EXCP_ERROR(env, "Call to enif_monitor_process() failed: target process is no longer alive");
        return 0;
    }

    if (!ioq_create(&channel->rx.ioq)) {
        (void)edf_channel_destroy(env, (void *)resource, channel);
        *error_term = EXCP_ERROR(env, "Call to enif_ioq_create() failed: unable to allocate ErlNifIOQueue");
        return 0;
    }

    *channelp = channel;
    return 1;
}

void
edf_channel_destroy(ErlNifEnv *env, edf_channel_resource_t *resource, edf_channel_t *channel)
{
    XNIF_TRACE_F("%s:%d edf_channel_destroy()\n", __FILE__, __LINE__);
    if (channel == NULL) {
        resource->inner = NULL;
        return;
    }
    channel->resource = NULL;
    (void)edf_mpid_demonitor_process(env, (void *)resource, &channel->owner);
    (void)edf_mpid_demonitor_process(env, (void *)resource, &channel->trace);
    channel->sysname = ATOM(undefined);
    channel->creation = 0;
    channel->connection_id = 0;
    channel->dflags = 0;
    if (channel->rx.cache != NULL) {
        (void)edf_atom_cache_destroy(channel->rx.cache);
        (void)enif_free((void *)channel->rx.cache);
        channel->rx.cache = NULL;
    }
    if (channel->rx.sequences != NULL) {
        (void)edf_external_sequence_destroy_all(&(channel->rx.sequences));
        channel->rx.sequences = NULL;
    }
    (void)edf_channel_stats_init_empty(&channel->rx.stats);
    if (!vec_is_free(&channel->rx.vec)) {
        (void)vec_destroy(&channel->rx.vec);
    }
    if (!ioq_is_free(&channel->rx.ioq)) {
        (void)ioq_destroy(&channel->rx.ioq);
    }
    resource->inner = NULL;
    (void)enif_free((void *)channel);
    (void)atomic_fetch_add(&edf_world->channels_destroyed, 1);
    return;
}

edf_channel_index_slot_t *
edf_channel_index_get_slow(void)
{
    edf_channel_index_slot_t *slot = NULL;
    slot = (void *)enif_tsd_get(edf_channel_index_table->key);
    if (slot == NULL) {
        slot = enif_alloc(sizeof(edf_channel_index_slot_t));
        if (slot == NULL) {
            unreachable();
            (void)perror("Too many processors or threads on this machine: OOM, unable to allocate edf_channel_index_slot_t!");
            abort();
            return NULL;
        }
        slot->_link.prev = NULL;
        slot->_link.next = NULL;
        (void)edf_channel_stats_init_empty(&slot->rx_stats);
        (void)core_rwlock_write_lock(&edf_channel_index_table->rwlock);
        (void)linklist_insert(&edf_channel_index_table->_link, &slot->_link);
        (void)core_rwlock_write_unlock(&edf_channel_index_table->rwlock);
        (void)enif_tsd_set(edf_channel_index_table->key, (void *)slot);
        return slot;
    }
    return slot;
}

void
edf_channel_stats_init_empty(edf_channel_stats_t *stats)
{
    stats->packet_count = 0;
    stats->emit_count = 0;
    stats->drop_count = 0;
    stats->dist_header_count = 0;
    stats->dist_frag_header_count = 0;
    stats->dist_frag_cont_count = 0;
    stats->dist_pass_through_count = 0;
    stats->atom_cache_read_count = 0;
    stats->atom_cache_write_count = 0;
    stats->atom_cache_overwrite_count = 0;
    stats->rewrite_fragment_header_count = 0;
    stats->rollback_atom_cache_count = 0;
    stats->compact_external_count = 0;
    stats->compact_fragment_count = 0;
    stats->control_has_export_ext = 0;
    stats->control_has_new_fun_ext = 0;
    stats->payload_has_export_ext = 0;
    stats->payload_has_new_fun_ext = 0;
    (void)edf_channel_stats_dop_init_empty(&stats->dop_link);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_send);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_exit);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_unlink);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_reg_send);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_group_leader);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_exit2);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_send_tt);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_exit_tt);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_reg_send_tt);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_exit2_tt);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_monitor_p);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_demonitor_p);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_monitor_p_exit);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_send_sender);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_send_sender_tt);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_payload_exit);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_payload_exit_tt);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_payload_exit2);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_payload_exit2_tt);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_payload_monitor_p_exit);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_spawn_request);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_spawn_request_tt);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_spawn_reply);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_spawn_reply_tt);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_alias_send);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_alias_send_tt);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_unlink_id);
    (void)edf_channel_stats_dop_init_empty(&stats->dop_unlink_id_ack);
    return;
}

inline void
edf_channel_stats_dop_init_empty(edf_channel_stats_dop_t *dop)
{
    dop->seen = 0;
    dop->emit = 0;
    dop->drop = 0;
    return;
}
