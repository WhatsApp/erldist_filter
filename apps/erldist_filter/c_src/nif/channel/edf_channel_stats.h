/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_CHANNEL_STATS_H
#define EDF_CHANNEL_STATS_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../edf_common.h"

/* Macro Definitions */

/* Type Definitions */

typedef struct edf_channel_stats_s edf_channel_stats_t;
typedef struct edf_channel_stats_dop_s edf_channel_stats_dop_t;

struct edf_channel_stats_dop_s {
    uint64_t seen;
    uint64_t emit;
    uint64_t drop;
};

struct edf_channel_stats_s {
    uint64_t packet_count;
    uint64_t emit_count;
    uint64_t drop_count;
    uint64_t dist_header_count;
    uint64_t dist_frag_header_count;
    uint64_t dist_frag_cont_count;
    uint64_t dist_pass_through_count;
    uint64_t atom_cache_read_count;
    uint64_t atom_cache_write_count;
    uint64_t atom_cache_overwrite_count;
    uint64_t rewrite_fragment_header_count;
    uint64_t rollback_atom_cache_count;
    uint64_t compact_external_count;
    uint64_t compact_fragment_count;
    uint64_t control_has_export_ext;
    uint64_t control_has_new_fun_ext;
    uint64_t payload_has_export_ext;
    uint64_t payload_has_new_fun_ext;
    uint64_t fastpath;
    uint64_t slowpath;
    edf_channel_stats_dop_t dop_link;
    edf_channel_stats_dop_t dop_send;
    edf_channel_stats_dop_t dop_exit;
    edf_channel_stats_dop_t dop_unlink;
    edf_channel_stats_dop_t dop_reg_send;
    edf_channel_stats_dop_t dop_group_leader;
    edf_channel_stats_dop_t dop_exit2;
    edf_channel_stats_dop_t dop_send_tt;
    edf_channel_stats_dop_t dop_exit_tt;
    edf_channel_stats_dop_t dop_reg_send_tt;
    edf_channel_stats_dop_t dop_exit2_tt;
    edf_channel_stats_dop_t dop_monitor_p;
    edf_channel_stats_dop_t dop_demonitor_p;
    edf_channel_stats_dop_t dop_monitor_p_exit;
    edf_channel_stats_dop_t dop_send_sender;
    edf_channel_stats_dop_t dop_send_sender_tt;
    edf_channel_stats_dop_t dop_payload_exit;
    edf_channel_stats_dop_t dop_payload_exit_tt;
    edf_channel_stats_dop_t dop_payload_exit2;
    edf_channel_stats_dop_t dop_payload_exit2_tt;
    edf_channel_stats_dop_t dop_payload_monitor_p_exit;
    edf_channel_stats_dop_t dop_spawn_request;
    edf_channel_stats_dop_t dop_spawn_request_tt;
    edf_channel_stats_dop_t dop_spawn_reply;
    edf_channel_stats_dop_t dop_spawn_reply_tt;
    edf_channel_stats_dop_t dop_alias_send;
    edf_channel_stats_dop_t dop_alias_send_tt;
    edf_channel_stats_dop_t dop_unlink_id;
    edf_channel_stats_dop_t dop_unlink_id_ack;
};

/* Function Declarations */

extern void edf_channel_stats_init_empty(edf_channel_stats_t *stats);

#ifdef __cplusplus
}
#endif

#endif
