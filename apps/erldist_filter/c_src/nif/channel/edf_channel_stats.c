/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "edf_channel_stats.h"

static void edf_channel_stats_dop_init_empty(edf_channel_stats_dop_t *dop);

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
    stats->fastpath = 0;
    stats->slowpath = 0;
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
