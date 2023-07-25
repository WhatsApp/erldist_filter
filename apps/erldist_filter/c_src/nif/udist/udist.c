/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "udist.h"
#include "../channel/edf_channel.h"

int
udist_get_channel_stats_dop(udist_t *up, edf_channel_stats_t *stats, edf_channel_stats_dop_t **statsdopp)
{
    if (up == NULL || stats == NULL || statsdopp == NULL) {
        return 0;
    }
    switch (up->info.dop) {
    case DOP_LINK:
        *statsdopp = &stats->dop_link;
        break;
    case DOP_SEND:
        *statsdopp = &stats->dop_send;
        break;
    case DOP_EXIT:
        *statsdopp = &stats->dop_exit;
        break;
    case DOP_UNLINK:
        *statsdopp = &stats->dop_unlink;
        break;
    case DOP_REG_SEND:
        *statsdopp = &stats->dop_reg_send;
        break;
    case DOP_GROUP_LEADER:
        *statsdopp = &stats->dop_group_leader;
        break;
    case DOP_EXIT2:
        *statsdopp = &stats->dop_exit2;
        break;
    case DOP_SEND_TT:
        *statsdopp = &stats->dop_send_tt;
        break;
    case DOP_EXIT_TT:
        *statsdopp = &stats->dop_exit_tt;
        break;
    case DOP_REG_SEND_TT:
        *statsdopp = &stats->dop_reg_send_tt;
        break;
    case DOP_EXIT2_TT:
        *statsdopp = &stats->dop_exit2_tt;
        break;
    case DOP_MONITOR_P:
        *statsdopp = &stats->dop_monitor_p;
        break;
    case DOP_DEMONITOR_P:
        *statsdopp = &stats->dop_demonitor_p;
        break;
    case DOP_MONITOR_P_EXIT:
        *statsdopp = &stats->dop_monitor_p_exit;
        break;
    case DOP_SEND_SENDER:
        *statsdopp = &stats->dop_send_sender;
        break;
    case DOP_SEND_SENDER_TT:
        *statsdopp = &stats->dop_send_sender_tt;
        break;
    case DOP_PAYLOAD_EXIT:
        *statsdopp = &stats->dop_payload_exit;
        break;
    case DOP_PAYLOAD_EXIT_TT:
        *statsdopp = &stats->dop_payload_exit_tt;
        break;
    case DOP_PAYLOAD_EXIT2:
        *statsdopp = &stats->dop_payload_exit2;
        break;
    case DOP_PAYLOAD_EXIT2_TT:
        *statsdopp = &stats->dop_payload_exit2_tt;
        break;
    case DOP_PAYLOAD_MONITOR_P_EXIT:
        *statsdopp = &stats->dop_payload_monitor_p_exit;
        break;
    case DOP_SPAWN_REQUEST:
        *statsdopp = &stats->dop_spawn_request;
        break;
    case DOP_SPAWN_REQUEST_TT:
        *statsdopp = &stats->dop_spawn_request_tt;
        break;
    case DOP_SPAWN_REPLY:
        *statsdopp = &stats->dop_spawn_reply;
        break;
    case DOP_SPAWN_REPLY_TT:
        *statsdopp = &stats->dop_spawn_reply_tt;
        break;
    case DOP_ALIAS_SEND:
        *statsdopp = &stats->dop_alias_send;
        break;
    case DOP_ALIAS_SEND_TT:
        *statsdopp = &stats->dop_alias_send_tt;
        break;
    case DOP_UNLINK_ID:
        *statsdopp = &stats->dop_unlink_id;
        break;
    case DOP_UNLINK_ID_ACK:
        *statsdopp = &stats->dop_unlink_id_ack;
        break;
    default:
        return 0;
    }

    return 1;
}

int
udist_get_dop_string(const udist_t *up, const char **name)
{
    switch (up->info.dop) {
    case DOP_UNKNOWN:
        *name = "DOP_UNKNOWN";
        break;
    case DOP_LINK:
        *name = "DOP_LINK";
        break;
    case DOP_SEND:
        *name = "DOP_SEND";
        break;
    case DOP_EXIT:
        *name = "DOP_EXIT";
        break;
    case DOP_UNLINK:
        *name = "DOP_UNLINK";
        break;
    case DOP_REG_SEND:
        *name = "DOP_REG_SEND";
        break;
    case DOP_GROUP_LEADER:
        *name = "DOP_GROUP_LEADER";
        break;
    case DOP_EXIT2:
        *name = "DOP_EXIT2";
        break;
    case DOP_SEND_TT:
        *name = "DOP_SEND_TT";
        break;
    case DOP_EXIT_TT:
        *name = "DOP_EXIT_TT";
        break;
    case DOP_REG_SEND_TT:
        *name = "DOP_REG_SEND_TT";
        break;
    case DOP_EXIT2_TT:
        *name = "DOP_EXIT2_TT";
        break;
    case DOP_MONITOR_P:
        *name = "DOP_MONITOR_P";
        break;
    case DOP_DEMONITOR_P:
        *name = "DOP_DEMONITOR_P";
        break;
    case DOP_MONITOR_P_EXIT:
        *name = "DOP_MONITOR_P_EXIT";
        break;
    case DOP_SEND_SENDER:
        *name = "DOP_SEND_SENDER";
        break;
    case DOP_SEND_SENDER_TT:
        *name = "DOP_SEND_SENDER_TT";
        break;
    case DOP_PAYLOAD_EXIT:
        *name = "DOP_PAYLOAD_EXIT";
        break;
    case DOP_PAYLOAD_EXIT_TT:
        *name = "DOP_PAYLOAD_EXIT_TT";
        break;
    case DOP_PAYLOAD_EXIT2:
        *name = "DOP_PAYLOAD_EXIT2";
        break;
    case DOP_PAYLOAD_EXIT2_TT:
        *name = "DOP_PAYLOAD_EXIT2_TT";
        break;
    case DOP_PAYLOAD_MONITOR_P_EXIT:
        *name = "DOP_PAYLOAD_MONITOR_P_EXIT";
        break;
    case DOP_SPAWN_REQUEST:
        *name = "DOP_SPAWN_REQUEST";
        break;
    case DOP_SPAWN_REQUEST_TT:
        *name = "DOP_SPAWN_REQUEST_TT";
        break;
    case DOP_SPAWN_REPLY:
        *name = "DOP_SPAWN_REPLY";
        break;
    case DOP_SPAWN_REPLY_TT:
        *name = "DOP_SPAWN_REPLY_TT";
        break;
    case DOP_ALIAS_SEND:
        *name = "DOP_ALIAS_SEND";
        break;
    case DOP_ALIAS_SEND_TT:
        *name = "DOP_ALIAS_SEND_TT";
        break;
    case DOP_UNLINK_ID:
        *name = "DOP_UNLINK_ID";
        break;
    case DOP_UNLINK_ID_ACK:
        *name = "DOP_UNLINK_ID_ACK";
        break;
    default:
        return 0;
    }
    return 1;
}
