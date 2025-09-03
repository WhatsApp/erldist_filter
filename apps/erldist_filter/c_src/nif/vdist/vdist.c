/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "vdist.h"

int
vdist_dop_from_vterm(vterm_env_t *vtenv, vdist_dop_t *dop, vterm_t *vtp)
{
    uint32_t tuple_arity;
    vterm_t *tuple_elements = NULL;
    uint32_t sub_tuple_arity;
    vterm_t *sub_tuple_elements = NULL;
    int32_t dop_tag;
    if (dop == NULL || *vtp == NULL || !vdist_dop_is_free(dop)) {
        return 0;
    }
    if (!vterm_is_tuple(vtenv, vtp) || !vterm_get_tuple(vtenv, vtp, &tuple_arity, &tuple_elements) || tuple_arity < 1) {
        return 0;
    }
    if (!vterm_get_fixed_integer(vtenv, &tuple_elements[0], &dop_tag)) {
        return 0;
    }
    switch (dop_tag) {
    case DOP_LINK: {
        if (tuple_arity != 3) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_LINK;
        if (!vterm_is_pid(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.link.from_pid = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.link.to_pid = tuple_elements[2];
        break;
    }
    case DOP_SEND: {
        if (tuple_arity != 3) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_SEND;
        dop->data.send.unused = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.send.to_pid = tuple_elements[2];
        break;
    }
    case DOP_EXIT: {
        if (tuple_arity != 4) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_EXIT;
        if (!vterm_is_pid(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.exit.from_pid = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.exit.to_pid = tuple_elements[2];
        dop->data.exit.reason = tuple_elements[3];
        break;
    }
    case DOP_UNLINK: {
        if (tuple_arity != 3) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_UNLINK;
        if (!vterm_is_pid(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.unlink.from_pid = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.unlink.to_pid = tuple_elements[2];
        break;
    }
    case DOP_REG_SEND: {
        if (tuple_arity != 4) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_REG_SEND;
        if (!vterm_is_pid(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.reg_send.from_pid = tuple_elements[1];
        dop->data.reg_send.unused = tuple_elements[2];
        if (!vterm_is_atom(vtenv, &tuple_elements[3])) {
            return 0;
        }
        dop->data.reg_send.to_name = tuple_elements[3];
        break;
    }
    case DOP_GROUP_LEADER: {
        if (tuple_arity != 3) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_GROUP_LEADER;
        if (!vterm_is_pid(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.group_leader.from_pid = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.group_leader.to_pid = tuple_elements[2];
        break;
    }
    case DOP_EXIT2: {
        if (tuple_arity != 4) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_EXIT2;
        if (!vterm_is_pid(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.exit2.from_pid = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.exit2.to_pid = tuple_elements[2];
        dop->data.exit2.reason = tuple_elements[3];
        break;
    }
    case DOP_SEND_TT: {
        if (tuple_arity != 4) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_SEND_TT;
        dop->data.send_tt.unused = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.send_tt.to_pid = tuple_elements[2];
        dop->data.send_tt.trace_token = tuple_elements[3];
        break;
    }
    case DOP_EXIT_TT: {
        if (tuple_arity != 5) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_EXIT_TT;
        if (!vterm_is_pid(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.exit_tt.from_pid = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.exit_tt.to_pid = tuple_elements[2];
        dop->data.exit_tt.trace_token = tuple_elements[3];
        dop->data.exit_tt.reason = tuple_elements[4];
        break;
    }
    case DOP_REG_SEND_TT: {
        if (tuple_arity != 5) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_REG_SEND_TT;
        if (!vterm_is_pid(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.reg_send_tt.from_pid = tuple_elements[1];
        dop->data.reg_send_tt.unused = tuple_elements[2];
        if (!vterm_is_atom(vtenv, &tuple_elements[3])) {
            return 0;
        }
        dop->data.reg_send_tt.to_name = tuple_elements[3];
        dop->data.reg_send_tt.trace_token = tuple_elements[4];
        break;
    }
    case DOP_EXIT2_TT: {
        if (tuple_arity != 5) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_EXIT2_TT;
        if (!vterm_is_pid(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.exit2_tt.from_pid = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.exit2_tt.to_pid = tuple_elements[2];
        dop->data.exit2_tt.trace_token = tuple_elements[3];
        dop->data.exit2_tt.reason = tuple_elements[4];
        break;
    }
    case DOP_MONITOR_P: {
        if (tuple_arity != 4) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_MONITOR_P;
        if (!vterm_is_pid(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.monitor_p.from_pid = tuple_elements[1];
        if (!vterm_is_atom(vtenv, &tuple_elements[2]) && !vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.monitor_p.to_proc = tuple_elements[2];
        if (!vterm_is_reference(vtenv, &tuple_elements[3])) {
            return 0;
        }
        dop->data.monitor_p.ref = tuple_elements[3];
        break;
    }
    case DOP_DEMONITOR_P: {
        if (tuple_arity != 4) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_DEMONITOR_P;
        if (!vterm_is_pid(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.demonitor_p.from_pid = tuple_elements[1];
        if (!vterm_is_atom(vtenv, &tuple_elements[2]) && !vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.demonitor_p.to_proc = tuple_elements[2];
        if (!vterm_is_reference(vtenv, &tuple_elements[3])) {
            return 0;
        }
        dop->data.demonitor_p.ref = tuple_elements[3];
        break;
    }
    case DOP_MONITOR_P_EXIT: {
        if (tuple_arity != 5) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_MONITOR_P_EXIT;
        if (!vterm_is_atom(vtenv, &tuple_elements[1]) && !vterm_is_pid(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.monitor_p_exit.from_proc = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.monitor_p_exit.to_pid = tuple_elements[2];
        if (!vterm_is_reference(vtenv, &tuple_elements[3])) {
            return 0;
        }
        dop->data.monitor_p_exit.ref = tuple_elements[3];
        dop->data.monitor_p_exit.reason = tuple_elements[4];
        break;
    }
    case DOP_SEND_SENDER: {
        if (tuple_arity != 3) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_SEND_SENDER;
        if (!vterm_is_pid(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.send_sender.from_pid = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.send_sender.to_pid = tuple_elements[2];
        break;
    }
    case DOP_SEND_SENDER_TT: {
        if (tuple_arity != 4) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_SEND_SENDER_TT;
        if (!vterm_is_pid(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.send_sender_tt.from_pid = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.send_sender_tt.to_pid = tuple_elements[2];
        dop->data.send_sender_tt.trace_token = tuple_elements[3];
        break;
    }
    case DOP_PAYLOAD_EXIT: {
        if (tuple_arity != 3) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_PAYLOAD_EXIT;
        if (!vterm_is_pid(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.payload_exit.from_pid = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.payload_exit.to_pid = tuple_elements[2];
        break;
    }
    case DOP_PAYLOAD_EXIT_TT: {
        if (tuple_arity != 4) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_PAYLOAD_EXIT_TT;
        if (!vterm_is_pid(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.payload_exit_tt.from_pid = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.payload_exit_tt.to_pid = tuple_elements[2];
        dop->data.payload_exit_tt.trace_token = tuple_elements[3];
        break;
    }
    case DOP_PAYLOAD_EXIT2: {
        if (tuple_arity != 3) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_PAYLOAD_EXIT2;
        if (!vterm_is_pid(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.payload_exit2.from_pid = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.payload_exit2.to_pid = tuple_elements[2];
        break;
    }
    case DOP_PAYLOAD_EXIT2_TT: {
        if (tuple_arity != 4) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_PAYLOAD_EXIT2_TT;
        if (!vterm_is_pid(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.payload_exit2_tt.from_pid = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.payload_exit2_tt.to_pid = tuple_elements[2];
        dop->data.payload_exit2_tt.trace_token = tuple_elements[3];
        break;
    }
    case DOP_PAYLOAD_MONITOR_P_EXIT: {
        if (tuple_arity != 4) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_PAYLOAD_MONITOR_P_EXIT;
        if (!vterm_is_atom(vtenv, &tuple_elements[1]) && !vterm_is_pid(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.payload_monitor_p_exit.from_proc = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.payload_monitor_p_exit.to_pid = tuple_elements[2];
        if (!vterm_is_reference(vtenv, &tuple_elements[3])) {
            return 0;
        }
        dop->data.payload_monitor_p_exit.ref = tuple_elements[3];
        break;
    }
    case DOP_SPAWN_REQUEST: {
        if (tuple_arity != 6) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_SPAWN_REQUEST;
        if (!vterm_is_reference(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.spawn_request.req_id = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.spawn_request.from = tuple_elements[2];
        if (!vterm_is_pid(vtenv, &tuple_elements[3])) {
            return 0;
        }
        dop->data.spawn_request.group_leader = tuple_elements[3];
        if (!vterm_is_tuple(vtenv, &tuple_elements[4]) ||
            !vterm_get_tuple(vtenv, &tuple_elements[4], &sub_tuple_arity, &sub_tuple_elements) || sub_tuple_arity != 3) {
            return 0;
        }
        if (!vterm_is_atom(vtenv, &sub_tuple_elements[0])) {
            return 0;
        }
        dop->data.spawn_request.mfa.module = sub_tuple_elements[0];
        if (!vterm_is_atom(vtenv, &sub_tuple_elements[1])) {
            return 0;
        }
        dop->data.spawn_request.mfa.function = sub_tuple_elements[1];
        if (!vterm_is_fixed_integer(vtenv, &sub_tuple_elements[2])) {
            return 0;
        }
        dop->data.spawn_request.mfa.arity = sub_tuple_elements[2];
        if (!vterm_is_proper_list(vtenv, &tuple_elements[5])) {
            return 0;
        }
        dop->data.spawn_request.opt_list = tuple_elements[5];
        break;
    }
    case DOP_SPAWN_REQUEST_TT: {
        if (tuple_arity != 7) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_SPAWN_REQUEST_TT;
        if (!vterm_is_reference(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.spawn_request_tt.req_id = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.spawn_request_tt.from = tuple_elements[2];
        if (!vterm_is_pid(vtenv, &tuple_elements[3])) {
            return 0;
        }
        dop->data.spawn_request_tt.group_leader = tuple_elements[3];
        if (!vterm_is_tuple(vtenv, &tuple_elements[4]) ||
            !vterm_get_tuple(vtenv, &tuple_elements[4], &sub_tuple_arity, &sub_tuple_elements) || sub_tuple_arity != 3) {
            return 0;
        }
        if (!vterm_is_atom(vtenv, &sub_tuple_elements[0])) {
            return 0;
        }
        dop->data.spawn_request_tt.mfa.module = sub_tuple_elements[0];
        if (!vterm_is_atom(vtenv, &sub_tuple_elements[1])) {
            return 0;
        }
        dop->data.spawn_request_tt.mfa.function = sub_tuple_elements[1];
        if (!vterm_is_fixed_integer(vtenv, &sub_tuple_elements[2])) {
            return 0;
        }
        dop->data.spawn_request_tt.mfa.arity = sub_tuple_elements[2];
        if (!vterm_is_proper_list(vtenv, &tuple_elements[5])) {
            return 0;
        }
        dop->data.spawn_request_tt.opt_list = tuple_elements[5];
        dop->data.spawn_request_tt.token = tuple_elements[6];
        break;
    }
    case DOP_SPAWN_REPLY: {
        if (tuple_arity != 5) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_SPAWN_REPLY;
        if (!vterm_is_reference(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.spawn_reply.req_id = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.spawn_reply.to = tuple_elements[2];
        if (!vterm_is_fixed_integer(vtenv, &tuple_elements[3])) {
            return 0;
        }
        dop->data.spawn_reply.flags = tuple_elements[3];
        if (!vterm_is_atom(vtenv, &tuple_elements[4]) && !vterm_is_pid(vtenv, &tuple_elements[4])) {
            return 0;
        }
        dop->data.spawn_reply.result = tuple_elements[4];
        break;
    }
    case DOP_SPAWN_REPLY_TT: {
        if (tuple_arity != 6) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_SPAWN_REPLY_TT;
        if (!vterm_is_reference(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.spawn_reply_tt.req_id = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.spawn_reply_tt.to = tuple_elements[2];
        if (!vterm_is_fixed_integer(vtenv, &tuple_elements[3])) {
            return 0;
        }
        dop->data.spawn_reply_tt.flags = tuple_elements[3];
        if (!vterm_is_atom(vtenv, &tuple_elements[4]) && !vterm_is_pid(vtenv, &tuple_elements[4])) {
            return 0;
        }
        dop->data.spawn_reply_tt.result = tuple_elements[4];
        dop->data.spawn_reply_tt.token = tuple_elements[5];
        break;
    }
    case DOP_ALIAS_SEND: {
        if (tuple_arity != 3) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_ALIAS_SEND;
        if (!vterm_is_pid(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.alias_send.from_pid = tuple_elements[1];
        if (!vterm_is_reference(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.alias_send.alias = tuple_elements[2];
        break;
    }
    case DOP_ALIAS_SEND_TT: {
        if (tuple_arity != 4) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_ALIAS_SEND_TT;
        if (!vterm_is_pid(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.alias_send_tt.from_pid = tuple_elements[1];
        if (!vterm_is_reference(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.alias_send_tt.alias = tuple_elements[2];
        dop->data.alias_send_tt.token = tuple_elements[3];
        break;
    }
    case DOP_UNLINK_ID: {
        if (tuple_arity != 4) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_UNLINK_ID;
        if (!vterm_is_integer(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.unlink_id.id = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.unlink_id.from_pid = tuple_elements[2];
        if (!vterm_is_pid(vtenv, &tuple_elements[3])) {
            return 0;
        }
        dop->data.unlink_id.to_pid = tuple_elements[3];
        break;
    }
    case DOP_UNLINK_ID_ACK: {
        if (tuple_arity != 4) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_UNLINK_ID_ACK;
        if (!vterm_is_integer(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.unlink_id_ack.id = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.unlink_id_ack.from_pid = tuple_elements[2];
        if (!vterm_is_pid(vtenv, &tuple_elements[3])) {
            return 0;
        }
        dop->data.unlink_id_ack.to_pid = tuple_elements[3];
        break;
    }
    case DOP_ALTACT_SIG_SEND: {
        if (tuple_arity != 4 && tuple_arity != 5) {
            return 0;
        }
        dop->tag = VDIST_DOP_TAG_ALTACT_SIG_SEND;
        if (!vterm_is_fixed_integer(vtenv, &tuple_elements[1])) {
            return 0;
        }
        dop->data.altact_sig_send.flags = tuple_elements[1];
        if (!vterm_is_pid(vtenv, &tuple_elements[2])) {
            return 0;
        }
        dop->data.altact_sig_send.sender_pid = tuple_elements[2];
        if (!vterm_is_pid(vtenv, &tuple_elements[3]) && !vterm_is_atom(vtenv, &tuple_elements[3]) &&
            !vterm_is_reference(vtenv, &tuple_elements[3])) {
            return 0;
        }
        dop->data.altact_sig_send.to = tuple_elements[3];
        if (tuple_arity == 5) {
            dop->data.altact_sig_send.token = tuple_elements[4];
        } else {
            dop->data.altact_sig_send.token = NULL;
        }
        break;
    }
    default:
        return 0;
    }
    return 1;
}

int
vdist_dop_debug_dump(ErlNifEnv *env, vterm_env_t *vtenv, vdist_dop_t *dop, ERL_NIF_TERM *term)
{
#define DUMP_VTERM(dst, sub_term)                                                                                                  \
    do {                                                                                                                           \
        if (!vterm_debug_dump(env, vtenv, (sub_term), &(dst))) {                                                                   \
            goto error;                                                                                                            \
        }                                                                                                                          \
    } while (0)

    switch (dop->tag) {
    case VDIST_DOP_TAG_LINK: {
        ERL_NIF_TERM from_pid_term;
        ERL_NIF_TERM to_pid_term;
        DUMP_VTERM(from_pid_term, &dop->data.link.from_pid);
        DUMP_VTERM(to_pid_term, &dop->data.link.to_pid);
        *term = enif_make_tuple3(env, ATOM(vdist_dop_link), from_pid_term, to_pid_term);
        break;
    }
    case VDIST_DOP_TAG_SEND: {
        ERL_NIF_TERM unused_term;
        ERL_NIF_TERM to_pid_term;
        DUMP_VTERM(unused_term, &dop->data.send.unused);
        DUMP_VTERM(to_pid_term, &dop->data.send.to_pid);
        *term = enif_make_tuple3(env, ATOM(vdist_dop_send), unused_term, to_pid_term);
        break;
    }
    case VDIST_DOP_TAG_EXIT: {
        ERL_NIF_TERM from_pid_term;
        ERL_NIF_TERM to_pid_term;
        ERL_NIF_TERM reason_term;
        DUMP_VTERM(from_pid_term, &dop->data.exit.from_pid);
        DUMP_VTERM(to_pid_term, &dop->data.exit.to_pid);
        DUMP_VTERM(reason_term, &dop->data.exit.reason);
        *term = enif_make_tuple4(env, ATOM(vdist_dop_exit), from_pid_term, to_pid_term, reason_term);
        break;
    }
    case VDIST_DOP_TAG_UNLINK: {
        ERL_NIF_TERM from_pid_term;
        ERL_NIF_TERM to_pid_term;
        DUMP_VTERM(from_pid_term, &dop->data.unlink.from_pid);
        DUMP_VTERM(to_pid_term, &dop->data.unlink.to_pid);
        *term = enif_make_tuple3(env, ATOM(vdist_dop_unlink), from_pid_term, to_pid_term);
        break;
    }
    case VDIST_DOP_TAG_REG_SEND: {
        ERL_NIF_TERM from_pid_term;
        ERL_NIF_TERM unused_term;
        ERL_NIF_TERM to_name_term;
        DUMP_VTERM(from_pid_term, &dop->data.reg_send.from_pid);
        DUMP_VTERM(unused_term, &dop->data.reg_send.unused);
        DUMP_VTERM(to_name_term, &dop->data.reg_send.to_name);
        *term = enif_make_tuple4(env, ATOM(vdist_dop_reg_send), from_pid_term, unused_term, to_name_term);
        break;
    }
    case VDIST_DOP_TAG_GROUP_LEADER: {
        ERL_NIF_TERM from_pid_term;
        ERL_NIF_TERM to_pid_term;
        DUMP_VTERM(from_pid_term, &dop->data.group_leader.from_pid);
        DUMP_VTERM(to_pid_term, &dop->data.group_leader.to_pid);
        *term = enif_make_tuple3(env, ATOM(vdist_dop_group_leader), from_pid_term, to_pid_term);
        break;
    }
    case VDIST_DOP_TAG_EXIT2: {
        ERL_NIF_TERM from_pid_term;
        ERL_NIF_TERM to_pid_term;
        ERL_NIF_TERM reason_term;
        DUMP_VTERM(from_pid_term, &dop->data.exit2.from_pid);
        DUMP_VTERM(to_pid_term, &dop->data.exit2.to_pid);
        DUMP_VTERM(reason_term, &dop->data.exit2.reason);
        *term = enif_make_tuple4(env, ATOM(vdist_dop_exit2), from_pid_term, to_pid_term, reason_term);
        break;
    }
    case VDIST_DOP_TAG_SEND_TT: {
        ERL_NIF_TERM unused_term;
        ERL_NIF_TERM to_pid_term;
        ERL_NIF_TERM trace_token_term;
        DUMP_VTERM(unused_term, &dop->data.send_tt.unused);
        DUMP_VTERM(to_pid_term, &dop->data.send_tt.to_pid);
        DUMP_VTERM(trace_token_term, &dop->data.send_tt.trace_token);
        *term = enif_make_tuple4(env, ATOM(vdist_dop_send_tt), unused_term, to_pid_term, trace_token_term);
        break;
    }
    case VDIST_DOP_TAG_EXIT_TT: {
        ERL_NIF_TERM from_pid_term;
        ERL_NIF_TERM to_pid_term;
        ERL_NIF_TERM trace_token_term;
        ERL_NIF_TERM reason_term;
        DUMP_VTERM(from_pid_term, &dop->data.exit_tt.from_pid);
        DUMP_VTERM(to_pid_term, &dop->data.exit_tt.to_pid);
        DUMP_VTERM(trace_token_term, &dop->data.exit_tt.trace_token);
        DUMP_VTERM(reason_term, &dop->data.exit_tt.reason);
        *term = enif_make_tuple5(env, ATOM(vdist_dop_exit_tt), from_pid_term, to_pid_term, trace_token_term, reason_term);
        break;
    }
    case VDIST_DOP_TAG_REG_SEND_TT: {
        ERL_NIF_TERM from_pid_term;
        ERL_NIF_TERM unused_term;
        ERL_NIF_TERM to_name_term;
        ERL_NIF_TERM trace_token_term;
        DUMP_VTERM(from_pid_term, &dop->data.reg_send_tt.from_pid);
        DUMP_VTERM(unused_term, &dop->data.reg_send_tt.unused);
        DUMP_VTERM(to_name_term, &dop->data.reg_send_tt.to_name);
        DUMP_VTERM(trace_token_term, &dop->data.reg_send_tt.trace_token);
        *term = enif_make_tuple5(env, ATOM(vdist_dop_reg_send_tt), from_pid_term, unused_term, to_name_term, trace_token_term);
        break;
    }
    case VDIST_DOP_TAG_EXIT2_TT: {
        ERL_NIF_TERM from_pid_term;
        ERL_NIF_TERM to_pid_term;
        ERL_NIF_TERM trace_token_term;
        ERL_NIF_TERM reason_term;
        DUMP_VTERM(from_pid_term, &dop->data.exit2_tt.from_pid);
        DUMP_VTERM(to_pid_term, &dop->data.exit2_tt.to_pid);
        DUMP_VTERM(trace_token_term, &dop->data.exit2_tt.trace_token);
        DUMP_VTERM(reason_term, &dop->data.exit2_tt.reason);
        *term = enif_make_tuple5(env, ATOM(vdist_dop_exit2_tt), from_pid_term, to_pid_term, trace_token_term, reason_term);
        break;
    }
    case VDIST_DOP_TAG_MONITOR_P: {
        ERL_NIF_TERM from_pid_term;
        ERL_NIF_TERM to_proc_term;
        ERL_NIF_TERM ref_term;
        DUMP_VTERM(from_pid_term, &dop->data.monitor_p.from_pid);
        DUMP_VTERM(to_proc_term, &dop->data.monitor_p.to_proc);
        DUMP_VTERM(ref_term, &dop->data.monitor_p.ref);
        *term = enif_make_tuple4(env, ATOM(vdist_dop_monitor_p), from_pid_term, to_proc_term, ref_term);
        break;
    }
    case VDIST_DOP_TAG_DEMONITOR_P: {
        ERL_NIF_TERM from_pid_term;
        ERL_NIF_TERM to_proc_term;
        ERL_NIF_TERM ref_term;
        DUMP_VTERM(from_pid_term, &dop->data.demonitor_p.from_pid);
        DUMP_VTERM(to_proc_term, &dop->data.demonitor_p.to_proc);
        DUMP_VTERM(ref_term, &dop->data.demonitor_p.ref);
        *term = enif_make_tuple4(env, ATOM(vdist_dop_demonitor_p), from_pid_term, to_proc_term, ref_term);
        break;
    }
    case VDIST_DOP_TAG_MONITOR_P_EXIT: {
        ERL_NIF_TERM from_proc_term;
        ERL_NIF_TERM to_pid_term;
        ERL_NIF_TERM ref_term;
        ERL_NIF_TERM reason_term;
        DUMP_VTERM(from_proc_term, &dop->data.monitor_p_exit.from_proc);
        DUMP_VTERM(to_pid_term, &dop->data.monitor_p_exit.to_pid);
        DUMP_VTERM(ref_term, &dop->data.monitor_p_exit.ref);
        DUMP_VTERM(reason_term, &dop->data.monitor_p_exit.reason);
        *term = enif_make_tuple5(env, ATOM(vdist_dop_monitor_p_exit), from_proc_term, to_pid_term, ref_term, reason_term);
        break;
    }
    case VDIST_DOP_TAG_SEND_SENDER: {
        ERL_NIF_TERM from_pid_term;
        ERL_NIF_TERM to_pid_term;
        DUMP_VTERM(from_pid_term, &dop->data.send_sender.from_pid);
        DUMP_VTERM(to_pid_term, &dop->data.send_sender.to_pid);
        *term = enif_make_tuple3(env, ATOM(vdist_dop_send_sender), from_pid_term, to_pid_term);
        break;
    }
    case VDIST_DOP_TAG_SEND_SENDER_TT: {
        ERL_NIF_TERM from_pid_term;
        ERL_NIF_TERM to_pid_term;
        ERL_NIF_TERM trace_token_term;
        DUMP_VTERM(from_pid_term, &dop->data.send_sender_tt.from_pid);
        DUMP_VTERM(to_pid_term, &dop->data.send_sender_tt.to_pid);
        DUMP_VTERM(trace_token_term, &dop->data.send_sender_tt.trace_token);
        *term = enif_make_tuple4(env, ATOM(vdist_dop_send_sender_tt), from_pid_term, to_pid_term, trace_token_term);
        break;
    }
    case VDIST_DOP_TAG_PAYLOAD_EXIT: {
        ERL_NIF_TERM from_pid_term;
        ERL_NIF_TERM to_pid_term;
        DUMP_VTERM(from_pid_term, &dop->data.payload_exit.from_pid);
        DUMP_VTERM(to_pid_term, &dop->data.payload_exit.to_pid);
        *term = enif_make_tuple3(env, ATOM(vdist_dop_payload_exit), from_pid_term, to_pid_term);
        break;
    }
    case VDIST_DOP_TAG_PAYLOAD_EXIT_TT: {
        ERL_NIF_TERM from_pid_term;
        ERL_NIF_TERM to_pid_term;
        ERL_NIF_TERM trace_token_term;
        DUMP_VTERM(from_pid_term, &dop->data.payload_exit_tt.from_pid);
        DUMP_VTERM(to_pid_term, &dop->data.payload_exit_tt.to_pid);
        DUMP_VTERM(trace_token_term, &dop->data.payload_exit_tt.trace_token);
        *term = enif_make_tuple4(env, ATOM(vdist_dop_payload_exit_tt), from_pid_term, to_pid_term, trace_token_term);
        break;
    }
    case VDIST_DOP_TAG_PAYLOAD_EXIT2: {
        ERL_NIF_TERM from_pid_term;
        ERL_NIF_TERM to_pid_term;
        DUMP_VTERM(from_pid_term, &dop->data.payload_exit2.from_pid);
        DUMP_VTERM(to_pid_term, &dop->data.payload_exit2.to_pid);
        *term = enif_make_tuple3(env, ATOM(vdist_dop_payload_exit2), from_pid_term, to_pid_term);
        break;
    }
    case VDIST_DOP_TAG_PAYLOAD_EXIT2_TT: {
        ERL_NIF_TERM from_pid_term;
        ERL_NIF_TERM to_pid_term;
        ERL_NIF_TERM trace_token_term;
        DUMP_VTERM(from_pid_term, &dop->data.payload_exit2_tt.from_pid);
        DUMP_VTERM(to_pid_term, &dop->data.payload_exit2_tt.to_pid);
        DUMP_VTERM(trace_token_term, &dop->data.payload_exit2_tt.trace_token);
        *term = enif_make_tuple4(env, ATOM(vdist_dop_payload_exit2_tt), from_pid_term, to_pid_term, trace_token_term);
        break;
    }
    case VDIST_DOP_TAG_PAYLOAD_MONITOR_P_EXIT: {
        ERL_NIF_TERM from_proc_term;
        ERL_NIF_TERM to_pid_term;
        ERL_NIF_TERM ref_term;
        DUMP_VTERM(from_proc_term, &dop->data.payload_monitor_p_exit.from_proc);
        DUMP_VTERM(to_pid_term, &dop->data.payload_monitor_p_exit.to_pid);
        DUMP_VTERM(ref_term, &dop->data.payload_monitor_p_exit.ref);
        *term = enif_make_tuple4(env, ATOM(vdist_dop_payload_monitor_p_exit), from_proc_term, to_pid_term, ref_term);
        break;
    }
    case VDIST_DOP_TAG_SPAWN_REQUEST: {
        ERL_NIF_TERM req_id_term;
        ERL_NIF_TERM from_term;
        ERL_NIF_TERM group_leader_term;
        ERL_NIF_TERM mfa_module_term;
        ERL_NIF_TERM mfa_function_term;
        ERL_NIF_TERM mfa_arity_term;
        ERL_NIF_TERM opt_list_term;
        DUMP_VTERM(req_id_term, &dop->data.spawn_request.req_id);
        DUMP_VTERM(from_term, &dop->data.spawn_request.from);
        DUMP_VTERM(group_leader_term, &dop->data.spawn_request.group_leader);
        DUMP_VTERM(mfa_module_term, &dop->data.spawn_request.mfa.module);
        DUMP_VTERM(mfa_function_term, &dop->data.spawn_request.mfa.function);
        DUMP_VTERM(mfa_arity_term, &dop->data.spawn_request.mfa.arity);
        if (!vterm_is_proper_list(vtenv, &dop->data.spawn_request.opt_list)) {
            goto error;
        }
        opt_list_term = enif_make_list(env, 0);
        if (!vterm_is_nil_ext(vtenv, &dop->data.spawn_request.opt_list)) {
            ERL_NIF_TERM element_term;
            int i = (int)(dop->data.spawn_request.opt_list->data.list_ext.len);
            while (i--) {
                DUMP_VTERM(element_term, &dop->data.spawn_request.opt_list->data.list_ext.elements[i]);
                opt_list_term = enif_make_list_cell(env, element_term, opt_list_term);
            }
        }
        *term = enif_make_tuple8(env, ATOM(vdist_dop_spawn_request), req_id_term, from_term, group_leader_term, mfa_module_term,
                                 mfa_function_term, mfa_arity_term, opt_list_term);
        break;
    }
    case VDIST_DOP_TAG_SPAWN_REQUEST_TT: {
        ERL_NIF_TERM req_id_term;
        ERL_NIF_TERM from_term;
        ERL_NIF_TERM group_leader_term;
        ERL_NIF_TERM mfa_module_term;
        ERL_NIF_TERM mfa_function_term;
        ERL_NIF_TERM mfa_arity_term;
        ERL_NIF_TERM opt_list_term;
        ERL_NIF_TERM token_term;
        DUMP_VTERM(req_id_term, &dop->data.spawn_request_tt.req_id);
        DUMP_VTERM(from_term, &dop->data.spawn_request_tt.from);
        DUMP_VTERM(group_leader_term, &dop->data.spawn_request_tt.group_leader);
        DUMP_VTERM(mfa_module_term, &dop->data.spawn_request_tt.mfa.module);
        DUMP_VTERM(mfa_function_term, &dop->data.spawn_request_tt.mfa.function);
        DUMP_VTERM(mfa_arity_term, &dop->data.spawn_request_tt.mfa.arity);
        if (!vterm_is_proper_list(vtenv, &dop->data.spawn_request_tt.opt_list)) {
            goto error;
        }
        opt_list_term = enif_make_list(env, 0);
        if (!vterm_is_nil_ext(vtenv, &dop->data.spawn_request_tt.opt_list)) {
            ERL_NIF_TERM element_term;
            int i = (int)(dop->data.spawn_request_tt.opt_list->data.list_ext.len);
            while (i--) {
                DUMP_VTERM(element_term, &dop->data.spawn_request_tt.opt_list->data.list_ext.elements[i]);
                opt_list_term = enif_make_list_cell(env, element_term, opt_list_term);
            }
        }
        DUMP_VTERM(token_term, &dop->data.spawn_request_tt.token);
        *term = enif_make_tuple9(env, ATOM(vdist_dop_spawn_request_tt), req_id_term, from_term, group_leader_term, mfa_module_term,
                                 mfa_function_term, mfa_arity_term, opt_list_term, token_term);
        break;
    }
    case VDIST_DOP_TAG_SPAWN_REPLY: {
        ERL_NIF_TERM req_id_term;
        ERL_NIF_TERM to_term;
        ERL_NIF_TERM flags_term;
        ERL_NIF_TERM result_term;
        DUMP_VTERM(req_id_term, &dop->data.spawn_reply.req_id);
        DUMP_VTERM(to_term, &dop->data.spawn_reply.to);
        DUMP_VTERM(flags_term, &dop->data.spawn_reply.flags);
        DUMP_VTERM(result_term, &dop->data.spawn_reply.result);
        *term = enif_make_tuple5(env, ATOM(vdist_dop_spawn_reply), req_id_term, to_term, flags_term, result_term);
        break;
    }
    case VDIST_DOP_TAG_SPAWN_REPLY_TT: {
        ERL_NIF_TERM req_id_term;
        ERL_NIF_TERM to_term;
        ERL_NIF_TERM flags_term;
        ERL_NIF_TERM result_term;
        ERL_NIF_TERM token_term;
        DUMP_VTERM(req_id_term, &dop->data.spawn_reply_tt.req_id);
        DUMP_VTERM(to_term, &dop->data.spawn_reply_tt.to);
        DUMP_VTERM(flags_term, &dop->data.spawn_reply_tt.flags);
        DUMP_VTERM(result_term, &dop->data.spawn_reply_tt.result);
        DUMP_VTERM(token_term, &dop->data.spawn_reply_tt.token);
        *term = enif_make_tuple6(env, ATOM(vdist_dop_spawn_reply_tt), req_id_term, to_term, flags_term, result_term, token_term);
        break;
    }
    case VDIST_DOP_TAG_ALIAS_SEND: {
        ERL_NIF_TERM from_pid_term;
        ERL_NIF_TERM alias_term;
        DUMP_VTERM(from_pid_term, &dop->data.alias_send.from_pid);
        DUMP_VTERM(alias_term, &dop->data.alias_send.alias);
        *term = enif_make_tuple3(env, ATOM(vdist_dop_alias_send), from_pid_term, alias_term);
        break;
    }
    case VDIST_DOP_TAG_ALIAS_SEND_TT: {
        ERL_NIF_TERM from_pid_term;
        ERL_NIF_TERM alias_term;
        ERL_NIF_TERM token_term;
        DUMP_VTERM(from_pid_term, &dop->data.alias_send_tt.from_pid);
        DUMP_VTERM(alias_term, &dop->data.alias_send_tt.alias);
        DUMP_VTERM(token_term, &dop->data.alias_send_tt.token);
        *term = enif_make_tuple4(env, ATOM(vdist_dop_alias_send_tt), from_pid_term, alias_term, token_term);
        break;
    }
    case VDIST_DOP_TAG_UNLINK_ID: {
        ERL_NIF_TERM id_term;
        ERL_NIF_TERM from_pid_term;
        ERL_NIF_TERM to_pid_term;
        DUMP_VTERM(id_term, &dop->data.unlink_id.id);
        DUMP_VTERM(from_pid_term, &dop->data.unlink_id.from_pid);
        DUMP_VTERM(to_pid_term, &dop->data.unlink_id.to_pid);
        *term = enif_make_tuple4(env, ATOM(vdist_dop_unlink_id), id_term, from_pid_term, to_pid_term);
        break;
    }
    case VDIST_DOP_TAG_UNLINK_ID_ACK: {
        ERL_NIF_TERM id_term;
        ERL_NIF_TERM from_pid_term;
        ERL_NIF_TERM to_pid_term;
        DUMP_VTERM(id_term, &dop->data.unlink_id_ack.id);
        DUMP_VTERM(from_pid_term, &dop->data.unlink_id_ack.from_pid);
        DUMP_VTERM(to_pid_term, &dop->data.unlink_id_ack.to_pid);
        *term = enif_make_tuple4(env, ATOM(vdist_dop_unlink_id_ack), id_term, from_pid_term, to_pid_term);
        break;
    }
    case VDIST_DOP_TAG_ALTACT_SIG_SEND: {
        ERL_NIF_TERM flags_term;
        ERL_NIF_TERM sender_pid_term;
        ERL_NIF_TERM to_term;
        ERL_NIF_TERM token_term;
        DUMP_VTERM(flags_term, &dop->data.altact_sig_send.flags);
        DUMP_VTERM(sender_pid_term, &dop->data.altact_sig_send.sender_pid);
        DUMP_VTERM(to_term, &dop->data.altact_sig_send.to);
        if (dop->data.altact_sig_send.token != NULL) {
            DUMP_VTERM(token_term, &dop->data.altact_sig_send.token);
            token_term = enif_make_tuple2(env, ATOM(some), token_term);
        } else {
            token_term = ATOM(none);
        }
        *term = enif_make_tuple5(env, ATOM(vdist_dop_altact_sig_send), flags_term, sender_pid_term, to_term, token_term);
        break;
    }
    default:
        goto error;
    }

    return 1;

error:
    return 0;

#undef DUMP_VTERM
}

int
vdist_dop_get_name(const vdist_dop_t *dop, const char **name)
{
    if (dop == NULL) {
        return 0;
    }
    switch (dop->tag) {
    case VDIST_DOP_TAG_LINK:
        *name = "LINK";
        return 1;
    case VDIST_DOP_TAG_SEND:
        *name = "SEND";
        return 1;
    case VDIST_DOP_TAG_EXIT:
        *name = "EXIT";
        return 1;
    case VDIST_DOP_TAG_UNLINK:
        *name = "UNLINK";
        return 1;
    case VDIST_DOP_TAG_REG_SEND:
        *name = "REG_SEND";
        return 1;
    case VDIST_DOP_TAG_GROUP_LEADER:
        *name = "GROUP_LEADER";
        return 1;
    case VDIST_DOP_TAG_EXIT2:
        *name = "EXIT2";
        return 1;
    case VDIST_DOP_TAG_SEND_TT:
        *name = "SEND_TT";
        return 1;
    case VDIST_DOP_TAG_EXIT_TT:
        *name = "EXIT_TT";
        return 1;
    case VDIST_DOP_TAG_REG_SEND_TT:
        *name = "REG_SEND_TT";
        return 1;
    case VDIST_DOP_TAG_EXIT2_TT:
        *name = "EXIT2_TT";
        return 1;
    case VDIST_DOP_TAG_MONITOR_P:
        *name = "MONITOR_P";
        return 1;
    case VDIST_DOP_TAG_DEMONITOR_P:
        *name = "DEMONITOR_P";
        return 1;
    case VDIST_DOP_TAG_MONITOR_P_EXIT:
        *name = "MONITOR_P_EXIT";
        return 1;
    case VDIST_DOP_TAG_SEND_SENDER:
        *name = "SEND_SENDER";
        return 1;
    case VDIST_DOP_TAG_SEND_SENDER_TT:
        *name = "SEND_SENDER_TT";
        return 1;
    case VDIST_DOP_TAG_PAYLOAD_EXIT:
        *name = "PAYLOAD_EXIT";
        return 1;
    case VDIST_DOP_TAG_PAYLOAD_EXIT_TT:
        *name = "PAYLOAD_EXIT_TT";
        return 1;
    case VDIST_DOP_TAG_PAYLOAD_EXIT2:
        *name = "PAYLOAD_EXIT2";
        return 1;
    case VDIST_DOP_TAG_PAYLOAD_EXIT2_TT:
        *name = "PAYLOAD_EXIT2_TT";
        return 1;
    case VDIST_DOP_TAG_PAYLOAD_MONITOR_P_EXIT:
        *name = "PAYLOAD_MONITOR_P_EXIT";
        return 1;
    case VDIST_DOP_TAG_SPAWN_REQUEST:
        *name = "SPAWN_REQUEST";
        return 1;
    case VDIST_DOP_TAG_SPAWN_REQUEST_TT:
        *name = "SPAWN_REQUEST_TT";
        return 1;
    case VDIST_DOP_TAG_SPAWN_REPLY:
        *name = "SPAWN_REPLY";
        return 1;
    case VDIST_DOP_TAG_SPAWN_REPLY_TT:
        *name = "SPAWN_REPLY_TT";
        return 1;
    case VDIST_DOP_TAG_ALIAS_SEND:
        *name = "ALIAS_SEND";
        return 1;
    case VDIST_DOP_TAG_ALIAS_SEND_TT:
        *name = "ALIAS_SEND_TT";
        return 1;
    case VDIST_DOP_TAG_UNLINK_ID:
        *name = "UNLINK_ID";
        return 1;
    case VDIST_DOP_TAG_UNLINK_ID_ACK:
        *name = "UNLINK_ID_ACK";
        return 1;
    case VDIST_DOP_TAG_ALTACT_SIG_SEND:
        *name = "ALTACT_SIG_SEND";
        return 1;
    default:
        return 0;
    }
}
