/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "udist.h"
#include "../channel/edf_channel.h"
#include "../etf/etf_decode.h"
#include "../uterm/uterm.h"

#define EMIT()                                                                                                                     \
    do {                                                                                                                           \
        up->flags |= (UDIST_CLASSIFY_FLAG_EMIT);                                                                                   \
    } while (0)

#define LOG()                                                                                                                      \
    do {                                                                                                                           \
        up->flags |= (UDIST_CLASSIFY_FLAG_LOG_EVENT);                                                                              \
    } while (0)

#define LOG_DROP()                                                                                                                 \
    do {                                                                                                                           \
        up->flags |= (UDIST_CLASSIFY_FLAG_LOG_EVENT | UDIST_CLASSIFY_FLAG_DROP);                                                   \
    } while (0)

#define LOG_DROP_OR_REDIRECT()                                                                                                     \
    do {                                                                                                                           \
        up->flags |= (UDIST_CLASSIFY_FLAG_LOG_EVENT | UDIST_CLASSIFY_FLAG_DROP | UDIST_CLASSIFY_FLAG_REDIRECT_DOP);                \
    } while (0)

#define LOG_REDIRECT_SPAWN_REQUEST()                                                                                               \
    do {                                                                                                                           \
        up->flags |= (UDIST_CLASSIFY_FLAG_LOG_EVENT | UDIST_CLASSIFY_FLAG_REDIRECT_SPAWN_REQUEST);                                 \
    } while (0)

static int udist_classify_send(ErlNifEnv *caller_env, vterm_env_t *vtenv, udist_t *up, bool untrusted, bool is_pass_through,
                               slice_t *payload, ERL_NIF_TERM *err_termp);
static int udist_classify_send_to_net_kernel(ErlNifEnv *caller_env, vterm_env_t *vtenv, udist_t *up, bool untrusted,
                                             bool is_pass_through, slice_t *payload, ERL_NIF_TERM *err_termp);
static int udist_classify_send_to_rex(ErlNifEnv *caller_env, vterm_env_t *vtenv, udist_t *up, bool untrusted, bool is_pass_through,
                                      slice_t *payload, ERL_NIF_TERM *err_termp);

int
udist_classify(ErlNifEnv *caller_env, vterm_env_t *vtenv, udist_t *up, bool untrusted, bool is_pass_through, slice_t *payload,
               ERL_NIF_TERM *err_termp)
{
    if (up->flags != UDIST_CLASSIFY_FLAG_NONE) {
        return 1;
    }
    switch (up->control.tag) {
    case UDIST_CONTROL_TAG_EXIT:
        // LOG, DROP
        LOG_DROP();
        return 1;
    case UDIST_CONTROL_TAG_EXIT2:
        // LOG, DROP or REDIRECT
        LOG_DROP_OR_REDIRECT();
        return 1;
    case UDIST_CONTROL_TAG_GROUP_LEADER:
        // LOG, DROP or REDIRECT
        LOG_DROP_OR_REDIRECT();
        return 1;
    case UDIST_CONTROL_TAG_LINK:
        // LOG, DROP
        LOG_DROP();
        return 1;
    case UDIST_CONTROL_TAG_MONITOR_RELATED:
        if (untrusted) {
            // LOG, EMIT
            LOG();
            return 1;
        }
        // EMIT
        EMIT();
        return 1;
    case UDIST_CONTROL_TAG_SEND_TO_ALIAS:
        if (untrusted) {
            // LOG, DROP or REDIRECT
            LOG_DROP_OR_REDIRECT();
            return 1;
        }
        return udist_classify_send(caller_env, vtenv, up, untrusted, is_pass_through, payload, err_termp);
    case UDIST_CONTROL_TAG_SEND_TO_NAME: {
        if (untrusted) {
            // LOG, DROP or REDIRECT
            LOG_DROP_OR_REDIRECT();
            return 1;
        }
        if (up->control.data.send_to == ATOM(net_kernel)) {
            return udist_classify_send_to_net_kernel(caller_env, vtenv, up, untrusted, is_pass_through, payload, err_termp);
        } else if (up->control.data.send_to == ATOM(rex)) {
            return udist_classify_send_to_rex(caller_env, vtenv, up, untrusted, is_pass_through, payload, err_termp);
        }
        return udist_classify_send(caller_env, vtenv, up, untrusted, is_pass_through, payload, err_termp);
    }
    case UDIST_CONTROL_TAG_SEND_TO_PID:
        if (untrusted) {
            // LOG, DROP or REDIRECT
            LOG_DROP_OR_REDIRECT();
            return 1;
        }
        return udist_classify_send(caller_env, vtenv, up, untrusted, is_pass_through, payload, err_termp);
    case UDIST_CONTROL_TAG_SPAWN_REPLY:
        if (untrusted) {
            // LOG, EMIT
            LOG();
            return 1;
        }
        // EMIT
        EMIT();
        return 1;
    case UDIST_CONTROL_TAG_SPAWN_REQUEST:
        // LOG, REDIRECT
        LOG_REDIRECT_SPAWN_REQUEST();
        return 1;
    case UDIST_CONTROL_TAG_UNLINK:
        if (untrusted) {
            // LOG, EMIT
            LOG();
            return 1;
        }
        // EMIT
        EMIT();
        return 1;
    default:
        *err_termp = EXCP_ERROR_F(caller_env, "Unknown control tag=%u for dop=%u\n", up->control.tag, up->info.dop);
        return 0;
    }
}

int
udist_classify_send(ErlNifEnv *caller_env, vterm_env_t *vtenv, udist_t *up, bool untrusted, bool is_pass_through, slice_t *payload,
                    ERL_NIF_TERM *err_termp)
{
    // Logs, emits, drops, and/or redirects any messages matching the following:
    //
    //     {system, From, Request}
    //     {'EXIT', Pid, Reason}
    //     {'$gen_cast', {try_again_restart, TryAgainId}}
    //     {'$gen_call', From, {start_child, ChildSpec}}
    //     {'$gen_call', From, {terminate_child, ChildId}}
    //     {'$gen_call', From, {restart_child, ChildId}}
    //     {'$gen_call', From, {delete_child, ChildId}}
    //     {io_request, From, ReplyAs, Request}
    //     {io_reply, ReplyAs, Reply}
    //
    // Otherwise, emit the message.
    vec_t vec[1];
    vec_reader_t vr[1];
    uint32_t arity;
    ERL_NIF_TERM atom = THE_NON_VALUE;
    if (payload == NULL) {
        *err_termp = EXCP_ERROR(caller_env, "Call to udist_classify_send() failed: payload is NULL\n");
        return 0;
    }
    (void)vec_init_free(vec);
    if (!vec_create_from_slice(vec, payload->head, payload->tail)) {
        *err_termp = EXCP_ERROR(caller_env, "Call to udist_classify_send() failed: payload is invalid\n");
        return 0;
    }
    if (!vec_reader_create(vr, vec, 0)) {
        *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_create() failed: unable to get slice for payload message\n");
        return 0;
    }
    if (is_pass_through && !vec_reader_skip_exact(vr, 1)) {
        *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_skip_exact() failed: unable to decode payload message\n");
        return 0;
    }
    if (uterm_is_tuple(vtenv, vr)) {
        if (is_pass_through && !vec_reader_back_exact(vr, 1)) {
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_back_exact() failed: unable to decode payload message\n");
            return 0;
        }
        if (!etf_decode_tuple_header(caller_env, vtenv, is_pass_through, vr, &arity, err_termp)) {
            return 0;
        }
        if (arity > 0 && uterm_is_atom(vtenv, vr)) {
            if (!etf_decode_atom_term(caller_env, vtenv, false, vr, &atom, err_termp)) {
                return 0;
            }
            if (arity == 2) {
                if (atom == ATOM($gen_cast)) {
                    if (uterm_is_tuple(vtenv, vr)) {
                        if (!etf_decode_tuple_header(caller_env, vtenv, false, vr, &arity, err_termp)) {
                            return 0;
                        }
                        if (arity > 0 && uterm_is_atom(vtenv, vr)) {
                            if (!etf_decode_atom_term(caller_env, vtenv, false, vr, &atom, err_termp)) {
                                return 0;
                            }
                            if (arity == 2 && atom == ATOM(try_again_restart)) {
                                // LOG, DROP or REDIRECT
                                LOG_DROP_OR_REDIRECT();
                                return 1;
                            }
                        }
                    }
                }
            } else if (arity == 3) {
                if (atom == ATOM(system) || atom == ATOM(EXIT)) {
                    // LOG, DROP or REDIRECT
                    LOG_DROP_OR_REDIRECT();
                    return 1;
                } else if (atom == ATOM($gen_call)) {
                    if (!etf_fast_skip_terms(caller_env, false, vr, 1, err_termp)) {
                        return 0;
                    }
                    if (uterm_is_tuple(vtenv, vr)) {
                        if (!etf_decode_tuple_header(caller_env, vtenv, false, vr, &arity, err_termp)) {
                            return 0;
                        }
                        if (arity > 0 && uterm_is_atom(vtenv, vr)) {
                            if (!etf_decode_atom_term(caller_env, vtenv, false, vr, &atom, err_termp)) {
                                return 0;
                            }
                            if (arity == 2 && (atom == ATOM(start_child) || atom == ATOM(terminate_child) ||
                                               atom == ATOM(restart_child) || atom == ATOM(delete_child))) {
                                // LOG, DROP or REDIRECT
                                LOG_DROP_OR_REDIRECT();
                                return 1;
                            }
                        }
                    }
                } else if (atom == ATOM(io_reply)) {
                    // LOG, DROP or REDIRECT
                    LOG_DROP_OR_REDIRECT();
                    return 1;
                }
            } else if (arity == 4) {
                if (atom == ATOM(io_request)) {
                    // LOG, DROP or REDIRECT
                    LOG_DROP_OR_REDIRECT();
                    return 1;
                }
            }
        }
    }
    // EMIT
    EMIT();
    return 1;
}

int
udist_classify_send_to_net_kernel(ErlNifEnv *caller_env, vterm_env_t *vtenv, udist_t *up, bool untrusted, bool is_pass_through,
                                  slice_t *payload, ERL_NIF_TERM *err_termp)
{
    // REG_SEND: net_kernel
    //
    // Only allow messages matching the following:
    //
    //     {'$gen_call', From, {is_auth, Node}}
    //
    // Otherwise, redirect the message (drop it).
    vec_t vec[1];
    vec_reader_t vr[1];
    uint32_t arity;
    ERL_NIF_TERM atom = THE_NON_VALUE;
    if (payload == NULL) {
        *err_termp = EXCP_ERROR(caller_env, "Call to udist_classify_send_to_net_kernel() failed: payload is NULL\n");
        return 0;
    }
    (void)vec_init_free(vec);
    if (!vec_create_from_slice(vec, payload->head, payload->tail)) {
        *err_termp = EXCP_ERROR(caller_env, "Call to udist_classify_send_to_net_kernel() failed: payload is invalid\n");
        return 0;
    }
    if (!vec_reader_create(vr, vec, 0)) {
        *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_create() failed: unable to get slice for payload message\n");
        return 0;
    }
    if (is_pass_through && !vec_reader_skip_exact(vr, 1)) {
        *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_skip_exact() failed: unable to decode payload message\n");
        return 0;
    }
    if (uterm_is_tuple(vtenv, vr)) {
        if (is_pass_through && !vec_reader_back_exact(vr, 1)) {
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_back_exact() failed: unable to decode payload message\n");
            return 0;
        }
        if (!etf_decode_tuple_header(caller_env, vtenv, is_pass_through, vr, &arity, err_termp)) {
            return 0;
        }
        if (arity == 3 && uterm_is_atom(vtenv, vr)) {
            if (!etf_decode_atom_term(caller_env, vtenv, false, vr, &atom, err_termp)) {
                return 0;
            }
            if (atom == ATOM($gen_call)) {
                if (!etf_fast_skip_terms(caller_env, false, vr, 1, err_termp)) {
                    return 0;
                }
                if (uterm_is_tuple(vtenv, vr)) {
                    if (!etf_decode_tuple_header(caller_env, vtenv, false, vr, &arity, err_termp)) {
                        return 0;
                    }
                    if (arity == 2 && uterm_is_atom(vtenv, vr)) {
                        if (!etf_decode_atom_term(caller_env, vtenv, false, vr, &atom, err_termp)) {
                            return 0;
                        }
                        if (atom == ATOM(is_auth)) {
                            // EMIT
                            EMIT();
                            return 1;
                        }
                    }
                }
            }
        }
    }
    // LOG, DROP or REDIRECT
    LOG_DROP_OR_REDIRECT();
    return 1;
}

int
udist_classify_send_to_rex(ErlNifEnv *caller_env, vterm_env_t *vtenv, udist_t *up, bool untrusted, bool is_pass_through,
                           slice_t *payload, ERL_NIF_TERM *err_termp)
{
    // REG_SEND: rex
    //
    // Only allow messages matching the following:
    //
    //     {From, features_request}
    //     {features_reply, node(), [erpc]}
    //
    // Otherwise, redirect the message (drop it).
    vec_t vec[1];
    vec_reader_t vr[1];
    uint32_t arity;
    ERL_NIF_TERM atom = THE_NON_VALUE;
    if (payload == NULL) {
        *err_termp = EXCP_ERROR(caller_env, "Call to udist_classify_send_to_rex() failed: payload is NULL\n");
        return 0;
    }
    (void)vec_init_free(vec);
    if (!vec_create_from_slice(vec, payload->head, payload->tail)) {
        *err_termp = EXCP_ERROR(caller_env, "Call to udist_classify_send_to_rex() failed: payload is invalid\n");
        return 0;
    }
    if (!vec_reader_create(vr, vec, 0)) {
        *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_create() failed: unable to get slice for payload message\n");
        return 0;
    }
    if (is_pass_through && !vec_reader_skip_exact(vr, 1)) {
        *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_skip_exact() failed: unable to decode payload message\n");
        return 0;
    }
    if (uterm_is_tuple(vtenv, vr)) {
        if (is_pass_through && !vec_reader_back_exact(vr, 1)) {
            *err_termp = EXCP_ERROR(caller_env, "Call to vec_reader_back_exact() failed: unable to decode payload message\n");
            return 0;
        }
        if (!etf_decode_tuple_header(caller_env, vtenv, is_pass_through, vr, &arity, err_termp)) {
            return 0;
        }
        if (arity == 2) {
            if (!etf_fast_skip_terms(caller_env, false, vr, 1, err_termp)) {
                return 0;
            }
            if (uterm_is_atom(vtenv, vr)) {
                if (!etf_decode_atom_term(caller_env, vtenv, false, vr, &atom, err_termp)) {
                    return 0;
                }
                if (atom == ATOM(features_request)) {
                    // EMIT
                    EMIT();
                    return 1;
                }
            }
        } else if (arity == 3 && uterm_is_atom(vtenv, vr)) {
            if (!etf_decode_atom_term(caller_env, vtenv, false, vr, &atom, err_termp)) {
                return 0;
            }
            if (atom == ATOM(features_reply)) {
                // EMIT
                EMIT();
                return 1;
            }
        }
    }
    // LOG, DROP or REDIRECT
    LOG_DROP_OR_REDIRECT();
    return 1;
}

#undef LOG_REDIRECT_SPAWN_REQUEST
#undef LOG_DROP_OR_REDIRECT
#undef LOG_DROP
#undef LOG
#undef EMIT

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
