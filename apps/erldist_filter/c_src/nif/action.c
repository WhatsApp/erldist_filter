/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "action.h"
#include "erldist_filter_nif.h"
#include "logger/edf_logger.h"

static void action_data_emit_destroy(action_data_emit_t *data);
static void action_data_log_destroy(action_data_log_t *data);
static int action_into_term_emit(ErlNifEnv *env, action_t *actions, ERL_NIF_TERM *termp);
static int action_into_term_log(ErlNifEnv *env, action_t *actions, ERL_NIF_TERM *termp);

void
action_destroy(action_t *action)
{
    XNIF_TRACE_F("%s:%d action_destroy()\n", __FILE__, __LINE__);
    switch (action->tag) {
    case ACTION_TAG_EMIT:
        action->tag = ACTION_TAG_FREE;
        (void)action_data_emit_destroy(&action->data.emit);
        break;
    case ACTION_TAG_LOG:
        action->tag = ACTION_TAG_FREE;
        (void)action_data_log_destroy(&action->data.log);
        break;
    default:
        break;
    }
}

inline void
action_data_emit_destroy(action_data_emit_t *data)
{
    XNIF_TRACE_F("%s:%d action_data_emit_destroy()\n", __FILE__, __LINE__);
    (void)vec_destroy(&data->vec);
    return;
}

inline void
action_data_log_destroy(action_data_log_t *data)
{
    XNIF_TRACE_F("%s:%d action_data_log_destroy()\n", __FILE__, __LINE__);
    data->fragment_count = 0;
    if (data->event != NULL) {
        (void)edf_logger_event_destroy(data->event);
        data->event = NULL;
    }
    return;
}

int
action_into_term(ErlNifEnv *env, action_t *action, ERL_NIF_TERM *termp)
{
    XNIF_TRACE_F("%s:%d action_into_term()\n", __FILE__, __LINE__);
    if (action_is_emit(action)) {
        return action_into_term_emit(env, action, termp);
    } else if (action_is_log(action)) {
        return action_into_term_log(env, action, termp);
    } else {
        return 0;
    }
}

inline int
action_into_term_emit(ErlNifEnv *env, action_t *action, ERL_NIF_TERM *termp)
{
    XNIF_TRACE_F("%s:%d action_into_term_emit()\n", __FILE__, __LINE__);
    ERL_NIF_TERM action_term;
    if (!action_is_emit(action)) {
        return 0;
    }
    action_term = vec_into_binary_term(env, &action->data.emit.vec);
    action_term = enif_make_tuple2(env, ATOM(emit), action_term);
    *termp = action_term;
    (void)action_destroy(action);
    return 1;
}

inline int
action_into_term_log(ErlNifEnv *env, action_t *action, ERL_NIF_TERM *termp)
{
    XNIF_TRACE_F("%s:%d action_into_term_log()\n", __FILE__, __LINE__);
    ERL_NIF_TERM action_term;
    if (!action_is_log(action)) {
        return 0;
    }
    if (!edf_logger_event_into_term(env, action->data.log.event, &action_term)) {
        return 0;
    }
    action->data.log.event = NULL;
    action_term =
        enif_make_tuple3(env, ATOM(log), enif_make_uint64(env, (ErlNifUInt64)action->data.log.fragment_count), action_term);
    *termp = action_term;
    (void)action_destroy(action);
    return 1;
}
