/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef ACTION_H
#define ACTION_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../primitive/portable_endian.h"

#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "erl_nif_trampoline.h"

#include "core/xnif_trace.h"
#include "vec.h"

/* Macros */

#ifndef THE_NON_VALUE
#define THE_NON_VALUE ((ERL_NIF_TERM)0)
#endif

/* Type Definitions */

// Don't include "edf_logger.h", just reference the type here.
typedef struct edf_logger_event_s edf_logger_event_t;

enum action_tag_t {
    ACTION_TAG_FREE = 0,
    ACTION_TAG_EMIT,
    ACTION_TAG_LOG,
};

typedef struct action_s action_t;
typedef enum action_tag_t action_tag_t;
typedef struct action_data_emit_s action_data_emit_t;
typedef struct action_data_log_s action_data_log_t;

struct action_data_emit_s {
    vec_t vec;
};

struct action_data_log_s {
    uint64_t fragment_count;
    edf_logger_event_t *event;
};

struct action_s {
    action_tag_t tag;
    union {
        action_data_emit_t emit;
        action_data_log_t log;
    } data;
};

/* Function Declarations */

static int action_init_free(action_t *action);
static int action_create_emit(action_t *action, vec_t *vec);
static int action_create_log(action_t *action, uint64_t fragment_count, edf_logger_event_t *event);
extern void action_destroy(action_t *action);

extern int action_into_term(ErlNifEnv *env, action_t *action, ERL_NIF_TERM *termp);
static int action_is_free(const action_t *action);
static int action_is_emit(const action_t *action);
static int action_is_log(const action_t *action);

/* Inline Function Definitions */

inline int
action_init_free(action_t *action)
{
    XNIF_TRACE_F("%s:%d action_init_free()\n", __FILE__, __LINE__);
    action->tag = ACTION_TAG_FREE;
    return 1;
}

inline int
action_create_emit(action_t *action, vec_t *vec)
{
    if (!action_is_free(action)) {
        return 0;
    }
    (void)vec_init_free(&action->data.emit.vec);
    if (!vec_into_owned(&action->data.emit.vec, vec)) {
        return 0;
    }
    action->tag = ACTION_TAG_EMIT;
    return 1;
}

inline int
action_create_log(action_t *action, uint64_t fragment_count, edf_logger_event_t *event)
{
    if (!action_is_free(action)) {
        return 0;
    }
    action->data.log.fragment_count = fragment_count;
    action->data.log.event = event;
    action->tag = ACTION_TAG_LOG;
    return 1;
}

inline int
action_is_free(const action_t *action)
{
    return (action->tag == ACTION_TAG_FREE);
}

inline int
action_is_emit(const action_t *action)
{
    return (action->tag == ACTION_TAG_EMIT);
}

inline int
action_is_log(const action_t *action)
{
    return (action->tag == ACTION_TAG_LOG);
}

#ifdef __cplusplus
}
#endif

#endif
