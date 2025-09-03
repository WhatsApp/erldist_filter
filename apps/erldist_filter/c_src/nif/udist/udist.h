/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef UDIST_H
#define UDIST_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../erldist_filter_nif.h"

#include "../erts/dist.h"
#include "../../primitive/slice.h"

/* Macro Definitions */

#define UDIST_CLASSIFY_FLAG_NONE (0)
#define UDIST_CLASSIFY_FLAG_DROP (1 << 0)
#define UDIST_CLASSIFY_FLAG_EMIT (1 << 1)
#define UDIST_CLASSIFY_FLAG_LOG_EVENT (1 << 2)
#define UDIST_CLASSIFY_FLAG_REDIRECT_DOP (1 << 3)
#define UDIST_CLASSIFY_FLAG_REDIRECT_SPAWN_REQUEST (1 << 4)

/* Type Definitions */

// Don't include "edf_channel.h", just reference the type here.
typedef struct edf_channel_stats_s edf_channel_stats_t;

// Don't include "edf_channel.h", just reference the type here.
typedef struct edf_channel_stats_dop_s edf_channel_stats_dop_t;

enum udist_control_tag_t {
    UDIST_CONTROL_TAG_NONE = 0,
    UDIST_CONTROL_TAG_EXIT,
    UDIST_CONTROL_TAG_EXIT2,
    UDIST_CONTROL_TAG_GROUP_LEADER,
    UDIST_CONTROL_TAG_LINK,
    UDIST_CONTROL_TAG_MONITOR_RELATED,
    UDIST_CONTROL_TAG_SEND_TO_ALIAS,
    UDIST_CONTROL_TAG_SEND_TO_NAME,
    UDIST_CONTROL_TAG_SEND_TO_PID,
    UDIST_CONTROL_TAG_SPAWN_REPLY,
    UDIST_CONTROL_TAG_SPAWN_REQUEST,
    UDIST_CONTROL_TAG_UNLINK,
};

typedef struct udist_s udist_t;
typedef struct udist_info_s udist_info_t;
typedef struct udist_control_s udist_control_t;
typedef enum udist_control_tag_t udist_control_tag_t;

struct udist_info_s {
    enum dop dop;
    uint32_t arity;
    int token_offset;
    bool payload;
};

struct udist_control_s {
    udist_control_tag_t tag;
    union {
        struct {
            int32_t flags;
            ERL_NIF_TERM to;
        } send;
        struct {
            ERL_NIF_TERM module;
            ERL_NIF_TERM function;
            int32_t arity;
        } spawn_request;
    } data;
};

struct udist_s {
    udist_info_t info;
    udist_control_t control;
    int flags;
};

// Don't include "vterm_env.h", just reference the type here.
typedef struct vterm_env_s vterm_env_t;

/* Function Declarations */

static void udist_init(udist_t *up);
extern int udist_classify(ErlNifEnv *caller_env, vterm_env_t *vtenv, udist_t *up, bool untrusted, bool is_pass_through,
                          slice_t *payload, ERL_NIF_TERM *err_termp);
extern int udist_get_channel_stats_dop(udist_t *up, edf_channel_stats_t *stats, edf_channel_stats_dop_t **statsdopp);
extern int udist_get_dop_string(const udist_t *up, const char **name);
static bool udist_control_is_exit(const udist_t *up);
static bool udist_control_is_exit2(const udist_t *up);
static bool udist_control_is_group_leader(const udist_t *up);
static bool udist_control_is_link(const udist_t *up);
static bool udist_control_is_monitor_related(const udist_t *up);
static bool udist_control_is_send(const udist_t *up);
static bool udist_control_is_send_to_alias(const udist_t *up);
static bool udist_control_is_send_to_name(const udist_t *up);
static bool udist_control_is_send_to_pid(const udist_t *up);
static bool udist_control_is_spawn_reply(const udist_t *up);
static bool udist_control_is_spawn_request(const udist_t *up);

/* Inline Function Definitions */

inline void
udist_init(udist_t *up)
{
    up->info.dop = DOP_UNKNOWN;
    up->info.arity = 0;
    up->info.token_offset = -1;
    up->info.payload = false;
    up->control.tag = UDIST_CONTROL_TAG_NONE;
    up->flags = UDIST_CLASSIFY_FLAG_NONE;
    return;
}

inline bool
udist_control_is_exit(const udist_t *up)
{
    return (up->control.tag == UDIST_CONTROL_TAG_EXIT);
}

inline bool
udist_control_is_exit2(const udist_t *up)
{
    return (up->control.tag == UDIST_CONTROL_TAG_EXIT2);
}

inline bool
udist_control_is_group_leader(const udist_t *up)
{
    return (up->control.tag == UDIST_CONTROL_TAG_GROUP_LEADER);
}

inline bool
udist_control_is_link(const udist_t *up)
{
    return (up->control.tag == UDIST_CONTROL_TAG_LINK);
}

inline bool
udist_control_is_monitor_related(const udist_t *up)
{
    return (up->control.tag == UDIST_CONTROL_TAG_MONITOR_RELATED);
}

inline bool
udist_control_is_send(const udist_t *up)
{
    switch (up->control.tag) {
    case UDIST_CONTROL_TAG_SEND_TO_ALIAS:
        [[fallthrough]];
    case UDIST_CONTROL_TAG_SEND_TO_NAME:
        [[fallthrough]];
    case UDIST_CONTROL_TAG_SEND_TO_PID:
        return true;
    default:
        return false;
    }
}

inline bool
udist_control_is_send_to_alias(const udist_t *up)
{
    return (up->control.tag == UDIST_CONTROL_TAG_SEND_TO_ALIAS);
}

inline bool
udist_control_is_send_to_name(const udist_t *up)
{
    return (up->control.tag == UDIST_CONTROL_TAG_SEND_TO_NAME);
}

inline bool
udist_control_is_send_to_pid(const udist_t *up)
{
    return (up->control.tag == UDIST_CONTROL_TAG_SEND_TO_PID);
}

inline bool
udist_control_is_spawn_reply(const udist_t *up)
{
    return (up->control.tag == UDIST_CONTROL_TAG_SPAWN_REPLY);
}

inline bool
udist_control_is_spawn_request(const udist_t *up)
{
    return (up->control.tag == UDIST_CONTROL_TAG_SPAWN_REQUEST);
}

inline bool
udist_control_is_unlink(const udist_t *up)
{
    return (up->control.tag == UDIST_CONTROL_TAG_UNLINK);
}

#ifdef __cplusplus
}
#endif

#endif
