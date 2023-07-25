/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef VDIST_H
#define VDIST_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../vterm/vterm.h"

#include "../erts/dist.h"

/* Macro Definitions */

/* Type Definitions */

typedef struct vdist_dop_s vdist_dop_t;
typedef enum vdist_dop_tag_t vdist_dop_tag_t;

enum vdist_dop_tag_t {
    VDIST_DOP_TAG_FREE = 0,
    VDIST_DOP_TAG_LINK = DOP_LINK,
    VDIST_DOP_TAG_SEND = DOP_SEND,
    VDIST_DOP_TAG_EXIT = DOP_EXIT,
    VDIST_DOP_TAG_UNLINK = DOP_UNLINK,
    VDIST_DOP_TAG_REG_SEND = DOP_REG_SEND,
    VDIST_DOP_TAG_GROUP_LEADER = DOP_GROUP_LEADER,
    VDIST_DOP_TAG_EXIT2 = DOP_EXIT2,
    VDIST_DOP_TAG_SEND_TT = DOP_SEND_TT,
    VDIST_DOP_TAG_EXIT_TT = DOP_EXIT_TT,
    VDIST_DOP_TAG_REG_SEND_TT = DOP_REG_SEND_TT,
    VDIST_DOP_TAG_EXIT2_TT = DOP_EXIT2_TT,
    VDIST_DOP_TAG_MONITOR_P = DOP_MONITOR_P,
    VDIST_DOP_TAG_DEMONITOR_P = DOP_DEMONITOR_P,
    VDIST_DOP_TAG_MONITOR_P_EXIT = DOP_MONITOR_P_EXIT,
    VDIST_DOP_TAG_SEND_SENDER = DOP_SEND_SENDER,
    VDIST_DOP_TAG_SEND_SENDER_TT = DOP_SEND_SENDER_TT,
    VDIST_DOP_TAG_PAYLOAD_EXIT = DOP_PAYLOAD_EXIT,
    VDIST_DOP_TAG_PAYLOAD_EXIT_TT = DOP_PAYLOAD_EXIT_TT,
    VDIST_DOP_TAG_PAYLOAD_EXIT2 = DOP_PAYLOAD_EXIT2,
    VDIST_DOP_TAG_PAYLOAD_EXIT2_TT = DOP_PAYLOAD_EXIT2_TT,
    VDIST_DOP_TAG_PAYLOAD_MONITOR_P_EXIT = DOP_PAYLOAD_MONITOR_P_EXIT,
    VDIST_DOP_TAG_SPAWN_REQUEST = DOP_SPAWN_REQUEST,
    VDIST_DOP_TAG_SPAWN_REQUEST_TT = DOP_SPAWN_REQUEST_TT,
    VDIST_DOP_TAG_SPAWN_REPLY = DOP_SPAWN_REPLY,
    VDIST_DOP_TAG_SPAWN_REPLY_TT = DOP_SPAWN_REPLY_TT,
    VDIST_DOP_TAG_ALIAS_SEND = DOP_ALIAS_SEND,
    VDIST_DOP_TAG_ALIAS_SEND_TT = DOP_ALIAS_SEND_TT,
    VDIST_DOP_TAG_UNLINK_ID = DOP_UNLINK_ID,
    VDIST_DOP_TAG_UNLINK_ID_ACK = DOP_UNLINK_ID_ACK,
};

typedef struct vdist_dop_data_link_s vdist_dop_data_link_t;
typedef struct vdist_dop_data_send_s vdist_dop_data_send_t;
typedef struct vdist_dop_data_exit_s vdist_dop_data_exit_t;
typedef struct vdist_dop_data_unlink_s vdist_dop_data_unlink_t;
typedef struct vdist_dop_data_reg_send_s vdist_dop_data_reg_send_t;
typedef struct vdist_dop_data_group_leader_s vdist_dop_data_group_leader_t;
typedef struct vdist_dop_data_exit2_s vdist_dop_data_exit2_t;
typedef struct vdist_dop_data_send_tt_s vdist_dop_data_send_tt_t;
typedef struct vdist_dop_data_exit_tt_s vdist_dop_data_exit_tt_t;
typedef struct vdist_dop_data_reg_send_tt_s vdist_dop_data_reg_send_tt_t;
typedef struct vdist_dop_data_exit2_tt_s vdist_dop_data_exit2_tt_t;
typedef struct vdist_dop_data_monitor_p_s vdist_dop_data_monitor_p_t;
typedef struct vdist_dop_data_demonitor_p_s vdist_dop_data_demonitor_p_t;
typedef struct vdist_dop_data_monitor_p_exit_s vdist_dop_data_monitor_p_exit_t;
typedef struct vdist_dop_data_send_sender_s vdist_dop_data_send_sender_t;
typedef struct vdist_dop_data_send_sender_tt_s vdist_dop_data_send_sender_tt_t;
typedef struct vdist_dop_data_payload_exit_s vdist_dop_data_payload_exit_t;
typedef struct vdist_dop_data_payload_exit_tt_s vdist_dop_data_payload_exit_tt_t;
typedef struct vdist_dop_data_payload_exit2_s vdist_dop_data_payload_exit2_t;
typedef struct vdist_dop_data_payload_exit2_tt_s vdist_dop_data_payload_exit2_tt_t;
typedef struct vdist_dop_data_payload_monitor_p_exit_s vdist_dop_data_payload_monitor_p_exit_t;
typedef struct vdist_dop_data_spawn_request_s vdist_dop_data_spawn_request_t;
typedef struct vdist_dop_data_spawn_request_tt_s vdist_dop_data_spawn_request_tt_t;
typedef struct vdist_dop_data_spawn_reply_s vdist_dop_data_spawn_reply_t;
typedef struct vdist_dop_data_spawn_reply_tt_s vdist_dop_data_spawn_reply_tt_t;
typedef struct vdist_dop_data_alias_send_s vdist_dop_data_alias_send_t;
typedef struct vdist_dop_data_alias_send_tt_s vdist_dop_data_alias_send_tt_t;
typedef struct vdist_dop_data_unlink_id_s vdist_dop_data_unlink_id_t;
typedef struct vdist_dop_data_unlink_id_ack_s vdist_dop_data_unlink_id_ack_t;

struct vdist_dop_data_link_s {
    vterm_t from_pid; // vterm:pid_t()
    vterm_t to_pid;   // vterm:pid_t()
};

struct vdist_dop_data_send_s {
    vterm_t unused; // vterm:t()
    vterm_t to_pid; // vterm:pid_t()
};

struct vdist_dop_data_exit_s {
    vterm_t from_pid; // vterm:pid_t()
    vterm_t to_pid;   // vterm:pid_t()
    vterm_t reason;   // vterm:t()
};

struct vdist_dop_data_unlink_s {
    vterm_t from_pid; // vterm:pid_t()
    vterm_t to_pid;   // vterm:pid_t()
};

struct vdist_dop_data_reg_send_s {
    vterm_t from_pid; // vterm:pid_t()
    vterm_t unused;   // vterm:t()
    vterm_t to_name;  // vterm:atom_t()
};

struct vdist_dop_data_group_leader_s {
    vterm_t from_pid; // vterm:pid_t()
    vterm_t to_pid;   // vterm:pid_t()
};

struct vdist_dop_data_exit2_s {
    vterm_t from_pid; // vterm:pid_t()
    vterm_t to_pid;   // vterm:pid_t()
    vterm_t reason;   // vterm:t()
};

struct vdist_dop_data_send_tt_s {
    vterm_t unused;      // vterm:t()
    vterm_t to_pid;      // vterm:pid_t()
    vterm_t trace_token; // vterm:t()
};

struct vdist_dop_data_exit_tt_s {
    vterm_t from_pid;    // vterm:pid_t()
    vterm_t to_pid;      // vterm:pid_t()
    vterm_t trace_token; // vterm:t()
    vterm_t reason;      // vterm:t()
};

struct vdist_dop_data_reg_send_tt_s {
    vterm_t from_pid;    // vterm:pid_t()
    vterm_t unused;      // vterm:t()
    vterm_t to_name;     // vterm:atom_t()
    vterm_t trace_token; // vterm:t()
};

struct vdist_dop_data_exit2_tt_s {
    vterm_t from_pid;    // vterm:pid_t()
    vterm_t to_pid;      // vterm:pid_t()
    vterm_t trace_token; // vterm:t()
    vterm_t reason;      // vterm:t()
};

struct vdist_dop_data_monitor_p_s {
    vterm_t from_pid; // vterm:pid_t()
    vterm_t to_proc;  // vterm:atom_t() | vterm:pid_t()
    vterm_t ref;      // vterm:reference_t()
};

struct vdist_dop_data_demonitor_p_s {
    vterm_t from_pid; // vterm:pid_t()
    vterm_t to_proc;  // vterm:atom_t() | vterm:pid_t()
    vterm_t ref;      // vterm:reference_t()
};

struct vdist_dop_data_monitor_p_exit_s {
    vterm_t from_proc; // vterm:atom_t() | vterm:pid_t()
    vterm_t to_pid;    // vterm:pid_t()
    vterm_t ref;       // vterm:reference_t()
    vterm_t reason;    // vterm:t()
};

struct vdist_dop_data_send_sender_s {
    vterm_t from_pid; // vterm:pid_t()
    vterm_t to_pid;   // vterm:pid_t()
};

struct vdist_dop_data_send_sender_tt_s {
    vterm_t from_pid;    // vterm:pid_t()
    vterm_t to_pid;      // vterm:pid_t()
    vterm_t trace_token; // vterm:t()
};

struct vdist_dop_data_payload_exit_s {
    vterm_t from_pid; // vterm:pid_t()
    vterm_t to_pid;   // vterm:pid_t()
};

struct vdist_dop_data_payload_exit_tt_s {
    vterm_t from_pid;    // vterm:pid_t()
    vterm_t to_pid;      // vterm:pid_t()
    vterm_t trace_token; // vterm:t()
};

struct vdist_dop_data_payload_exit2_s {
    vterm_t from_pid; // vterm:pid_t()
    vterm_t to_pid;   // vterm:pid_t()
};

struct vdist_dop_data_payload_exit2_tt_s {
    vterm_t from_pid;    // vterm:pid_t()
    vterm_t to_pid;      // vterm:pid_t()
    vterm_t trace_token; // vterm:t()
};

struct vdist_dop_data_payload_monitor_p_exit_s {
    vterm_t from_proc; // vterm:atom_t() | vterm:pid_t()
    vterm_t to_pid;    // vterm:pid_t()
    vterm_t ref;       // vterm:reference_t()
};

struct vdist_dop_data_spawn_request_s {
    vterm_t req_id;       // vterm:reference_t()
    vterm_t from;         // vterm:pid_t()
    vterm_t group_leader; // vterm:pid_t()
    struct {
        vterm_t module;   // vterm:atom_t()
        vterm_t function; // vterm:atom_t()
        vterm_t arity;    // vterm:fixed_integer_t()
    } mfa;                // vterm:tuple_t()
    vterm_t opt_list;     // [vterm:t()]
};

struct vdist_dop_data_spawn_request_tt_s {
    vterm_t req_id;       // vterm:reference_t()
    vterm_t from;         // vterm:pid_t()
    vterm_t group_leader; // vterm:pid_t()
    struct {
        vterm_t module;   // vterm:atom_t()
        vterm_t function; // vterm:atom_t()
        vterm_t arity;    // vterm:fixed_integer_t()
    } mfa;                // vterm:tuple_t()
    vterm_t opt_list;     // [vterm:t()]
    vterm_t token;        // vterm:t()
};

struct vdist_dop_data_spawn_reply_s {
    vterm_t req_id; // vterm:reference_t()
    vterm_t to;     // vterm:pid_t()
    vterm_t flags;  // vterm:fixed_integer_t()
    vterm_t result; // vterm:atom_t() | vterm:pid_t()
};

struct vdist_dop_data_spawn_reply_tt_s {
    vterm_t req_id; // vterm:reference_t()
    vterm_t to;     // vterm:pid_t()
    vterm_t flags;  // vterm:fixed_integer_t()
    vterm_t result; // vterm:atom_t() | vterm:pid_t()
    vterm_t token;  // vterm:t()
};

struct vdist_dop_data_alias_send_s {
    vterm_t from_pid; // vterm:pid_t()
    vterm_t alias;    // vterm:reference_t()
};

struct vdist_dop_data_alias_send_tt_s {
    vterm_t from_pid; // vterm:pid_t()
    vterm_t alias;    // vterm:reference_t()
    vterm_t token;    // vterm:t()
};

struct vdist_dop_data_unlink_id_s {
    vterm_t id;       // vterm:integer_t()
    vterm_t from_pid; // vterm:pid_t()
    vterm_t to_pid;   // vterm:pid_t()
};

struct vdist_dop_data_unlink_id_ack_s {
    vterm_t id;       // vterm:integer_t()
    vterm_t from_pid; // vterm:pid_t()
    vterm_t to_pid;   // vterm:pid_t()
};

struct vdist_dop_s {
    vdist_dop_tag_t tag;
    union {
        vdist_dop_data_link_t link;
        vdist_dop_data_send_t send;
        vdist_dop_data_exit_t exit;
        vdist_dop_data_unlink_t unlink;
        vdist_dop_data_reg_send_t reg_send;
        vdist_dop_data_group_leader_t group_leader;
        vdist_dop_data_exit2_t exit2;
        vdist_dop_data_send_tt_t send_tt;
        vdist_dop_data_exit_tt_t exit_tt;
        vdist_dop_data_reg_send_tt_t reg_send_tt;
        vdist_dop_data_exit2_tt_t exit2_tt;
        vdist_dop_data_monitor_p_t monitor_p;
        vdist_dop_data_demonitor_p_t demonitor_p;
        vdist_dop_data_monitor_p_exit_t monitor_p_exit;
        vdist_dop_data_send_sender_t send_sender;
        vdist_dop_data_send_sender_tt_t send_sender_tt;
        vdist_dop_data_payload_exit_t payload_exit;
        vdist_dop_data_payload_exit_tt_t payload_exit_tt;
        vdist_dop_data_payload_exit2_t payload_exit2;
        vdist_dop_data_payload_exit2_tt_t payload_exit2_tt;
        vdist_dop_data_payload_monitor_p_exit_t payload_monitor_p_exit;
        vdist_dop_data_spawn_request_t spawn_request;
        vdist_dop_data_spawn_request_tt_t spawn_request_tt;
        vdist_dop_data_spawn_reply_t spawn_reply;
        vdist_dop_data_spawn_reply_tt_t spawn_reply_tt;
        vdist_dop_data_alias_send_t alias_send;
        vdist_dop_data_alias_send_tt_t alias_send_tt;
        vdist_dop_data_unlink_id_t unlink_id;
        vdist_dop_data_unlink_id_ack_t unlink_id_ack;
    } data;
};

/* Function Declarations */

static int vdist_dop_init_free(vdist_dop_t *dop);
static int vdist_dop_has_payload(const vdist_dop_t *dop);
extern int vdist_dop_from_vterm(vterm_env_t *vtenv, vdist_dop_t *dop, vterm_t *vtp);
extern int vdist_dop_debug_dump(ErlNifEnv *env, vterm_env_t *vtenv, vdist_dop_t *dop, ERL_NIF_TERM *term);
extern int vdist_dop_get_name(const vdist_dop_t *dop, const char **name);

static int vdist_dop_is_free(const vdist_dop_t *dop);
static int vdist_dop_is_link(const vdist_dop_t *dop);
static int vdist_dop_is_send(const vdist_dop_t *dop);
static int vdist_dop_is_exit(const vdist_dop_t *dop);
static int vdist_dop_is_unlink(const vdist_dop_t *dop);
static int vdist_dop_is_reg_send(const vdist_dop_t *dop);
static int vdist_dop_is_group_leader(const vdist_dop_t *dop);
static int vdist_dop_is_exit2(const vdist_dop_t *dop);
static int vdist_dop_is_send_tt(const vdist_dop_t *dop);
static int vdist_dop_is_exit_tt(const vdist_dop_t *dop);
static int vdist_dop_is_reg_send_tt(const vdist_dop_t *dop);
static int vdist_dop_is_exit2_tt(const vdist_dop_t *dop);
static int vdist_dop_is_monitor_p(const vdist_dop_t *dop);
static int vdist_dop_is_demonitor_p(const vdist_dop_t *dop);
static int vdist_dop_is_monitor_p_exit(const vdist_dop_t *dop);
static int vdist_dop_is_send_sender(const vdist_dop_t *dop);
static int vdist_dop_is_send_sender_tt(const vdist_dop_t *dop);
static int vdist_dop_is_payload_exit(const vdist_dop_t *dop);
static int vdist_dop_is_payload_exit_tt(const vdist_dop_t *dop);
static int vdist_dop_is_payload_exit2(const vdist_dop_t *dop);
static int vdist_dop_is_payload_exit2_tt(const vdist_dop_t *dop);
static int vdist_dop_is_payload_monitor_p_exit(const vdist_dop_t *dop);
static int vdist_dop_is_spawn_request(const vdist_dop_t *dop);
static int vdist_dop_is_spawn_request_tt(const vdist_dop_t *dop);
static int vdist_dop_is_spawn_reply(const vdist_dop_t *dop);
static int vdist_dop_is_spawn_reply_tt(const vdist_dop_t *dop);
static int vdist_dop_is_alias_send(const vdist_dop_t *dop);
static int vdist_dop_is_alias_send_tt(const vdist_dop_t *dop);
static int vdist_dop_is_unlink_id(const vdist_dop_t *dop);
static int vdist_dop_is_unlink_id_ack(const vdist_dop_t *dop);

/* Inline Function Definitions */

inline int
vdist_dop_init_free(vdist_dop_t *dop)
{
    if (dop == NULL) {
        return 0;
    }
    dop->tag = VDIST_DOP_TAG_FREE;
    return 1;
}

inline int
vdist_dop_has_payload(const vdist_dop_t *dop)
{
    if (dop == NULL) {
        return 0;
    }
    switch (dop->tag) {
    case VDIST_DOP_TAG_SEND:
        [[fallthrough]];
    case VDIST_DOP_TAG_REG_SEND:
        [[fallthrough]];
    case VDIST_DOP_TAG_SEND_TT:
        [[fallthrough]];
    case VDIST_DOP_TAG_REG_SEND_TT:
        [[fallthrough]];
    case VDIST_DOP_TAG_SEND_SENDER:
        [[fallthrough]];
    case VDIST_DOP_TAG_SEND_SENDER_TT:
        [[fallthrough]];
    case VDIST_DOP_TAG_PAYLOAD_EXIT:
        [[fallthrough]];
    case VDIST_DOP_TAG_PAYLOAD_EXIT_TT:
        [[fallthrough]];
    case VDIST_DOP_TAG_PAYLOAD_EXIT2:
        [[fallthrough]];
    case VDIST_DOP_TAG_PAYLOAD_EXIT2_TT:
        [[fallthrough]];
    case VDIST_DOP_TAG_PAYLOAD_MONITOR_P_EXIT:
        [[fallthrough]];
    case VDIST_DOP_TAG_SPAWN_REQUEST:
        [[fallthrough]];
    case VDIST_DOP_TAG_SPAWN_REQUEST_TT:
        [[fallthrough]];
    case VDIST_DOP_TAG_ALIAS_SEND:
        [[fallthrough]];
    case VDIST_DOP_TAG_ALIAS_SEND_TT:
        return 1;
    default:
        return 0;
    }
}

inline int
vdist_dop_is_free(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_FREE);
}

inline int
vdist_dop_is_link(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_LINK);
}

inline int
vdist_dop_is_send(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_SEND);
}

inline int
vdist_dop_is_exit(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_EXIT);
}

inline int
vdist_dop_is_unlink(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_UNLINK);
}

inline int
vdist_dop_is_reg_send(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_REG_SEND);
}

inline int
vdist_dop_is_group_leader(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_GROUP_LEADER);
}

inline int
vdist_dop_is_exit2(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_EXIT2);
}

inline int
vdist_dop_is_send_tt(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_SEND_TT);
}

inline int
vdist_dop_is_exit_tt(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_EXIT_TT);
}

inline int
vdist_dop_is_reg_send_tt(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_REG_SEND_TT);
}

inline int
vdist_dop_is_exit2_tt(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_EXIT2_TT);
}

inline int
vdist_dop_is_monitor_p(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_MONITOR_P);
}

inline int
vdist_dop_is_demonitor_p(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_DEMONITOR_P);
}

inline int
vdist_dop_is_monitor_p_exit(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_MONITOR_P_EXIT);
}

inline int
vdist_dop_is_send_sender(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_SEND_SENDER);
}

inline int
vdist_dop_is_send_sender_tt(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_SEND_SENDER_TT);
}

inline int
vdist_dop_is_payload_exit(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_PAYLOAD_EXIT);
}

inline int
vdist_dop_is_payload_exit_tt(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_PAYLOAD_EXIT_TT);
}

inline int
vdist_dop_is_payload_exit2(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_PAYLOAD_EXIT2);
}

inline int
vdist_dop_is_payload_exit2_tt(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_PAYLOAD_EXIT2_TT);
}

inline int
vdist_dop_is_payload_monitor_p_exit(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_PAYLOAD_MONITOR_P_EXIT);
}

inline int
vdist_dop_is_spawn_request(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_SPAWN_REQUEST);
}

inline int
vdist_dop_is_spawn_request_tt(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_SPAWN_REQUEST_TT);
}

inline int
vdist_dop_is_spawn_reply(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_SPAWN_REPLY);
}

inline int
vdist_dop_is_spawn_reply_tt(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_SPAWN_REPLY_TT);
}

inline int
vdist_dop_is_alias_send(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_ALIAS_SEND);
}

inline int
vdist_dop_is_alias_send_tt(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_ALIAS_SEND_TT);
}

inline int
vdist_dop_is_unlink_id(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_UNLINK_ID);
}

inline int
vdist_dop_is_unlink_id_ack(const vdist_dop_t *dop)
{
    return (dop != NULL && dop->tag == VDIST_DOP_TAG_UNLINK_ID_ACK);
}

#ifdef __cplusplus
}
#endif

#endif
