/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef ETF_DECODE_VTERM_H
#define ETF_DECODE_VTERM_H

#ifdef __cplusplus
extern "C" {
#endif

#include "etf_common.h"
#include "../vterm/vterm.h"

/* Macro Definitions */

/* Type Definitions */

typedef struct etf_decode_vterm_trap_s etf_decode_vterm_trap_t;
typedef enum etf_decode_vterm_trap_state_t etf_decode_vterm_trap_state_t;

typedef void (*etf_decode_vterm_done_t)(ErlNifEnv *caller_env, etf_decode_vterm_trap_t *trap, void *arg, edf_trap_result_t *result);

enum etf_decode_vterm_trap_state_t {
    ETF_DECODE_VTERM_TRAP_STATE_INIT = 0,
    ETF_DECODE_VTERM_TRAP_STATE_VERSION_MAGIC,
    ETF_DECODE_VTERM_TRAP_STATE_DECODE,
    ETF_DECODE_VTERM_TRAP_STATE_DECODE_SKIP,
    ETF_DECODE_VTERM_TRAP_STATE_DONE,
};

struct etf_decode_vterm_trap_s {
    edf_trap_t super;
    etf_decode_vterm_trap_state_t state;
    bool is_external_term;
    edf_atom_translation_table_t *attab;
    vec_t slice;
    vec_reader_t reader;
    ssize_t limit;
    struct {
        etf_decode_vterm_done_t cb;
        void *arg;
    } done;
    vterm_env_t *vtenv;
    vterm_t vterm;
    vterm_t *objp;
    vterm_t *next;
};

/* Function Declarations */

extern ERL_NIF_TERM etf_decode_vterm_trap_open(ErlNifEnv *env, vterm_env_t *vtenv, bool is_external_term,
                                               edf_atom_translation_table_t *attab, vec_t *slice, ssize_t limit,
                                               etf_decode_vterm_done_t done_cb, void *done_arg, etf_decode_vterm_trap_t **trapp);
extern edf_trap_result_t etf_decode_vterm_blocking(ErlNifEnv *caller_env, vterm_env_t *vtenv, bool is_external_term,
                                                   edf_atom_translation_table_t *attab, vec_t *slice, ssize_t limit,
                                                   etf_decode_vterm_done_t done_cb, void *done_arg);
extern int etf_decode_atom_term(ErlNifEnv *caller_env, vterm_env_t *vtenv, bool is_external_term, vec_reader_t *vr,
                                ERL_NIF_TERM *atomp, ERL_NIF_TERM *err_termp);
extern int etf_decode_fixed_integer(ErlNifEnv *caller_env, vterm_env_t *vtenv, bool is_external_term, vec_reader_t *vr,
                                    int32_t *integerp, ERL_NIF_TERM *err_termp);
extern int etf_decode_list_header(ErlNifEnv *caller_env, vterm_env_t *vtenv, bool is_external_term, vec_reader_t *orig_vr,
                                  bool *is_nilp, uint32_t *lengthp, ERL_NIF_TERM *err_termp);
extern int etf_decode_pid_term(ErlNifEnv *caller_env, vterm_env_t *vtenv, bool is_external_term, vec_reader_t *vr,
                               ERL_NIF_TERM *pidp, ERL_NIF_TERM *err_termp);
extern int etf_decode_port_term(ErlNifEnv *caller_env, vterm_env_t *vtenv, bool is_external_term, vec_reader_t *vr,
                                ERL_NIF_TERM *portp, ERL_NIF_TERM *err_termp);
extern int etf_decode_reference_term(ErlNifEnv *caller_env, vterm_env_t *vtenv, bool is_external_term, vec_reader_t *vr,
                                     ERL_NIF_TERM *refp, ERL_NIF_TERM *err_termp);
extern int etf_decode_tuple_header(ErlNifEnv *caller_env, vterm_env_t *vtenv, bool is_external_term, vec_reader_t *vr,
                                   uint32_t *arityp, ERL_NIF_TERM *err_termp);

/* Inline Function Definitions */

#ifdef __cplusplus
}
#endif

#endif
