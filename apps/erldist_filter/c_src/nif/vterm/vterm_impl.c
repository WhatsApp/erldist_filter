/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#define ERLDIST_FILTER_NIF_INTERNAL_API 1
#include "vterm_impl.h"
#include "../vdist/vdist.h"

#include "../etf/etf_decode.h"

typedef struct edf_btt_trap_s edf_btt_trap_t;
typedef struct edf_btt_trap_init_s edf_btt_trap_init_t;
typedef enum edf_btt_trap_state_t edf_btt_trap_state_t;

typedef void (*edf_btt_done_t)(ErlNifEnv *caller_env, edf_btt_trap_t *trap, void *arg, edf_trap_result_t *result);

enum edf_btt_trap_state_t {
    EDF_BTT_TRAP_STATE_INIT = 0,
    EDF_BTT_TRAP_STATE_DECODE_LENGTH,
    EDF_BTT_TRAP_STATE_INIT_DECODE,
    EDF_BTT_TRAP_STATE_DECODE,
    EDF_BTT_TRAP_STATE_DONE,
};

struct edf_btt_trap_s {
    edf_trap_t super;
    edf_btt_trap_state_t state;
    struct {
        edf_btt_done_t cb;
        void *arg;
    } done;
    bool is_external_term;
    edf_atom_translation_table_t attab;
    vec_t input_vec;
    ssize_t limit;
    size_t heap_size;
    vterm_env_t *vtenv;
    vterm_t vterm;
};

static ERL_NIF_TERM edf_btt_trap_open(ErlNifEnv *env, bool is_external_term, int arity, const ERL_NIF_TERM *atoms,
                                      ERL_NIF_TERM input_term, ssize_t limit, edf_btt_done_t done_cb, void *done_arg,
                                      edf_btt_trap_t **trapp);
static void edf_btt_trap_clear(ErlNifEnv *caller_env, edf_btt_trap_t *trap);
static void edf_btt_trap_dtor(ErlNifEnv *caller_env, edf_trap_t *super, void *arg);
static void edf_btt_trap_edit(ErlNifEnv *caller_env, edf_trap_t *super, void *arg, edf_trap_result_t *result);
static edf_trap_result_t edf_btt_trap_next(ErlNifEnv *caller_env, edf_trap_t *super, void *arg);
static void edf_btt_decode_term_length_callback(ErlNifEnv *caller_env, etf_decode_term_length_trap_t *child, void *arg,
                                                edf_trap_result_t *result);
static void edf_btt_decode_vterm_callback(ErlNifEnv *caller_env, etf_decode_vterm_trap_t *child, void *arg,
                                          edf_trap_result_t *result);

ERL_NIF_TERM
edf_btt_trap_open(ErlNifEnv *env, bool is_external_term, int atoms_arity, const ERL_NIF_TERM *atoms_tuple, ERL_NIF_TERM input_term,
                  ssize_t limit, edf_btt_done_t done_cb, void *done_arg, edf_btt_trap_t **trapp)
{
    edf_trap_state_t trap_state = {
        .resource = NULL,
        .acquire = NULL,
        .release = NULL,
        .dtor = edf_btt_trap_dtor,
        .edit = edf_btt_trap_edit,
        .next = edf_btt_trap_next,
        .arg = NULL,
    };
    edf_btt_trap_t *trap = NULL;
    ERL_NIF_TERM trap_term;
    int i;

    if (atoms_arity < 0 || atoms_arity > ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES) {
        return EXCP_ERROR_F(env, "AtomsTuple must have an arity between 0 and %d inclusive\n",
                            ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES);
    }

    for (i = 0; i < atoms_arity; i++) {
        if (!enif_is_atom(env, atoms_tuple[i])) {
            return EXCP_BADARG_F(env, "AtomsTuple must only contain atoms (invalid element at index=%d)\n", i);
        }
    }

    if (!enif_is_binary(env, input_term)) {
        return EXCP_ERROR(env, "InputBinary must be a binary\n");
    }

    trap_term = edf_trap_open(env, &trap_state, sizeof(edf_btt_trap_t), (edf_trap_t **)(&trap));
    if (trap == NULL) {
        return trap_term;
    }

    trap->super.state.arg = (void *)trap;
    trap->state = EDF_BTT_TRAP_STATE_INIT;
    trap->done.cb = done_cb;
    trap->done.arg = done_arg;
    trap->is_external_term = is_external_term;
    (void)edf_atom_translation_table_init(&trap->attab);
    (void)vec_init_free(&trap->input_vec);
    trap->limit = limit;
    trap->heap_size = 0;
    trap->vtenv = NULL;
    trap->vterm = NULL;

    if (!vec_create_from_bin(&trap->input_vec, input_term)) {
        return EXCP_ERROR(env, "Call to vec_create_from_bin() failed\n");
    }
    if (!edf_atom_translation_table_set_size(&trap->attab, (size_t)atoms_arity)) {
        return EXCP_ERROR(env, "Call to edf_atom_translation_table_set_size() failed\n");
    }
    for (i = 0; i < atoms_arity; i++) {
        if (!edf_atom_translation_table_set_entry(&trap->attab, 0, i, atoms_tuple[i], false)) {
            return EXCP_ERROR(env, "Call to edf_atom_translation_table_set_entry() failed\n");
        }
    }

    if (trapp != NULL) {
        *trapp = trap;
    }

    return trap_term;
}

void
edf_btt_trap_clear(ErlNifEnv *caller_env, edf_btt_trap_t *trap)
{
    XNIF_TRACE_F("%s:%d [trap] clear callback\n", __FILE__, __LINE__);

    (void)caller_env;

    if (trap->vtenv != NULL) {
        (void)vterm_env_free(trap->vtenv);
        trap->vtenv = NULL;
    }
    (void)edf_atom_translation_table_destroy(&trap->attab);
    (void)vec_destroy(&trap->input_vec);

    return;
}

void
edf_btt_trap_dtor(ErlNifEnv *caller_env, edf_trap_t *super, void *arg)
{
    edf_btt_trap_t *trap = (void *)arg;

    XNIF_TRACE_F("%s:%d [trap] dtor callback\n", __FILE__, __LINE__);

    (void)super;

    (void)edf_btt_trap_clear(caller_env, trap);

    return;
}

void
edf_btt_trap_edit(ErlNifEnv *caller_env, edf_trap_t *super, void *arg, edf_trap_result_t *result)
{
    edf_btt_trap_t *trap = (void *)arg;

    if (result->tag == EDF_TRAP_RESULT_TAG_OK && trap->state == EDF_BTT_TRAP_STATE_DONE && trap->done.cb != NULL) {
        (void)trap->done.cb(caller_env, trap, trap->done.arg, result);
    }

    return;
}

edf_trap_result_t
edf_btt_trap_next(ErlNifEnv *caller_env, edf_trap_t *super, void *arg)
{
    edf_btt_trap_t *trap = (void *)arg;

    do {
        switch (trap->state) {
        case EDF_BTT_TRAP_STATE_INIT: {
            trap->state = EDF_BTT_TRAP_STATE_DECODE_LENGTH;
            goto next_state;
        }
        case EDF_BTT_TRAP_STATE_DECODE_LENGTH: {
            edf_trap_result_t child_result;
            if (!edf_trap_has_child(&trap->super)) {
                vec_t slice;
                ERL_NIF_TERM child_trap_term;
                etf_decode_term_length_trap_t *child_trap = NULL;
                (void)vec_init_free(&slice);
                if (!vec_create_from_slice(&slice, vec_buf(&trap->input_vec), vec_buf_tail(&trap->input_vec))) {
                    return TRAP_ERR(EXCP_ERROR(caller_env, "Call to vec_create_from_slice() failed\n"));
                }
                child_trap_term = etf_decode_term_length_trap_open(caller_env, trap->is_external_term, &slice,
                                                                   edf_btt_decode_term_length_callback, (void *)trap, &child_trap);
                (void)vec_destroy(&slice);
                if (child_trap == NULL || enif_is_exception(caller_env, child_trap_term)) {
                    return TRAP_ERR(child_trap_term);
                }
                if (!edf_trap_attach_child(&trap->super, &child_trap->super)) {
                    return TRAP_ERR(EXCP_ERROR(caller_env, "Call to edf_trap_attach_child() failed\n"));
                }
            }
            child_result = edf_trap_child_next(caller_env, &trap->super);
            switch (child_result.tag) {
            case EDF_TRAP_RESULT_TAG_OK:
                (void)edf_trap_detach_child(&trap->super);
                trap->state = EDF_BTT_TRAP_STATE_INIT_DECODE;
                goto next_state;
            case EDF_TRAP_RESULT_TAG_ERR:
                (void)edf_trap_detach_child(&trap->super);
                return child_result;
            case EDF_TRAP_RESULT_TAG_YIELD:
                return child_result;
            default:
                (void)edf_trap_detach_child(&trap->super);
                return TRAP_ERR(EXCP_ERROR_F(caller_env,
                                             "Fatal error: corrupted child result while in state "
                                             "EDF_EXTERNAL_RECV_TRAP_STATE_DECODE_PAYLOAD_LENGTH (result.tag was %d)\n",
                                             (int)(child_result.tag)));
            }
        }
        case EDF_BTT_TRAP_STATE_INIT_DECODE: {
            trap->vtenv = vterm_env_prealloc(&trap->attab, trap->heap_size);
            if (trap->vtenv == NULL) {
                return TRAP_ERR(EXCP_ERROR(caller_env, "Call to vterm_env_prealloc() failed\n"));
            }
            trap->state = EDF_BTT_TRAP_STATE_DECODE;
            goto next_state;
        }
        case EDF_BTT_TRAP_STATE_DECODE: {
            edf_trap_result_t child_result;
            if (!edf_trap_has_child(&trap->super)) {
                vec_t slice;
                ERL_NIF_TERM child_trap_term;
                etf_decode_vterm_trap_t *child_trap = NULL;
                (void)vec_init_free(&slice);
                if (!vec_create_from_slice(&slice, vec_buf(&trap->input_vec), vec_buf_tail(&trap->input_vec))) {
                    return TRAP_ERR(EXCP_ERROR(caller_env, "Call to vec_create_from_slice() failed\n"));
                }
                child_trap_term = etf_decode_vterm_trap_open(caller_env, trap->vtenv, trap->is_external_term, &trap->attab, &slice,
                                                             trap->limit, edf_btt_decode_vterm_callback, (void *)trap, &child_trap);
                (void)vec_destroy(&slice);
                if (child_trap == NULL || enif_is_exception(caller_env, child_trap_term)) {
                    return TRAP_ERR(child_trap_term);
                }
                if (!edf_trap_attach_child(&trap->super, &child_trap->super)) {
                    return TRAP_ERR(EXCP_ERROR(caller_env, "Call to edf_trap_attach_child() failed\n"));
                }
            }
            child_result = edf_trap_child_next(caller_env, &trap->super);
            switch (child_result.tag) {
            case EDF_TRAP_RESULT_TAG_OK:
                (void)edf_trap_detach_child(&trap->super);
                trap->state = EDF_BTT_TRAP_STATE_DONE;
                goto next_state;
            case EDF_TRAP_RESULT_TAG_ERR:
                (void)edf_trap_detach_child(&trap->super);
                return child_result;
            case EDF_TRAP_RESULT_TAG_YIELD:
                return child_result;
            default:
                (void)edf_trap_detach_child(&trap->super);
                return TRAP_ERR(EXCP_ERROR_F(
                    caller_env,
                    "Fatal error: corrupted child result while in state EDF_BTT_TRAP_STATE_DECODE (result.tag was %d)\n",
                    (int)(child_result.tag)));
            }
        }
        case EDF_BTT_TRAP_STATE_DONE: {
            return TRAP_OK(THE_NON_VALUE);
        }
        default:
            return TRAP_ERR(EXCP_ERROR_F(caller_env, "Fatal error: unknown edf_btt_trap_t->state value %d\n", (int)(trap->state)));
        }
    next_state : {
        if (TRAP_SHOULD_YIELD(trap)) {
            return TRAP_YIELD();
        }
        continue;
    }
    } while (1);
}

void
edf_btt_decode_term_length_callback(ErlNifEnv *caller_env, etf_decode_term_length_trap_t *child, void *arg,
                                    edf_trap_result_t *result)
{
    edf_btt_trap_t *parent = (void *)arg;

    (void)caller_env;
    (void)result;

    if (result->tag != EDF_TRAP_RESULT_TAG_OK) {
        return;
    }

    parent->heap_size = child->heap_size;

    return;
}

void
edf_btt_decode_vterm_callback(ErlNifEnv *caller_env, etf_decode_vterm_trap_t *child, void *arg, edf_trap_result_t *result)
{
    edf_btt_trap_t *parent = (void *)arg;

    (void)caller_env;
    (void)result;

    if (result->tag != EDF_TRAP_RESULT_TAG_OK) {
        return;
    }

    parent->vterm = child->vterm;
    child->vtenv = NULL;

    return;
}

static ERL_NIF_TERM erldist_filter_nif_dist_any_to_vdist_x(ErlNifEnv *env, bool is_external, int atoms_arity,
                                                           const ERL_NIF_TERM *atoms_tuple, ERL_NIF_TERM input_term, ssize_t limit);
static void erldist_filter_nif_dist_any_to_vdist_x_callback(ErlNifEnv *caller_env, edf_btt_trap_t *trap, void *arg,
                                                            edf_trap_result_t *result);
static ERL_NIF_TERM erldist_filter_nif_dist_any_to_vterm_x(ErlNifEnv *env, bool is_external, int atoms_arity,
                                                           const ERL_NIF_TERM *atoms_tuple, ERL_NIF_TERM input_term, ssize_t limit);
static void erldist_filter_nif_dist_any_to_vterm_x_callback(ErlNifEnv *caller_env, edf_btt_trap_t *trap, void *arg,
                                                            edf_trap_result_t *result);

ERL_NIF_TERM
erldist_filter_nif_dist_any_to_vdist_x(ErlNifEnv *env, bool is_external, int atoms_arity, const ERL_NIF_TERM *atoms_tuple,
                                       ERL_NIF_TERM input_term, ssize_t limit)
{
    ERL_NIF_TERM trap_term = THE_NON_VALUE;
    trap_term = edf_btt_trap_open(env, is_external, atoms_arity, atoms_tuple, input_term, limit,
                                  erldist_filter_nif_dist_any_to_vdist_x_callback, NULL, NULL);
    if (enif_is_exception(env, trap_term)) {
        return trap_term;
    }
    return edf_trap_schedule_from_term(env, trap_term);
}

void
erldist_filter_nif_dist_any_to_vdist_x_callback(ErlNifEnv *caller_env, edf_btt_trap_t *trap, void *arg, edf_trap_result_t *result)
{
    ERL_NIF_TERM output_term = THE_NON_VALUE;
    vdist_dop_t dop;

    (void)arg;

    if (result->tag != EDF_TRAP_RESULT_TAG_OK) {
        return;
    }

    if (trap->vtenv == NULL) {
        output_term = EXCP_ERROR(caller_env, "Corrupted state, trap->vtenv is NULL\n");
        *result = TRAP_ERR(output_term);
        return;
    }

    (void)vdist_dop_init_free(&dop);

    if (!vdist_dop_from_vterm(trap->vtenv, &dop, &trap->vterm)) {
        output_term = EXCP_ERROR(caller_env, "Call to vdist_dop_from_vterm() failed\n");
        *result = TRAP_ERR(output_term);
        return;
    }

    if (!vdist_dop_debug_dump(caller_env, trap->vtenv, &dop, &output_term)) {
        (void)edf_btt_trap_clear(caller_env, trap);
        *result = TRAP_ERR(output_term);
        return;
    }

    (void)edf_btt_trap_clear(caller_env, trap);
    *result = TRAP_OK(output_term);
    return;
}

ERL_NIF_TERM
erldist_filter_nif_dist_any_to_vterm_x(ErlNifEnv *env, bool is_external, int atoms_arity, const ERL_NIF_TERM *atoms_tuple,
                                       ERL_NIF_TERM input_term, ssize_t limit)
{
    ERL_NIF_TERM trap_term = THE_NON_VALUE;
    trap_term = edf_btt_trap_open(env, is_external, atoms_arity, atoms_tuple, input_term, limit,
                                  erldist_filter_nif_dist_any_to_vterm_x_callback, NULL, NULL);
    if (enif_is_exception(env, trap_term)) {
        return trap_term;
    }
    return edf_trap_schedule_from_term(env, trap_term);
}

void
erldist_filter_nif_dist_any_to_vterm_x_callback(ErlNifEnv *caller_env, edf_btt_trap_t *trap, void *arg, edf_trap_result_t *result)
{
    ERL_NIF_TERM output_term = THE_NON_VALUE;

    (void)arg;

    if (result->tag != EDF_TRAP_RESULT_TAG_OK) {
        return;
    }

    if (trap->vtenv == NULL) {
        output_term = EXCP_ERROR(caller_env, "Corrupted state, trap->vtenv is NULL\n");
        *result = TRAP_ERR(output_term);
        return;
    }

    if (!vterm_debug_dump(caller_env, trap->vtenv, &trap->vterm, &output_term)) {
        (void)edf_btt_trap_clear(caller_env, trap);
        *result = TRAP_ERR(output_term);
        return;
    }

    (void)edf_btt_trap_clear(caller_env, trap);
    *result = TRAP_OK(output_term);
    return;
}

ERL_NIF_TERM
erldist_filter_nif_dist_ext_to_vdist_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int atoms_arity;
    const ERL_NIF_TERM *atoms_tuple = NULL;

    if (argc != 2) {
        return EXCP_BADARG(env, "argc must be 2");
    }

    if (!enif_is_tuple(env, argv[0]) || !enif_get_tuple(env, argv[0], &atoms_arity, &atoms_tuple)) {
        return EXCP_BADARG(env, "AtomsTuple must be a tuple containing only atoms");
    }

    if (!enif_is_binary(env, argv[1])) {
        return EXCP_BADARG(env, "InputBinary must be a binary");
    }

    return erldist_filter_nif_dist_any_to_vdist_x(env, true, atoms_arity, atoms_tuple, argv[1], -1);
}

ERL_NIF_TERM
erldist_filter_nif_dist_ext_to_vterm_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int atoms_arity;
    const ERL_NIF_TERM *atoms_tuple = NULL;

    if (argc != 2) {
        return EXCP_BADARG(env, "argc must be 2");
    }

    if (!enif_is_tuple(env, argv[0]) || !enif_get_tuple(env, argv[0], &atoms_arity, &atoms_tuple)) {
        return EXCP_BADARG(env, "AtomsTuple must be a tuple containing only atoms");
    }

    if (!enif_is_binary(env, argv[1])) {
        return EXCP_BADARG(env, "InputBinary must be a binary");
    }

    return erldist_filter_nif_dist_any_to_vterm_x(env, true, atoms_arity, atoms_tuple, argv[1], -1);
}

ERL_NIF_TERM
erldist_filter_nif_dist_ext_to_vterm_3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int atoms_arity;
    const ERL_NIF_TERM *atoms_tuple = NULL;
    ErlNifSInt64 limit = -1;

    if (argc != 3) {
        return EXCP_BADARG(env, "argc must be 3");
    }

    if (!enif_is_tuple(env, argv[0]) || !enif_get_tuple(env, argv[0], &atoms_arity, &atoms_tuple)) {
        return EXCP_BADARG(env, "AtomsTuple must be a tuple containing only atoms");
    }

    if (!enif_is_binary(env, argv[1])) {
        return EXCP_BADARG(env, "InputBinary must be a binary");
    }

    if (!enif_get_int64(env, argv[2], &limit)) {
        return EXCP_BADARG(env, "Limit must be a signed integer");
    }

    return erldist_filter_nif_dist_any_to_vterm_x(env, true, atoms_arity, atoms_tuple, argv[1], (ssize_t)limit);
}

ERL_NIF_TERM
erldist_filter_nif_dist_int_to_vdist_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int atoms_arity;
    const ERL_NIF_TERM *atoms_tuple = NULL;

    if (argc != 2) {
        return EXCP_BADARG(env, "argc must be 2");
    }

    if (!enif_is_tuple(env, argv[0]) || !enif_get_tuple(env, argv[0], &atoms_arity, &atoms_tuple)) {
        return EXCP_BADARG(env, "AtomsTuple must be a tuple containing only atoms");
    }

    if (!enif_is_binary(env, argv[1])) {
        return EXCP_BADARG(env, "InputBinary must be a binary");
    }

    return erldist_filter_nif_dist_any_to_vdist_x(env, false, atoms_arity, atoms_tuple, argv[1], -1);
}

ERL_NIF_TERM
erldist_filter_nif_dist_int_to_vterm_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int atoms_arity;
    const ERL_NIF_TERM *atoms_tuple = NULL;

    if (argc != 2) {
        return EXCP_BADARG(env, "argc must be 2");
    }

    if (!enif_is_tuple(env, argv[0]) || !enif_get_tuple(env, argv[0], &atoms_arity, &atoms_tuple)) {
        return EXCP_BADARG(env, "AtomsTuple must be a tuple containing only atoms");
    }

    if (!enif_is_binary(env, argv[1])) {
        return EXCP_BADARG(env, "InputBinary must be a binary");
    }

    return erldist_filter_nif_dist_any_to_vterm_x(env, false, atoms_arity, atoms_tuple, argv[1], -1);
}

ERL_NIF_TERM
erldist_filter_nif_dist_int_to_vterm_3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int atoms_arity;
    const ERL_NIF_TERM *atoms_tuple = NULL;
    ErlNifSInt64 limit = -1;

    if (argc != 3) {
        return EXCP_BADARG(env, "argc must be 3");
    }

    if (!enif_is_tuple(env, argv[0]) || !enif_get_tuple(env, argv[0], &atoms_arity, &atoms_tuple)) {
        return EXCP_BADARG(env, "AtomsTuple must be a tuple containing only atoms");
    }

    if (!enif_is_binary(env, argv[1])) {
        return EXCP_BADARG(env, "InputBinary must be a binary");
    }

    if (!enif_get_int64(env, argv[2], &limit)) {
        return EXCP_BADARG(env, "Limit must be a signed integer");
    }

    return erldist_filter_nif_dist_any_to_vterm_x(env, false, atoms_arity, atoms_tuple, argv[1], (ssize_t)limit);
}
