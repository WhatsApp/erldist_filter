/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "edf_trap_impl.h"

static int edf_trap_resource_acquire(ErlNifEnv *caller_env, ERL_NIF_TERM trap_term, edf_trap_t **trapp, ERL_NIF_TERM *error_term);
static void edf_trap_resource_release(ErlNifEnv *caller_env, edf_trap_t **trapp);

ERL_NIF_TERM
erldist_filter_nif_trap_2_continue(ErlNifEnv *caller_env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM error_term;
    ERL_NIF_TERM trap_term;
    ERL_NIF_TERM keep_term;
    edf_trap_t *trap = NULL;
    edf_trap_t *unsafe_trap = NULL;
    edf_trap_result_t result;

    XNIF_TRACE_F("%s:%d ENTER\n", __FILE__, __LINE__);
    if (argc != 2) {
        return EXCP_BADARG(caller_env, "argc must be 2");
    }
    error_term = THE_NON_VALUE;
    trap_term = argv[0];
    keep_term = argv[1];
    if (!edf_trap_resource_acquire(caller_env, trap_term, &trap, &error_term)) {
        return error_term;
    }
    trap->keep_term = keep_term;
    if (trap->state.next == NULL) {
        (void)edf_trap_resource_release(caller_env, &trap);
        error_term = EXCP_BADARG(caller_env, "Trap Resource reference is invalid: trap->state.next is NULL");
        return error_term;
    }
    trap->reds = REDUCTIONS_UNTIL_YCF_YIELD();
    result = trap->state.next(caller_env, trap, trap->state.arg);
    if (trap->reds < 0 || trap->reds > REDUCTIONS_UNTIL_YCF_YIELD()) {
        trap->reds = 0;
    }
    unsafe_trap = trap;
    (void)edf_trap_resource_release(caller_env, &trap);
    if (unsafe_trap->state.edit != NULL) {
        (void)unsafe_trap->state.edit(caller_env, unsafe_trap, unsafe_trap->state.arg, &result);
    }
    keep_term = unsafe_trap->keep_term;
    unsafe_trap->keep_term = THE_NON_VALUE;
    switch (result.tag) {
    case EDF_TRAP_RESULT_TAG_OK:
        XNIF_TRACE_F("%s:%d OK\n", __FILE__, __LINE__);
        BUMP_REMAINING_REDS(caller_env, unsafe_trap->reds);
        if (result.ok == THE_NON_VALUE) {
            result = TRAP_ERR(
                EXCP_ERROR(caller_env, "Trap Resource result tag was OK, but term was THE_NON_VALUE: this should never happen\n"));
            return result.err;
        }
        return result.ok;
    case EDF_TRAP_RESULT_TAG_ERR:
        BUMP_REMAINING_REDS(caller_env, unsafe_trap->reds);
        XNIF_TRACE_F("got an error...\n");
        XNIF_TRACE_F("error was: %T\n", result.err);
        {
            ERL_NIF_TERM reason;
            if (enif_has_pending_exception(caller_env, &reason)) {
                XNIF_TRACE_F("exception was: %T\n", reason);
            }
        }
        return result.err;
    case EDF_TRAP_RESULT_TAG_YIELD:
        XNIF_TRACE_F("%s:%d YIELD\n", __FILE__, __LINE__);
        BUMP_ALL_REDS(caller_env);
        if (result.yield == THE_NON_VALUE) {
            // TRAP_YIELD() was used, yield using the existing trap_term.
            result.yield = trap_term;
        }
        return edf_trap_schedule_from_term_x(caller_env, result.yield, keep_term);
    default:
        return EXCP_ERROR_F(caller_env, "Trap Resource result tag is invalid: %d\n", result.tag);
    }
}

inline int
edf_trap_resource_acquire(ErlNifEnv *caller_env, ERL_NIF_TERM trap_term, edf_trap_t **trapp, ERL_NIF_TERM *error_term)
{
    edf_trap_t *trap = NULL;

    if (!enif_get_resource(caller_env, trap_term, edf_trap_resource_type, (void **)&trap)) {
        *error_term = EXCP_BADARG(caller_env, "Trap Resource reference is invalid");
        return 0;
    }

    if (trap->state.acquire != NULL && !trap->state.acquire(caller_env, trap, trap->state.arg, error_term)) {
        return 0;
    }

    *trapp = trap;

    return 1;
}

inline void
edf_trap_resource_release(ErlNifEnv *caller_env, edf_trap_t **trapp)
{
    edf_trap_t *trap = (*trapp);
    *trapp = NULL;
    if (trap->state.release != NULL) {
        (void)trap->state.release(caller_env, trap, trap->state.arg);
    }
    return;
}
