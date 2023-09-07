/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "edf_trap.h"
#include "edf_trap_impl.h"

#include "../core/xnif_trace.h"

/* Global Variables */

ErlNifResourceType *edf_trap_resource_type = NULL;

/* Static Declarations */

static void edf_trap_resource_type_dtor(ErlNifEnv *env, void *obj);

/* Function Definitions */

int
edf_trap_load(ErlNifEnv *env)
{
    int retval = 0;

    static ErlNifResourceTypeInit edf_trap_resource_type_init = {
        .dtor = edf_trap_resource_type_dtor,
        .stop = NULL,
        .down = NULL,
        .members = 4,
        .dyncall = NULL,
    };
    edf_trap_resource_type = enif_init_resource_type(env, "erldist_filter_nif_trap", &edf_trap_resource_type_init,
                                                     ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    if (edf_trap_resource_type == NULL) {
        retval = -1;
        return retval;
    }

    return retval;
}

void
edf_trap_unload(ErlNifEnv *env)
{
    (void)env;
    edf_trap_resource_type = NULL;
    return;
}

int
edf_trap_open_x(ErlNifEnv *env, const edf_trap_state_t *state, size_t sz, edf_trap_t **trapp, ERL_NIF_TERM *err_termp)
{
    edf_trap_t *trap = NULL;

    if (trapp == NULL) {
        *err_termp = EXCP_ERROR(env, "Call to edf_trap_open_x() failed: **trapp must not be NULL.");
        return 0;
    }

    if (sz < sizeof(edf_trap_t)) {
        *err_termp = EXCP_ERROR_F(env, "Fatal error: size must be at least %d-bytes (was %d-bytes).", sizeof(edf_trap_t), sz);
        return 0;
    }

    trap = enif_alloc_resource(edf_trap_resource_type, sz);
    if (trap == NULL) {
        *err_termp = EXCP_ERROR_F(env, "Can't allocate edf_trap_t: unable to allocate %d-bytes", sz);
        return 0;
    }

    trap->reds = 0;
    trap->parent = NULL;
    trap->child = NULL;
    trap->child_result.tag = EDF_TRAP_RESULT_TAG_ERR;
    trap->child_result.err = THE_NON_VALUE;
    trap->state = *state;
    trap->keep_term = THE_NON_VALUE;

    if (trap->state.resource != NULL) {
        (void)enif_keep_resource((void *)(trap->state.resource));
    }

    *trapp = trap;

    return 1;
}

void
edf_trap_resource_type_dtor(ErlNifEnv *caller_env, void *obj)
{
    edf_trap_t *trap = (void *)obj;

    XNIF_TRACE_F("%s:%d [trap] dtor callback\n", __FILE__, __LINE__);

    if (trap->state.dtor != NULL) {
        (void)trap->state.dtor(caller_env, trap, trap->state.arg);
    }
    if (trap->child != NULL) {
        (void)enif_release_resource((void *)(trap->child));
        trap->child = NULL;
    }
    if (trap->state.resource != NULL) {
        (void)enif_release_resource((void *)(trap->state.resource));
    }

    return;
}

edf_trap_result_t
edf_trap_block_on_next(ErlNifEnv *caller_env, edf_trap_t *trap)
{
    edf_trap_result_t trap_result;
    ERL_NIF_TERM err_term = THE_NON_VALUE;

#define TRAP_PREP_ERR(gen_err)                                                                                                     \
    do {                                                                                                                           \
        err_term = (gen_err);                                                                                                      \
        trap_result = TRAP_ERR(err_term);                                                                                          \
    } while (0)

    if (trap == NULL) {
        TRAP_PREP_ERR(EXCP_BADARG(caller_env, "Trap Resource reference is invalid: trap is NULL"));
        return trap_result;
    }
    (void)enif_keep_resource((void *)trap);
    // Why two nested loops that continue on yielding?
    // It's possible that the `trap->state.edit` changes the result to a yield,
    // so we need to continue looping if that happens.
    do {
        if (trap->state.acquire != NULL && !trap->state.acquire(caller_env, trap, trap->state.arg, &err_term)) {
            TRAP_PREP_ERR(err_term);
            return trap_result;
        }
        do {
            trap->reds = REDUCTIONS_UNTIL_YCF_YIELD();
            if (trap->state.next == NULL) {
                if (trap->state.release != NULL) {
                    (void)trap->state.release(caller_env, trap, trap->state.arg);
                }
                TRAP_PREP_ERR(EXCP_BADARG(caller_env, "Trap Resource reference is invalid: trap->state.next is NULL"));
                return trap_result;
            }
            trap_result = trap->state.next(caller_env, trap, trap->state.arg);
            if (trap->reds < 0 || trap->reds > REDUCTIONS_UNTIL_YCF_YIELD()) {
                trap->reds = 0;
            }
        } while (trap_result.tag == EDF_TRAP_RESULT_TAG_YIELD);
        if (trap->state.release != NULL) {
            (void)trap->state.release(caller_env, trap, trap->state.arg);
        }
        if (trap->state.edit != NULL) {
            (void)trap->state.edit(caller_env, trap, trap->state.arg, &trap_result);
        }
    } while (trap_result.tag == EDF_TRAP_RESULT_TAG_YIELD);
    (void)enif_release_resource((void *)trap);
    return trap_result;

#undef TRAP_PREP_ERR
}

ERL_NIF_TERM
edf_trap_schedule_from_term_x(ErlNifEnv *caller_env, ERL_NIF_TERM trap_term, ERL_NIF_TERM keep_term)
{
    ERL_NIF_TERM newargv[2];
    newargv[0] = trap_term;
    newargv[1] = keep_term;
    return enif_schedule_nif(caller_env, "erldist_filter_nif_trap_2_continue", ERL_NIF_NORMAL_JOB_BOUND,
                             erldist_filter_nif_trap_2_continue, 2, newargv);
}

edf_trap_result_t
edf_trap_child_next(ErlNifEnv *caller_env, edf_trap_t *parent)
{
    edf_trap_t *child = parent->child;
    ERL_NIF_TERM err_term = THE_NON_VALUE;

#define TRAP_PREP_ERR(gen_err)                                                                                                     \
    do {                                                                                                                           \
        err_term = (gen_err);                                                                                                      \
        parent->reds = child->reds;                                                                                                \
        parent->child_result = TRAP_ERR(err_term);                                                                                 \
    } while (0)

    if (child == NULL) {
        TRAP_PREP_ERR(EXCP_BADARG(caller_env, "Child Trap Resource reference is invalid: parent->child is NULL"));
        return parent->child_result;
    }
    child->reds = parent->reds;
    if (child->state.acquire != NULL && !child->state.acquire(caller_env, child, child->state.arg, &err_term)) {
        TRAP_PREP_ERR(err_term);
        return parent->child_result;
    }
    if (child->state.next == NULL) {
        if (child->state.release != NULL) {
            (void)child->state.release(caller_env, child, child->state.arg);
        }
        TRAP_PREP_ERR(EXCP_BADARG(caller_env, "Child Trap Resource reference is invalid: child->state.next is NULL"));
        return parent->child_result;
    }
    parent->child_result = child->state.next(caller_env, child, child->state.arg);
    if (child->reds < 0 || child->reds > REDUCTIONS_UNTIL_YCF_YIELD()) {
        child->reds = 0;
    }
    if (child->state.release != NULL) {
        (void)child->state.release(caller_env, child, child->state.arg);
    }
    if (child->state.edit != NULL) {
        (void)child->state.edit(caller_env, child, child->state.arg, &parent->child_result);
    }
    parent->reds = child->reds;
    return parent->child_result;

#undef TRAP_PREP_ERR
}
