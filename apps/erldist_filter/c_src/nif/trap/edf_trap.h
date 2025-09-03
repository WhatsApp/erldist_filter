/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_TRAP_H
#define EDF_TRAP_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../erldist_filter_nif.h"

/* Macros */

#define TRAP_OK(ok_term)                                                                                                           \
    ((edf_trap_result_t){                                                                                                          \
        .tag = EDF_TRAP_RESULT_TAG_OK,                                                                                             \
        .err = (ok_term),                                                                                                          \
    })
#define TRAP_ERR(err_term)                                                                                                         \
    ((edf_trap_result_t){                                                                                                          \
        .tag = EDF_TRAP_RESULT_TAG_ERR,                                                                                            \
        .err = (err_term),                                                                                                         \
    })
#define TRAP_YIELD()                                                                                                               \
    ((edf_trap_result_t){                                                                                                          \
        .tag = EDF_TRAP_RESULT_TAG_YIELD,                                                                                          \
        .yield = THE_NON_VALUE,                                                                                                    \
    })
#define TRAP_YIELD_NEW(new_trap_term)                                                                                              \
    ((edf_trap_result_t){                                                                                                          \
        .tag = EDF_TRAP_RESULT_TAG_YIELD,                                                                                          \
        .yield = (new_trap_term),                                                                                                  \
    })

#define TRAP_REDUCE(trap, nr_of_reductions) (((edf_trap_t *)(trap))->reds -= (int)(nr_of_reductions))
#define TRAP_SHOULD_YIELD(trap) (((edf_trap_t *)(trap))->reds <= 0)

/* Type Definitions */

enum edf_trap_result_tag_t {
    EDF_TRAP_RESULT_TAG_YIELD = -1,
    EDF_TRAP_RESULT_TAG_ERR = 0,
    EDF_TRAP_RESULT_TAG_OK = 1,
};

typedef struct edf_trap_s edf_trap_t;
typedef struct edf_trap_result_s edf_trap_result_t;
typedef enum edf_trap_result_tag_t edf_trap_result_tag_t;
typedef struct edf_trap_state_s edf_trap_state_t;

typedef int (*edf_trap_state_acquire_t)(ErlNifEnv *caller_env, edf_trap_t *trap, void *arg, ERL_NIF_TERM *error_term);
typedef void (*edf_trap_state_release_t)(ErlNifEnv *caller_env, edf_trap_t *trap, void *arg);
typedef void (*edf_trap_state_dtor_t)(ErlNifEnv *caller_env, edf_trap_t *trap, void *arg);
typedef void (*edf_trap_state_edit_t)(ErlNifEnv *caller_env, edf_trap_t *trap, void *arg, edf_trap_result_t *result);
typedef edf_trap_result_t (*edf_trap_state_next_t)(ErlNifEnv *caller_env, edf_trap_t *trap, void *arg);

struct edf_trap_result_s {
    edf_trap_result_tag_t tag;
    union {
        ERL_NIF_TERM ok;
        ERL_NIF_TERM err;
        ERL_NIF_TERM yield;
    };
};

struct edf_trap_state_s {
    void *resource;
    edf_trap_state_acquire_t acquire;
    edf_trap_state_release_t release;
    edf_trap_state_dtor_t dtor;
    edf_trap_state_edit_t edit;
    edf_trap_state_next_t next;
    void *arg;
};

struct edf_trap_s {
    int reds;
    edf_trap_t *parent;
    edf_trap_t *child;
    edf_trap_result_t child_result;
    edf_trap_state_t state;
    ERL_NIF_TERM keep_term;
};

/* Global Declarations */

extern ErlNifResourceType *edf_trap_resource_type;

/* Function Declarations */

extern int edf_trap_load(ErlNifEnv *env);
extern void edf_trap_unload(ErlNifEnv *env);
static ERL_NIF_TERM edf_trap_open(ErlNifEnv *env, const edf_trap_state_t *state, size_t sz, edf_trap_t **trapp);
extern int edf_trap_open_x(ErlNifEnv *env, const edf_trap_state_t *state, size_t sz, edf_trap_t **trapp, ERL_NIF_TERM *err_termp);
static ERL_NIF_TERM edf_trap_open_x_make_term(ErlNifEnv *env, edf_trap_t *trap);
extern edf_trap_result_t edf_trap_block_on_next(ErlNifEnv *caller_env, edf_trap_t *trap);
static ERL_NIF_TERM edf_trap_schedule_from_term(ErlNifEnv *caller_env, ERL_NIF_TERM trap_term);
extern ERL_NIF_TERM edf_trap_schedule_from_term_x(ErlNifEnv *caller_env, ERL_NIF_TERM trap_term, ERL_NIF_TERM keep_term);
extern edf_trap_result_t edf_trap_child_next(ErlNifEnv *caller_env, edf_trap_t *parent);
static bool edf_trap_has_child(const edf_trap_t *parent);
static int edf_trap_attach_child(edf_trap_t *parent, edf_trap_t *child);
static int edf_trap_detach_child(edf_trap_t *parent);

/* Inline Function Definitions */

inline ERL_NIF_TERM
edf_trap_open(ErlNifEnv *env, const edf_trap_state_t *state, size_t sz, edf_trap_t **trapp)
{
    edf_trap_t *trap = NULL;
    ERL_NIF_TERM err_term = THE_NON_VALUE;
    if (!edf_trap_open_x(env, state, sz, &trap, &err_term)) {
        return err_term;
    }
    if (trapp != NULL) {
        *trapp = trap;
    }
    return edf_trap_open_x_make_term(env, trap);
}

inline ERL_NIF_TERM
edf_trap_open_x_make_term(ErlNifEnv *env, edf_trap_t *trap)
{
    ERL_NIF_TERM trap_term = THE_NON_VALUE;
    trap_term = enif_make_resource(env, (void *)trap);
    (void)enif_release_resource((void *)trap);
    return trap_term;
}

inline ERL_NIF_TERM
edf_trap_schedule_from_term(ErlNifEnv *caller_env, ERL_NIF_TERM trap_term)
{
    ERL_NIF_TERM keep_term = enif_make_list(caller_env, 0);
    return edf_trap_schedule_from_term_x(caller_env, trap_term, keep_term);
}

inline bool
edf_trap_has_child(const edf_trap_t *parent)
{
    return (parent->child != NULL);
}

inline int
edf_trap_attach_child(edf_trap_t *parent, edf_trap_t *child)
{
    if (parent == NULL || child == NULL || parent->child != NULL || child->parent != NULL) {
        return 0;
    }
    parent->child = child;
    parent->child_result.tag = EDF_TRAP_RESULT_TAG_ERR;
    parent->child_result.err = THE_NON_VALUE;
    (void)enif_keep_resource((void *)child);
    child->parent = parent;
    return 1;
}

inline int
edf_trap_detach_child(edf_trap_t *parent)
{
    edf_trap_t *child = NULL;
    if (parent == NULL || parent->child == NULL || parent->child->parent != parent) {
        return 0;
    }
    child = parent->child;
    parent->child = NULL;
    parent->child_result.tag = EDF_TRAP_RESULT_TAG_ERR;
    parent->child_result.err = THE_NON_VALUE;
    child->parent = NULL;
    (void)enif_release_resource((void *)(child));
    return 1;
}

#ifdef __cplusplus
}
#endif

#endif
