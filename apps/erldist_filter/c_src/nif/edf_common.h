/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_COMMON_H
#define EDF_COMMON_H

#ifdef __cplusplus
extern "C" {
#endif

#include "core/portable_endian.h"
#include "core/portable_hint.h"

#include <errno.h>
#include <inttypes.h>
#include "core/align.h"
#include <stdatomic.h>
#include "core/bool.h"
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <erl_nif.h>

#include "edf_atoms.h"
#include "slice.h"

#include "core/xnif_trace.h"

/* NIF Utility Macros */

#ifndef THE_NON_VALUE
#define THE_NON_VALUE ((ERL_NIF_TERM)0)
#endif

#ifndef ERL_NIF_NORMAL_JOB_BOUND
#define ERL_NIF_NORMAL_JOB_BOUND (0)
#endif

#define REDUCTIONS_UNTIL_YCF_YIELD() (20000)
#define BUMP_ALL_REDS(env)                                                                                                         \
    do {                                                                                                                           \
        (void)enif_consume_timeslice((env), 100);                                                                                  \
    } while (0)
#define BUMP_REMAINING_REDS(env, nr_of_reductions)                                                                                 \
    do {                                                                                                                           \
        (void)enif_consume_timeslice((env),                                                                                        \
                                     (int)((REDUCTIONS_UNTIL_YCF_YIELD() - (nr_of_reductions)) / REDUCTIONS_UNTIL_YCF_YIELD()));   \
    } while (0)

/* All nif functions return a valid value or throws an exception */
#define EXCP(Env, ClassTerm, ReasonString) xnif_raise_exception((Env), __FILE__, __LINE__, (ClassTerm), (ReasonString))

#define EXCP_F(Env, ClassTerm, ReasonFormat, ...)                                                                                  \
    xnif_raise_exception_format((Env), __FILE__, __LINE__, (ClassTerm), (ReasonFormat), __VA_ARGS__)

#define EXCP_NOTSUP(Env, Str) EXCP((Env), ATOM(notsup), (Str))
#define EXCP_BADARG(Env, Str) EXCP((Env), ATOM(badarg), (Str))
#define EXCP_BADARG_F(Env, Fmt, ...) EXCP_F((Env), ATOM(badarg), Fmt, __VA_ARGS__)
#define EXCP_ERROR(Env, Str) EXCP((Env), ATOM(error), (Str))
#define EXCP_ERROR_F(Env, Fmt, ...) EXCP_F((Env), ATOM(error), Fmt, __VA_ARGS__)

/* Common Types */

/* Global Variables */

#define ERLDIST_FILTER_ROUTER_LIMIT (1024)
extern uint64_t erldist_filter_router_count;
extern ERL_NIF_TERM erldist_filter_router_names[];

/* Function Declarations */

#ifdef __cplusplus
}
#endif

#endif
