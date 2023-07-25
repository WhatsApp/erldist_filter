/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef ETF_DECODE_UDIST_H
#define ETF_DECODE_UDIST_H

#ifdef __cplusplus
extern "C" {
#endif

#include "etf_common.h"

/* Macro Definitions */

/* Type Definitions */

// Don't include "vterm_env.h", just reference the type here.
typedef struct vterm_env_s vterm_env_t;

/* Function Declarations */

extern int etf_decode_udist_control(ErlNifEnv *caller_env, vterm_env_t *vtenv, bool is_external, vec_t *slice, udist_t *up,
                                    ERL_NIF_TERM *err_termp);

/* Inline Function Definitions */

#ifdef __cplusplus
}
#endif

#endif
