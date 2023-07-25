/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef ETF_REDIRECT_SPAWN_REQUEST_H
#define ETF_REDIRECT_SPAWN_REQUEST_H

#ifdef __cplusplus
extern "C" {
#endif

#include "etf_common.h"

/* Macro Definitions */

/* Type Definitions */

/* Function Declarations */

extern int etf_redirect_spawn_request(ErlNifEnv *caller_env, edf_external_t *external, ERL_NIF_TERM *err_termp);

/* Inline Function Definitions */

#ifdef __cplusplus
}
#endif

#endif
