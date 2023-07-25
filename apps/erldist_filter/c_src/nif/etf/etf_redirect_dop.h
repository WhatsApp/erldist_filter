/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef ETF_REDIRECT_DOP_H
#define ETF_REDIRECT_DOP_H

#ifdef __cplusplus
extern "C" {
#endif

#include "etf_common.h"

/* Macro Definitions */

/* Type Definitions */

/* Function Declarations */

extern int etf_redirect_dop(ErlNifEnv *caller_env, edf_external_t *external, ERL_NIF_TERM *err_termp);
extern int etf_redirect_dop_resolve_node(ErlNifEnv *caller_env, edf_external_t *ext, vec_t *node_vec);

/* Inline Function Definitions */

#ifdef __cplusplus
}
#endif

#endif
