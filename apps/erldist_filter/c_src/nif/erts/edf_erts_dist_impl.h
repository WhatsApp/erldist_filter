/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_ERTS_DIST_IMPL_H
#define EDF_ERTS_DIST_IMPL_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../erldist_filter_nif.h"

/* Function Declarations */

extern ERL_NIF_TERM erldist_filter_nif_altact_sig_flags_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_distribution_flags_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_spawn_flags_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#ifdef __cplusplus
}
#endif

#endif
