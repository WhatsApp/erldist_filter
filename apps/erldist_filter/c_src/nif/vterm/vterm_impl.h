/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef VTERM_IMPL_H
#define VTERM_IMPL_H

#ifdef __cplusplus
extern "C" {
#endif

#include "vterm.h"

/* Macro Definitions */

/* Type Definitions */

/* Global Declarations */

/* Function Declarations */

extern ERL_NIF_TERM erldist_filter_nif_dist_ext_to_vdist_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_dist_ext_to_vterm_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_dist_ext_to_vterm_3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_dist_int_to_vdist_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_dist_int_to_vterm_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_dist_int_to_vterm_3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/* Inline Function Definitions */

#ifdef __cplusplus
}
#endif

#endif
