/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_CHANNEL_IMPL_H
#define EDF_CHANNEL_IMPL_H

#ifdef __cplusplus
extern "C" {
#endif

#include "edf_channel.h"

/* Macro Definitions */

/* Type Definitions */

/* Global Declarations */

/* Function Declarations */

extern ERL_NIF_TERM erldist_filter_nif_channel_open_5(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_channel_close_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_channel_inspect_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_channel_list_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_channel_list_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_channel_recv_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_channel_set_controlling_process_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_channel_set_tracing_process_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/* Inline Function Definitions */

#ifdef __cplusplus
}
#endif

#endif
