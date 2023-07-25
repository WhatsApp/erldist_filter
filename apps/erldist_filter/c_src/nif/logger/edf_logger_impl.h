/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_LOGGER_IMPL_H
#define EDF_LOGGER_IMPL_H

#ifdef __cplusplus
extern "C" {
#endif

#include "edf_logger.h"

/* Macro Definitions */

/* Type Definitions */

/* Global Declarations */

/* Function Declarations */

extern ERL_NIF_TERM erldist_filter_nif_logger_open_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_logger_close_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_logger_inspect_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_logger_list_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_logger_recv_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_logger_set_capacity_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_logger_set_controlling_process_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/* Inline Function Definitions */

#ifdef __cplusplus
}
#endif

#endif
