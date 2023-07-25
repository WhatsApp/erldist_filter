/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_CHANNEL_INSPECT_H
#define EDF_CHANNEL_INSPECT_H

#ifdef __cplusplus
extern "C" {
#endif

#include "edf_channel.h"

/* Macro Definitions */

/* Type Definitions */

/* Global Declarations */

/* Function Declarations */

extern int edf_channel_inspect_atom_cache(ErlNifEnv *env, edf_atom_cache_t *cache, ERL_NIF_TERM *termp);
extern int edf_channel_inspect_entry(ErlNifEnv *env, edf_channel_t *channel, ERL_NIF_TERM *termp);
extern int edf_channel_inspect_rx(ErlNifEnv *env, edf_channel_t *channel, ERL_NIF_TERM *termp);
extern int edf_channel_inspect_stats(ErlNifEnv *env, edf_channel_stats_t *stats, ERL_NIF_TERM *termp);

/* Inline Function Definitions */

#ifdef __cplusplus
}
#endif

#endif
