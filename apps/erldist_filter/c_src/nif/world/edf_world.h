/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_WORLD_H
#define EDF_WORLD_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../edf_common.h"

/* Macro Definitions */

/* Type Definitions */

typedef struct edf_world_s edf_world_t;

struct edf_world_s {
    atomic_uint_fast64_t channels_created;
    atomic_uint_fast64_t channels_destroyed;
};

/* Global Declarations */

extern edf_world_t *edf_world;

/* Function Declarations */

/* Function Declarations */

extern int edf_world_load(ErlNifEnv *env);
extern void edf_world_unload(ErlNifEnv *env);

#ifdef __cplusplus
}
#endif

#endif
