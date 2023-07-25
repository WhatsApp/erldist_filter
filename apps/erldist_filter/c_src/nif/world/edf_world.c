/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "edf_world.h"

static atomic_flag edf_world_initialized = ATOMIC_FLAG_INIT;
static edf_world_t edf_world_internal;
edf_world_t *edf_world = &edf_world_internal;

int
edf_world_load(ErlNifEnv *env)
{
    int retval = 0;

    if (!atomic_flag_test_and_set(&edf_world_initialized)) {
        (void)atomic_init(&edf_world->channels_created, 0);
        (void)atomic_init(&edf_world->channels_destroyed, 0);
    }

    return retval;
}

void
edf_world_unload(ErlNifEnv *env)
{
    (void)env;
    return;
}
