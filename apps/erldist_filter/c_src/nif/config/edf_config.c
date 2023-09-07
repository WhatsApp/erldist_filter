/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "edf_config.h"

#include "../core/xnif_trace.h"

/* Global Variables */

static edf_config_t edf_config_internal = {
    .compact_fragments = false,
    .deep_packet_inspection = false,
    .logging = false,
    .redirect_dist_operations = false,
    .untrusted = false,
};
edf_config_t *edf_config_global = &edf_config_internal;

/* Function Definitions */

int
edf_config_load(ErlNifEnv *env)
{
    int retval = 0;
    (void)env;
    return retval;
}

void
edf_config_unload(ErlNifEnv *env)
{
    (void)env;
    return;
}
