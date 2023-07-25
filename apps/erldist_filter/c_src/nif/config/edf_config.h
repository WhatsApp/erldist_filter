/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_CONFIG_H
#define EDF_CONFIG_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../edf_common.h"

/* Macro Definitions */

/* Type Definitions */

typedef struct edf_config_s edf_config_t;

struct edf_config_s {
    alignas(uintptr_t) bool compact_fragments;
    alignas(uintptr_t) bool deep_packet_inspection;
    alignas(uintptr_t) bool logging;
    alignas(uintptr_t) bool redirect_dist_operations;
};

/* Global Declarations */

extern edf_config_t *edf_config_global;

/* Function Declarations */

extern int edf_config_load(ErlNifEnv *env);
extern void edf_config_unload(ErlNifEnv *env);

static bool edf_config_is_compact_fragments_enabled(void);
static bool edf_config_is_deep_packet_inspection_enabled(void);
static bool edf_config_is_logging_enabled(void);
static bool edf_config_is_redirect_dist_operations_enabled(void);

/* Inline Function Definitions */

inline bool
edf_config_is_compact_fragments_enabled(void)
{
    return (edf_config_global->compact_fragments);
}

inline bool
edf_config_is_deep_packet_inspection_enabled(void)
{
    return (edf_config_global->deep_packet_inspection);
}

inline bool
edf_config_is_logging_enabled(void)
{
    return (edf_config_global->logging);
}

inline bool
edf_config_is_redirect_dist_operations_enabled(void)
{
    return (edf_config_global->redirect_dist_operations);
}

#ifdef __cplusplus
}
#endif

#endif
