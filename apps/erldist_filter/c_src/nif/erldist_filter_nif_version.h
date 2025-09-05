/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef ERLDIST_FILTER_NIF_VERSION_H
#define ERLDIST_FILTER_NIF_VERSION_H

#ifdef __cplusplus
extern "C" {
#endif

#include "erldist_filter_nif.h"

/* Function Declarations */

extern ERL_NIF_TERM erldist_filter_nif_version_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#ifdef __cplusplus
}
#endif

#endif
