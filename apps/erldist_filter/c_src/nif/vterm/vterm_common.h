/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef VTERM_COMMON_H
#define VTERM_COMMON_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../edf_common.h"
#include "../channel/edf_atom_cache.h"
#include "../vec.h"

/* Macro Definitions */

/* Type Definitions */

typedef uintptr_t vword_t;

typedef struct __vterm_s __vterm_t;
typedef enum __vterm_tag_t __vterm_tag_t;

typedef __vterm_t *vterm_t;
typedef vword_t vterm_tag_t;

#ifdef __cplusplus
}
#endif

#endif
