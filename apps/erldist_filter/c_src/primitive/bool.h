/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef CORE_BOOL_H
#define CORE_BOOL_H

#ifdef __cplusplus
extern "C" {
#endif

// bool, true, and false are now keywords as part of the C23 standard
// See https://en.cppreference.com/w/c/language/bool_constant
#include <stdbool.h>

/* Darwin can be frustrating sometimes */

// #ifndef bool
// #define bool _Bool
// #endif
#ifndef false
#define false 0
#endif
#ifndef true
#define true 1
#endif

#ifdef __cplusplus
}
#endif

#endif
