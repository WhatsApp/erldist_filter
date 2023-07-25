/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef CORE_ALIGN_H
#define CORE_ALIGN_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdalign.h>

/* Darwin can be frustrating sometimes */

#ifndef alignas
#define alignas(T) _Alignas(T)
#endif

#ifdef __cplusplus
}
#endif

#endif
