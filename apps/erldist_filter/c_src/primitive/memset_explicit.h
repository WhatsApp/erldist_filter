/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef CORE_MEMSET_EXPLICIT_H
#define CORE_MEMSET_EXPLICIT_H

#ifdef __cplusplus
extern "C" {
#endif

#include <string.h>

/* Part of C23, but not available everywhere, yet. */

#ifndef memset_explicit
#define memset_explicit(dest, ch, count) __core_memset_explicit(dest, ch, count)

/* Function Declarations */

static void *__core_memset_explicit(void *dest, int ch, size_t count);

/* Inline Function Definitions */

inline void *
__core_memset_explicit(void *dest, int ch, size_t count)
{
    // Use builtin memset function to set the memory.
    (void)memset(dest, ch, count);
    // avoid dead store elimination
    // The asm itself should also be sufficient to behave as a compiler barrier.
    __asm__ __volatile__("" ::"r"(dest) : "memory");
    return dest;
}

#endif

#ifdef __cplusplus
}
#endif

#endif
