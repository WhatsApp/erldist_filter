/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef CORE_UNREACHABLE_H
#define CORE_UNREACHABLE_H

#ifdef __cplusplus
extern "C" {
#endif

// unreachable() is part of the C23 standard
// See https://en.cppreference.com/w/c/program/unreachable
#include <stddef.h>

#if !defined(unreachable)
// Uses compiler specific extensions if possible.
#ifdef __GNUC__ // GCC, Clang, ICC
#define unreachable() (__builtin_unreachable())
#elif defined(_MSC_VER) // MSVC
#define unreachable() (__assume(false))
#else
// Even if no extension is used, undefined behavior is still raised by
// the empty function body and the noreturn attribute.
// The external definition of unreachable_impl must be emitted in a separated TU
// due to the rule for inline functions in C.
[[noreturn]] inline void
unreachable_impl()
{
}
#define unreachable() (unreachable_impl())
#endif
#endif

#ifdef __cplusplus
}
#endif

#endif
