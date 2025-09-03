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

#include "../erldist_filter_nif.h"
#include "../channel/edf_atom_cache.h"
#include "../vec.h"

/* Macro Definitions */

/* Type Definitions */

typedef uintptr_t vword_t;

enum __vterm_tag_t {
    VTERM_TAG_LAZY_TERM = -3,
    VTERM_TAG_NIF_TERM = -2,
    VTERM_TAG_ATOM_CACHE_REF_RESOLVED = -1,
    VTERM_TAG_THE_NON_VALUE = 0,
    VTERM_TAG_SMALL_INTEGER_EXT = SMALL_INTEGER_EXT,
    VTERM_TAG_INTEGER_EXT = INTEGER_EXT,
    VTERM_TAG_FLOAT_EXT = FLOAT_EXT,
    VTERM_TAG_ATOM_EXT = ATOM_EXT,
    VTERM_TAG_SMALL_ATOM_EXT = SMALL_ATOM_EXT,
    VTERM_TAG_REFERENCE_EXT = REFERENCE_EXT,
    VTERM_TAG_NEW_REFERENCE_EXT = NEW_REFERENCE_EXT,
    VTERM_TAG_NEWER_REFERENCE_EXT = NEWER_REFERENCE_EXT,
    VTERM_TAG_PORT_EXT = PORT_EXT,
    VTERM_TAG_NEW_PORT_EXT = NEW_PORT_EXT,
    VTERM_TAG_NEW_FLOAT_EXT = NEW_FLOAT_EXT,
    VTERM_TAG_PID_EXT = PID_EXT,
    VTERM_TAG_NEW_PID_EXT = NEW_PID_EXT,
    VTERM_TAG_SMALL_TUPLE_EXT = SMALL_TUPLE_EXT,
    VTERM_TAG_LARGE_TUPLE_EXT = LARGE_TUPLE_EXT,
    VTERM_TAG_NIL_EXT = NIL_EXT,
    VTERM_TAG_STRING_EXT = STRING_EXT,
    VTERM_TAG_LIST_EXT = LIST_EXT,
    VTERM_TAG_BINARY_EXT = BINARY_EXT,
    VTERM_TAG_BIT_BINARY_EXT = BIT_BINARY_EXT,
    VTERM_TAG_SMALL_BIG_EXT = SMALL_BIG_EXT,
    VTERM_TAG_LARGE_BIG_EXT = LARGE_BIG_EXT,
    VTERM_TAG_NEW_FUN_EXT = NEW_FUN_EXT,
    VTERM_TAG_EXPORT_EXT = EXPORT_EXT,
    VTERM_TAG_MAP_EXT = MAP_EXT,
    VTERM_TAG_ATOM_UTF8_EXT = ATOM_UTF8_EXT,
    VTERM_TAG_SMALL_ATOM_UTF8_EXT = SMALL_ATOM_UTF8_EXT,
    VTERM_TAG_V4_PORT_EXT = V4_PORT_EXT,
    VTERM_TAG_ATOM_CACHE_REF = ATOM_CACHE_REF,
};

typedef struct __vterm_s __vterm_t;
typedef enum __vterm_tag_t __vterm_tag_t;

typedef __vterm_t *vterm_t;
typedef vword_t vterm_tag_t;

#ifdef __cplusplus
}
#endif

#endif
