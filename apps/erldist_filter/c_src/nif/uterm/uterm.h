/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef UTERM_H
#define UTERM_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../vterm/vterm.h"

/* Macro Definitions */

/* Type Definitions */

/* Function Declarations */

static int uterm_is_small_integer_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_integer_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_float_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_atom_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_small_atom_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_reference_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_new_reference_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_newer_reference_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_port_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_new_port_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_new_float_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_pid_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_new_pid_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_small_tuple_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_large_tuple_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_nil_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_string_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_list_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_binary_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_bit_binary_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_small_big_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_large_big_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_new_fun_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_export_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_map_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_atom_utf8_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_small_atom_utf8_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_v4_port_ext(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_atom_cache_ref(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_atom(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_fixed_integer(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_integer(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_list(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_pid(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_port(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_reference(vterm_env_t *vtenv, const vec_reader_t *vr);
static int uterm_is_tuple(vterm_env_t *vtenv, const vec_reader_t *vr);

/* Inline Function Definitions */

inline int
uterm_is_small_integer_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == SMALL_INTEGER_EXT);
}

inline int
uterm_is_integer_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == INTEGER_EXT);
}

inline int
uterm_is_float_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == FLOAT_EXT);
}

inline int
uterm_is_atom_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == ATOM_EXT);
}

inline int
uterm_is_small_atom_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == SMALL_ATOM_EXT);
}

inline int
uterm_is_reference_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == REFERENCE_EXT);
}

inline int
uterm_is_new_reference_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == NEW_REFERENCE_EXT);
}

inline int
uterm_is_newer_reference_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == NEWER_REFERENCE_EXT);
}

inline int
uterm_is_port_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == PORT_EXT);
}

inline int
uterm_is_new_port_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == NEW_PORT_EXT);
}

inline int
uterm_is_new_float_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == NEW_FLOAT_EXT);
}

inline int
uterm_is_pid_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == PID_EXT);
}

inline int
uterm_is_new_pid_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == NEW_PID_EXT);
}

inline int
uterm_is_small_tuple_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == SMALL_TUPLE_EXT);
}

inline int
uterm_is_large_tuple_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == LARGE_TUPLE_EXT);
}

inline int
uterm_is_nil_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == NIL_EXT);
}

inline int
uterm_is_string_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == STRING_EXT);
}

inline int
uterm_is_list_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == LIST_EXT);
}

inline int
uterm_is_binary_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == BINARY_EXT);
}

inline int
uterm_is_bit_binary_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == BIT_BINARY_EXT);
}

inline int
uterm_is_small_big_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == SMALL_BIG_EXT);
}

inline int
uterm_is_large_big_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == LARGE_BIG_EXT);
}

inline int
uterm_is_new_fun_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == NEW_FUN_EXT);
}

inline int
uterm_is_export_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == EXPORT_EXT);
}

inline int
uterm_is_map_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == MAP_EXT);
}

inline int
uterm_is_atom_utf8_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == ATOM_UTF8_EXT);
}

inline int
uterm_is_small_atom_utf8_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == SMALL_ATOM_UTF8_EXT);
}

inline int
uterm_is_v4_port_ext(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == V4_PORT_EXT);
}

inline int
uterm_is_atom_cache_ref(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    return (tag == ATOM_CACHE_REF);
}

inline int
uterm_is_atom(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    switch (tag) {
    case VTERM_TAG_ATOM_EXT:
        [[fallthrough]];
    case VTERM_TAG_SMALL_ATOM_EXT:
        [[fallthrough]];
    case VTERM_TAG_ATOM_UTF8_EXT:
        [[fallthrough]];
    case VTERM_TAG_SMALL_ATOM_UTF8_EXT:
        [[fallthrough]];
    case VTERM_TAG_ATOM_CACHE_REF:
        return 1;
    default:
        return 0;
    }
}

inline int
uterm_is_fixed_integer(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    switch (tag) {
    case VTERM_TAG_SMALL_INTEGER_EXT:
        [[fallthrough]];
    case VTERM_TAG_INTEGER_EXT:
        return 1;
    default:
        return 0;
    }
}

inline int
uterm_is_integer(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    switch (tag) {
    case VTERM_TAG_SMALL_INTEGER_EXT:
        [[fallthrough]];
    case VTERM_TAG_INTEGER_EXT:
        [[fallthrough]];
    case VTERM_TAG_SMALL_BIG_EXT:
        [[fallthrough]];
    case VTERM_TAG_LARGE_BIG_EXT:
        return 1;
    default:
        return 0;
    }
}

inline int
uterm_is_list(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    switch (tag) {
    case VTERM_TAG_NIL_EXT:
        [[fallthrough]];
    case VTERM_TAG_LIST_EXT:
        return 1;
    default:
        return 0;
    }
}

inline int
uterm_is_pid(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    switch (tag) {
    case VTERM_TAG_PID_EXT:
        [[fallthrough]];
    case VTERM_TAG_NEW_PID_EXT:
        return 1;
    default:
        return 0;
    }
}

inline int
uterm_is_port(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    switch (tag) {
    case VTERM_TAG_PORT_EXT:
        [[fallthrough]];
    case VTERM_TAG_NEW_PORT_EXT:
        [[fallthrough]];
    case VTERM_TAG_V4_PORT_EXT:
        return 1;
    default:
        return 0;
    }
}

inline int
uterm_is_reference(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    switch (tag) {
    case VTERM_TAG_REFERENCE_EXT:
        [[fallthrough]];
    case VTERM_TAG_NEW_REFERENCE_EXT:
        [[fallthrough]];
    case VTERM_TAG_NEWER_REFERENCE_EXT:
        return 1;
    default:
        return 0;
    }
}

inline int
uterm_is_tuple(vterm_env_t *vtenv, const vec_reader_t *vr)
{
    uint8_t tag;
    if (!vec_reader_peek_u8(vr, &tag)) {
        return 0;
    }
    switch (tag) {
    case VTERM_TAG_SMALL_TUPLE_EXT:
        [[fallthrough]];
    case VTERM_TAG_LARGE_TUPLE_EXT:
        return 1;
    default:
        return 0;
    }
}

#ifdef __cplusplus
}
#endif

#endif
