/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "vterm.h"

static int encode_write_vterm(ErlNifEnv *caller_env, int flags, vterm_env_t *vtenv, vterm_t *vtp, vec_writer_t *vw);
static int encode_write_vterm_atom(ErlNifEnv *caller_env, int flags, vterm_env_t *vtenv, vterm_t *vtp, vec_writer_t *vw);
static int encode_write_vterm_fixed_integer(ErlNifEnv *caller_env, int flags, vterm_env_t *vtenv, vterm_t *vtp, vec_writer_t *vw);
static int encode_write_vterm_pid(ErlNifEnv *caller_env, int flags, vterm_env_t *vtenv, vterm_t *vtp, vec_writer_t *vw);

int
vterm_encode(ErlNifEnv *caller_env, int flags, vterm_env_t *vtenv, vterm_t *vtp, vec_t *dst_vec)
{
    vec_writer_t vw[1];
    size_t len = 0;

    if (dst_vec != NULL && !vec_is_free(dst_vec)) {
        return 0;
    }
    if (!vterm_encode_length(caller_env, flags, vtenv, vtp, &len)) {
        return 0;
    }
    if (dst_vec != NULL) {
        if (!vec_create_owned(dst_vec, len)) {
            return 0;
        }
        if (!vec_writer_create(vw, dst_vec, 0)) {
            (void)vec_destroy(dst_vec);
            return 0;
        }
        if (!encode_write_vterm(caller_env, flags, vtenv, vtp, vw)) {
            (void)vec_writer_destroy(vw);
            (void)vec_destroy(dst_vec);
            return 0;
        }
        (void)vec_writer_destroy(vw);
    }

    return 1;
}

int
vterm_encode_length(ErlNifEnv *caller_env, int flags, vterm_env_t *vtenv, vterm_t *vtp, size_t *lenp)
{
    vec_t vp[1];
    vec_writer_t vw[1];

    (void)vec_init_free(vp);
    if (!vec_create_null(vp)) {
        return 0;
    }
    if (!vec_writer_create(vw, vp, 0)) {
        (void)vec_destroy(vp);
        return 0;
    }
    if (!encode_write_vterm(caller_env, flags, vtenv, vtp, vw)) {
        (void)vec_writer_destroy(vw);
        (void)vec_destroy(vp);
        return 0;
    }
    if (lenp != NULL) {
        *lenp = vec_writer_offset(vw);
    }
    (void)vec_writer_destroy(vw);
    (void)vec_destroy(vp);
    return 1;
}

int
vterm_encode_and_try_resolve(ErlNifEnv *env, vterm_env_t *vtenv, vterm_t *vtp, ERL_NIF_TERM *termp)
{
    vec_t vp[1];
    size_t bytes_read;

    if (!vterm_maybe_decode_lazy_term(vtenv, vtp)) {
        return 0;
    }
    if (vterm_is_atom(vtenv, vtp)) {
        ErlNifCharEncoding encoding = ERL_NIF_UTF8;
        const uint8_t *name = NULL;
        size_t len = 0;
        if (vterm_get_atom_text(vtenv, vtp, &encoding, &name, &len)) {
            if (!edf_atom_text_put(name, len, encoding, termp)) {
                return 0;
            }
            return 1;
        }
    }
    (void)vec_init_free(vp);
    if (!vterm_encode(env, (VTERM_ENCODE_FLAG_IS_EXTERNAL), vtenv, vtp, vp)) {
        return 0;
    }
    bytes_read = enif_binary_to_term(env, vec_buf(vp), vec_len(vp), termp, 0);
    if (bytes_read == 0) {
        (void)vec_destroy(vp);
        return 0;
    }
    (void)vec_destroy(vp);
    return 1;
}

int
encode_write_vterm(ErlNifEnv *caller_env, int flags, vterm_env_t *vtenv, vterm_t *vtp, vec_writer_t *vw)
{
    int internal_flags = (flags & (~VTERM_ENCODE_FLAG_IS_EXTERNAL));

#define WRITE_ATOM(vtp)                                                                                                            \
    do {                                                                                                                           \
        if (!encode_write_vterm_atom(caller_env, internal_flags, vtenv, (vtp), vw)) {                                              \
                                                                                                                                   \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define WRITE_BYTES(buf, len)                                                                                                      \
    do {                                                                                                                           \
        if (!vec_writer_write_exact(vw, (buf), (len))) {                                                                           \
                                                                                                                                   \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define WRITE_FIXED_INTEGER(vtp)                                                                                                   \
    do {                                                                                                                           \
        if (!encode_write_vterm_fixed_integer(caller_env, internal_flags, vtenv, (vtp), vw)) {                                     \
                                                                                                                                   \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define WRITE_I32(val)                                                                                                             \
    do {                                                                                                                           \
        if (!vec_writer_write_i32(vw, (val))) {                                                                                    \
                                                                                                                                   \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define WRITE_PID(vtp)                                                                                                             \
    do {                                                                                                                           \
        if (!encode_write_vterm_pid(caller_env, internal_flags, vtenv, (vtp), vw)) {                                               \
                                                                                                                                   \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define WRITE_U8(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_writer_write_u8(vw, (val))) {                                                                                     \
                                                                                                                                   \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define WRITE_U16(val)                                                                                                             \
    do {                                                                                                                           \
        if (!vec_writer_write_u16(vw, (val))) {                                                                                    \
                                                                                                                                   \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define WRITE_U32(val)                                                                                                             \
    do {                                                                                                                           \
        if (!vec_writer_write_u32(vw, (val))) {                                                                                    \
                                                                                                                                   \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define WRITE_U64(val)                                                                                                             \
    do {                                                                                                                           \
        if (!vec_writer_write_u64(vw, (val))) {                                                                                    \
                                                                                                                                   \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define WRITE_VTERM(vtp)                                                                                                           \
    do {                                                                                                                           \
        if (!encode_write_vterm(caller_env, internal_flags, vtenv, (vtp), vw)) {                                                   \
                                                                                                                                   \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

    if ((flags & VTERM_ENCODE_FLAG_IS_EXTERNAL) != 0) {
        WRITE_U8(VERSION_MAGIC);
    }

    if (!vterm_maybe_decode_lazy_term(vtenv, vtp)) {
        return 0;
    }

    switch ((*vtp)->tag) {
    case VTERM_TAG_ATOM_CACHE_REF_RESOLVED: {
        return encode_write_vterm_atom(caller_env, internal_flags, vtenv, vtp, vw);
    }
    case VTERM_TAG_SMALL_INTEGER_EXT: {
        vterm_data_small_integer_ext_t *dp = &(*vtp)->data.small_integer_ext;
        WRITE_U8(SMALL_INTEGER_EXT);
        WRITE_U8(dp->value);
        break;
    }
    case VTERM_TAG_INTEGER_EXT: {
        vterm_data_integer_ext_t *dp = &(*vtp)->data.integer_ext;
        WRITE_U8(INTEGER_EXT);
        WRITE_I32(dp->value);
        break;
    }
    case VTERM_TAG_FLOAT_EXT: {
        vterm_data_float_ext_t *dp = &(*vtp)->data.float_ext;
        WRITE_U8(FLOAT_EXT);
        WRITE_BYTES(dp->float_string, 31);
        break;
    }
    case VTERM_TAG_ATOM_EXT: {
        vterm_data_atom_ext_t *dp = &(*vtp)->data.atom_ext;
        WRITE_U8(ATOM_EXT);
        WRITE_U16(dp->len);
        WRITE_BYTES(dp->name, dp->len);
        break;
    }
    case VTERM_TAG_SMALL_ATOM_EXT: {
        vterm_data_small_atom_ext_t *dp = &(*vtp)->data.small_atom_ext;
        WRITE_U8(SMALL_ATOM_EXT);
        WRITE_U8(dp->len);
        WRITE_BYTES(dp->name, dp->len);
        break;
    }
    case VTERM_TAG_REFERENCE_EXT: {
        vterm_data_reference_ext_t *dp = &(*vtp)->data.reference_ext;
        WRITE_U8(REFERENCE_EXT);
        WRITE_ATOM(&dp->node);
        WRITE_U32(dp->id);
        WRITE_U8(dp->creation);
        break;
    }
    case VTERM_TAG_NEW_REFERENCE_EXT: {
        vterm_data_new_reference_ext_t *dp = &(*vtp)->data.new_reference_ext;
        WRITE_U8(NEW_REFERENCE_EXT);
        WRITE_U16(dp->len);
        WRITE_ATOM(&dp->node);
        WRITE_U8(dp->creation);
        for (size_t i = 0; i < (size_t)(dp->len); i++) {
            WRITE_U32(dp->ids[i]);
        }
        break;
    }
    case VTERM_TAG_NEWER_REFERENCE_EXT: {
        vterm_data_newer_reference_ext_t *dp = &(*vtp)->data.newer_reference_ext;
        WRITE_U8(NEWER_REFERENCE_EXT);
        WRITE_U16(dp->len);
        WRITE_ATOM(&dp->node);
        WRITE_U32(dp->creation);
        for (size_t i = 0; i < (size_t)(dp->len); i++) {
            WRITE_U32(dp->ids[i]);
        }
        break;
    }
    case VTERM_TAG_PORT_EXT: {
        vterm_data_port_ext_t *dp = &(*vtp)->data.port_ext;
        WRITE_U8(PORT_EXT);
        WRITE_ATOM(&dp->node);
        WRITE_U32(dp->id);
        WRITE_U8(dp->creation);
        break;
    }
    case VTERM_TAG_NEW_PORT_EXT: {
        vterm_data_new_port_ext_t *dp = &(*vtp)->data.new_port_ext;
        WRITE_U8(NEW_PORT_EXT);
        WRITE_ATOM(&dp->node);
        WRITE_U32(dp->id);
        WRITE_U32(dp->creation);
        break;
    }
    case VTERM_TAG_NEW_FLOAT_EXT: {
        vterm_data_new_float_ext_t *dp = &(*vtp)->data.new_float_ext;
        WRITE_U8(NEW_FLOAT_EXT);
        WRITE_BYTES(dp->ieee_float, 8);
        break;
    }
    case VTERM_TAG_PID_EXT: {
        vterm_data_pid_ext_t *dp = &(*vtp)->data.pid_ext;
        WRITE_U8(PID_EXT);
        WRITE_ATOM(&dp->node);
        WRITE_U32(dp->id);
        WRITE_U32(dp->serial);
        WRITE_U8(dp->creation);
        break;
    }
    case VTERM_TAG_NEW_PID_EXT: {
        vterm_data_new_pid_ext_t *dp = &(*vtp)->data.new_pid_ext;
        WRITE_U8(NEW_PID_EXT);
        WRITE_ATOM(&dp->node);
        WRITE_U32(dp->id);
        WRITE_U32(dp->serial);
        WRITE_U32(dp->creation);
        break;
    }
    case VTERM_TAG_SMALL_TUPLE_EXT: {
        vterm_data_small_tuple_ext_t *dp = &(*vtp)->data.small_tuple_ext;
        WRITE_U8(SMALL_TUPLE_EXT);
        WRITE_U8(dp->arity);
        for (size_t i = 0; i < (size_t)(dp->arity); i++) {
            WRITE_VTERM(&dp->elements[i]);
        }
        break;
    }
    case VTERM_TAG_LARGE_TUPLE_EXT: {
        vterm_data_large_tuple_ext_t *dp = &(*vtp)->data.large_tuple_ext;
        WRITE_U8(LARGE_TUPLE_EXT);
        WRITE_U32(dp->arity);
        for (size_t i = 0; i < (size_t)(dp->arity); i++) {
            WRITE_VTERM(&dp->elements[i]);
        }
        break;
    }
    case VTERM_TAG_NIL_EXT: {
        WRITE_U8(NIL_EXT);
        break;
    }
    case VTERM_TAG_STRING_EXT: {
        vterm_data_string_ext_t *dp = &(*vtp)->data.string_ext;
        WRITE_U8(STRING_EXT);
        WRITE_U16(dp->len);
        WRITE_BYTES(dp->characters, dp->len);
        break;
    }
    case VTERM_TAG_LIST_EXT: {
        vterm_data_list_ext_t *dp = &(*vtp)->data.list_ext;
        WRITE_U8(LIST_EXT);
        WRITE_U32(dp->len);
        for (size_t i = 0; i < (size_t)(dp->len); i++) {
            WRITE_VTERM(&dp->elements[i]);
        }
        WRITE_VTERM(&dp->tail);
        break;
    }
    case VTERM_TAG_BINARY_EXT: {
        vterm_data_binary_ext_t *dp = &(*vtp)->data.binary_ext;
        WRITE_U8(BINARY_EXT);
        WRITE_U32(dp->len);
        WRITE_BYTES(dp->data, dp->len);
        break;
    }
    case VTERM_TAG_BIT_BINARY_EXT: {
        vterm_data_bit_binary_ext_t *dp = &(*vtp)->data.bit_binary_ext;
        WRITE_U8(BIT_BINARY_EXT);
        WRITE_U32(dp->len);
        WRITE_U8(dp->bits);
        WRITE_BYTES(dp->data, dp->len);
        break;
    }
    case VTERM_TAG_SMALL_BIG_EXT: {
        vterm_data_small_big_ext_t *dp = &(*vtp)->data.small_big_ext;
        WRITE_U8(SMALL_BIG_EXT);
        WRITE_U8(dp->n);
        WRITE_U8(dp->sign);
        WRITE_BYTES(dp->d, dp->n);
        break;
    }
    case VTERM_TAG_LARGE_BIG_EXT: {
        vterm_data_large_big_ext_t *dp = &(*vtp)->data.large_big_ext;
        WRITE_U8(LARGE_BIG_EXT);
        WRITE_U32(dp->n);
        WRITE_U8(dp->sign);
        WRITE_BYTES(dp->d, dp->n);
        break;
    }
    case VTERM_TAG_NEW_FUN_EXT: {
        vterm_data_new_fun_ext_t *dp = &(*vtp)->data.new_fun_ext;
        WRITE_U8(NEW_FUN_EXT);
        WRITE_U32(dp->size);
        WRITE_U8(dp->arity);
        WRITE_BYTES(dp->uniq, 16);
        WRITE_U32(dp->index);
        WRITE_U32(dp->num_free);
        WRITE_ATOM(&dp->mod);
        WRITE_FIXED_INTEGER(&dp->old_index);
        WRITE_FIXED_INTEGER(&dp->old_uniq);
        WRITE_PID(&dp->pid);
        for (size_t i = 0; i < (size_t)(dp->num_free); i++) {
            WRITE_VTERM(&dp->free_vars[i]);
        }
        break;
    }
    case VTERM_TAG_EXPORT_EXT: {
        vterm_data_export_ext_t *dp = &(*vtp)->data.export_ext;
        WRITE_U8(EXPORT_EXT);
        WRITE_ATOM(&dp->mod);
        WRITE_ATOM(&dp->fun);
        WRITE_FIXED_INTEGER(&dp->arity);
        break;
    }
    case VTERM_TAG_MAP_EXT: {
        vterm_data_map_ext_t *dp = &(*vtp)->data.map_ext;
        WRITE_U8(MAP_EXT);
        WRITE_U32(dp->arity);
        for (size_t i = 0; i < (size_t)(dp->arity * 2); i += 2) {
            WRITE_VTERM(&dp->pairs[i]);
            WRITE_VTERM(&dp->pairs[i + 1]);
        }
        break;
    }
    case VTERM_TAG_ATOM_UTF8_EXT: {
        vterm_data_atom_utf8_ext_t *dp = &(*vtp)->data.atom_utf8_ext;
        WRITE_U8(ATOM_UTF8_EXT);
        WRITE_U16(dp->len);
        WRITE_BYTES(dp->name, dp->len);
        break;
    }
    case VTERM_TAG_SMALL_ATOM_UTF8_EXT: {
        vterm_data_small_atom_utf8_ext_t *dp = &(*vtp)->data.small_atom_utf8_ext;
        WRITE_U8(SMALL_ATOM_UTF8_EXT);
        WRITE_U8(dp->len);
        WRITE_BYTES(dp->name, dp->len);
        break;
    }
    case VTERM_TAG_V4_PORT_EXT: {
        vterm_data_v4_port_ext_t *dp = &(*vtp)->data.v4_port_ext;
        WRITE_U8(V4_PORT_EXT);
        WRITE_ATOM(&dp->node);
        WRITE_U64(dp->id);
        WRITE_U32(dp->creation);
        break;
    }
    case VTERM_TAG_ATOM_CACHE_REF: {
        vterm_data_atom_cache_ref_t *dp = &(*vtp)->data.atom_cache_ref;
        if ((flags & VTERM_ENCODE_FLAG_ALLOW_ATOM_CACHE_REFS) == 0) {
            return 0;
        }
        WRITE_U8(ATOM_CACHE_REF);
        WRITE_U8(dp->index);
        break;
    }
    default:
        return 0;
    }

    return 1;

#undef WRITE_VTERM
#undef WRITE_U64
#undef WRITE_U32
#undef WRITE_U16
#undef WRITE_U8
#undef WRITE_PID
#undef WRITE_I32
#undef WRITE_FIXED_INTEGER
#undef WRITE_BYTES
#undef WRITE_ATOM
}

int
encode_write_vterm_atom(ErlNifEnv *caller_env, int flags, vterm_env_t *vtenv, vterm_t *vtp, vec_writer_t *vw)
{
    int internal_flags = (flags & (~VTERM_ENCODE_FLAG_IS_EXTERNAL));

#define WRITE_BYTES(buf, len)                                                                                                      \
    do {                                                                                                                           \
        if (!vec_writer_write_exact(vw, (buf), (len))) {                                                                           \
                                                                                                                                   \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define WRITE_U8(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_writer_write_u8(vw, (val))) {                                                                                     \
                                                                                                                                   \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define WRITE_U16(val)                                                                                                             \
    do {                                                                                                                           \
        if (!vec_writer_write_u16(vw, (val))) {                                                                                    \
                                                                                                                                   \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

    (void)internal_flags;

    if ((flags & VTERM_ENCODE_FLAG_IS_EXTERNAL) != 0) {
        WRITE_U8(VERSION_MAGIC);
    }

    if (!vterm_maybe_decode_lazy_term(vtenv, vtp)) {
        return 0;
    }

    switch ((*vtp)->tag) {
    case VTERM_TAG_ATOM_CACHE_REF_RESOLVED: {
        const vterm_data_atom_cache_ref_resolved_t *dp = &(*vtp)->data.atom_cache_ref_resolved;
        if ((flags & VTERM_ENCODE_FLAG_ALLOW_ATOM_CACHE_REFS) != 0) {
            WRITE_U8(ATOM_CACHE_REF);
            WRITE_U8(dp->index);
            break;
        } else {
            ErlNifCharEncoding atom_encoding = ERL_NIF_UTF8;
            const uint8_t *atom_text = NULL;
            size_t atom_len = 0;
            if (!edf_atom_text_get_length(dp->term, atom_encoding, &atom_len)) {
                atom_encoding = ERL_NIF_LATIN1;
                atom_len = 0;
                if (edf_atom_text_get_length(dp->term, atom_encoding, &atom_len)) {
                    return 0;
                }
            }
            if (atom_len > MAX_ATOM_CHARACTERS) {
                if (atom_encoding == ERL_NIF_UTF8) {
                    WRITE_U8(ATOM_UTF8_EXT);
                    WRITE_U16((uint16_t)(atom_len));
                } else {
                    WRITE_U8(ATOM_EXT);
                    WRITE_U16((uint16_t)(atom_len));
                }
            } else {
                if (atom_encoding == ERL_NIF_UTF8) {
                    WRITE_U8(SMALL_ATOM_UTF8_EXT);
                    WRITE_U8((uint8_t)(atom_len));
                } else {
                    WRITE_U8(SMALL_ATOM_EXT);
                    WRITE_U8((uint8_t)(atom_len));
                }
            }
            if (vec_is_null(vw->vec)) {
                WRITE_BYTES(NULL, atom_len);
            } else {
                if (!edf_atom_text_get_name(dp->term, atom_encoding, &atom_text, &atom_len)) {
                    return 0;
                }
                WRITE_BYTES(atom_text, atom_len);
            }
            break;
        }
    }
    case VTERM_TAG_ATOM_EXT: {
        vterm_data_atom_ext_t *dp = &(*vtp)->data.atom_ext;
        WRITE_U8(ATOM_EXT);
        WRITE_U16(dp->len);
        WRITE_BYTES(dp->name, dp->len);
        break;
    }
    case VTERM_TAG_SMALL_ATOM_EXT: {
        vterm_data_small_atom_ext_t *dp = &(*vtp)->data.small_atom_ext;
        WRITE_U8(SMALL_ATOM_EXT);
        WRITE_U8(dp->len);
        WRITE_BYTES(dp->name, dp->len);
        break;
    }
    case VTERM_TAG_ATOM_UTF8_EXT: {
        vterm_data_atom_utf8_ext_t *dp = &(*vtp)->data.atom_utf8_ext;
        WRITE_U8(ATOM_UTF8_EXT);
        WRITE_U16(dp->len);
        WRITE_BYTES(dp->name, dp->len);
        break;
    }
    case VTERM_TAG_SMALL_ATOM_UTF8_EXT: {
        vterm_data_small_atom_utf8_ext_t *dp = &(*vtp)->data.small_atom_utf8_ext;
        WRITE_U8(SMALL_ATOM_UTF8_EXT);
        WRITE_U8(dp->len);
        WRITE_BYTES(dp->name, dp->len);
        break;
    }
    case VTERM_TAG_ATOM_CACHE_REF: {
        vterm_data_atom_cache_ref_t *dp = &(*vtp)->data.atom_cache_ref;
        if ((flags & VTERM_ENCODE_FLAG_ALLOW_ATOM_CACHE_REFS) == 0) {
            return 0;
        }
        WRITE_U8(ATOM_CACHE_REF);
        WRITE_U8(dp->index);
        break;
    }
    default:
        return 0;
    }

    return 1;

#undef WRITE_U16
#undef WRITE_U8
#undef WRITE_BYTES
}

int
encode_write_vterm_fixed_integer(ErlNifEnv *caller_env, int flags, vterm_env_t *vtenv, vterm_t *vtp, vec_writer_t *vw)
{
    int internal_flags = (flags & (~VTERM_ENCODE_FLAG_IS_EXTERNAL));

#define WRITE_I32(val)                                                                                                             \
    do {                                                                                                                           \
        if (!vec_writer_write_i32(vw, (val))) {                                                                                    \
                                                                                                                                   \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define WRITE_U8(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_writer_write_u8(vw, (val))) {                                                                                     \
                                                                                                                                   \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

    (void)internal_flags;

    if ((flags & VTERM_ENCODE_FLAG_IS_EXTERNAL) != 0) {
        WRITE_U8(VERSION_MAGIC);
    }

    if (!vterm_maybe_decode_lazy_term(vtenv, vtp)) {
        return 0;
    }

    switch ((*vtp)->tag) {
    case VTERM_TAG_SMALL_INTEGER_EXT: {
        vterm_data_small_integer_ext_t *dp = &(*vtp)->data.small_integer_ext;
        WRITE_U8(SMALL_INTEGER_EXT);
        WRITE_U8(dp->value);
        break;
    }
    case VTERM_TAG_INTEGER_EXT: {
        vterm_data_integer_ext_t *dp = &(*vtp)->data.integer_ext;
        WRITE_U8(INTEGER_EXT);
        WRITE_I32(dp->value);
        break;
    }
    default:
        return 0;
    }

    return 1;

#undef WRITE_U8
#undef WRITE_I32
}

int
encode_write_vterm_pid(ErlNifEnv *caller_env, int flags, vterm_env_t *vtenv, vterm_t *vtp, vec_writer_t *vw)
{
    int internal_flags = (flags & (~VTERM_ENCODE_FLAG_IS_EXTERNAL));

#define WRITE_ATOM(vtp)                                                                                                            \
    do {                                                                                                                           \
        if (!encode_write_vterm_atom(caller_env, internal_flags, vtenv, (vtp), vw)) {                                              \
                                                                                                                                   \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define WRITE_U8(val)                                                                                                              \
    do {                                                                                                                           \
        if (!vec_writer_write_u8(vw, (val))) {                                                                                     \
                                                                                                                                   \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

#define WRITE_U32(val)                                                                                                             \
    do {                                                                                                                           \
        if (!vec_writer_write_u32(vw, (val))) {                                                                                    \
                                                                                                                                   \
            return 0;                                                                                                              \
        }                                                                                                                          \
    } while (0)

    (void)internal_flags;

    if ((flags & VTERM_ENCODE_FLAG_IS_EXTERNAL) != 0) {
        WRITE_U8(VERSION_MAGIC);
    }

    if (!vterm_maybe_decode_lazy_term(vtenv, vtp)) {
        return 0;
    }

    switch ((*vtp)->tag) {
    case VTERM_TAG_PID_EXT: {
        vterm_data_pid_ext_t *dp = &(*vtp)->data.pid_ext;
        WRITE_U8(PID_EXT);
        WRITE_ATOM(&dp->node);
        WRITE_U32(dp->id);
        WRITE_U32(dp->serial);
        WRITE_U8(dp->creation);
        break;
    }
    case VTERM_TAG_NEW_PID_EXT: {
        vterm_data_new_pid_ext_t *dp = &(*vtp)->data.new_pid_ext;
        WRITE_U8(NEW_PID_EXT);
        WRITE_ATOM(&dp->node);
        WRITE_U32(dp->id);
        WRITE_U32(dp->serial);
        WRITE_U32(dp->creation);
        break;
    }
    default:
        return 0;
    }

    return 1;

#undef WRITE_U32
#undef WRITE_U8
#undef WRITE_ATOM
}
