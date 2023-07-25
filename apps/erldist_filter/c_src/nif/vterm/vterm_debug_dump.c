/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "vterm.h"

#include "../core/xnif_trace.h"

int
vterm_debug_dump(ErlNifEnv *env, vterm_env_t *vtenv, vterm_t *vtp, ERL_NIF_TERM *term)
{
#define COPY_BYTES(dst, src, sz)                                                                                                   \
    do {                                                                                                                           \
        ErlNifBinary bin;                                                                                                          \
        if (!enif_alloc_binary((sz), &bin)) {                                                                                      \
            return 0;                                                                                                              \
        }                                                                                                                          \
        if (bin.size != (sz)) {                                                                                                    \
            (void)enif_release_binary(&bin);                                                                                       \
            return 0;                                                                                                              \
        }                                                                                                                          \
        (void)memcpy(bin.data, (src), bin.size);                                                                                   \
        (dst) = enif_make_binary(env, &bin);                                                                                       \
    } while (0)

#define MAKE_SUB_TERM(dst, sub_term)                                                                                               \
    do {                                                                                                                           \
        if (!vterm_debug_dump(env, vtenv, &(sub_term), &(dst))) {                                                                  \
            goto error;                                                                                                            \
        }                                                                                                                          \
    } while (0)

    vterm_t vt = *vtp;

    switch (vt->tag) {
    case VTERM_TAG_LAZY_TERM: {
        ERL_NIF_TERM bin_term;
        COPY_BYTES(bin_term, vt->data.lazy_term.slice.head,
                   (size_t)(vt->data.lazy_term.slice.tail - vt->data.lazy_term.slice.head));
        *term = enif_make_tuple2(env, ATOM(vterm_lazy_term), bin_term);
        break;
    }
    case VTERM_TAG_THE_NON_VALUE:
        *term = enif_make_tuple1(env, ATOM(vterm_the_non_value));
        break;
    case VTERM_TAG_NIF_TERM:
        *term = enif_make_tuple2(env, ATOM(vterm_nif_term), enif_make_copy(env, vt->data.nif_term.term));
        break;
    case VTERM_TAG_SMALL_INTEGER_EXT:
        *term = enif_make_tuple2(env, ATOM(vterm_small_integer_ext),
                                 enif_make_uint(env, (unsigned int)(vt->data.small_integer_ext.value)));
        break;
    case VTERM_TAG_INTEGER_EXT:
        *term = enif_make_tuple2(env, ATOM(vterm_integer_ext), enif_make_int(env, (int)(vt->data.integer_ext.value)));
        break;
    case VTERM_TAG_FLOAT_EXT: {
        ERL_NIF_TERM bin_term;
        COPY_BYTES(bin_term, vt->data.float_ext.float_string, 31);
        *term = enif_make_tuple2(env, ATOM(vterm_float_ext), bin_term);
        break;
    }
    case VTERM_TAG_ATOM_EXT: {
        ERL_NIF_TERM bin_term;
        COPY_BYTES(bin_term, vt->data.atom_ext.name, (size_t)(vt->data.atom_ext.len));
        *term = enif_make_tuple3(env, ATOM(vterm_atom_ext), enif_make_uint(env, (unsigned int)(vt->data.atom_ext.len)), bin_term);
        break;
    }
    case VTERM_TAG_SMALL_ATOM_EXT: {
        ERL_NIF_TERM bin_term;
        COPY_BYTES(bin_term, vt->data.small_atom_ext.name, (size_t)(vt->data.small_atom_ext.len));
        *term = enif_make_tuple3(env, ATOM(vterm_small_atom_ext), enif_make_uint(env, (unsigned int)(vt->data.small_atom_ext.len)),
                                 bin_term);
        break;
    }
    case VTERM_TAG_REFERENCE_EXT: {
        ERL_NIF_TERM node_term;
        MAKE_SUB_TERM(node_term, vt->data.reference_ext.node);
        *term = enif_make_tuple4(env, ATOM(vterm_reference_ext), node_term,
                                 enif_make_uint(env, (unsigned int)(vt->data.reference_ext.id)),
                                 enif_make_uint(env, (unsigned int)(vt->data.reference_ext.creation)));
        break;
    }
    case VTERM_TAG_NEW_REFERENCE_EXT: {
        ERL_NIF_TERM node_term;
        ERL_NIF_TERM *ids = NULL;
        ERL_NIF_TERM ids_term;
        size_t i;
        MAKE_SUB_TERM(node_term, vt->data.new_reference_ext.node);
        ids = vterm_env_heap_reserve(vtenv, sizeof(ERL_NIF_TERM) * vt->data.new_reference_ext.len);
        for (i = 0; i < (size_t)(vt->data.new_reference_ext.len); i++) {
            ids[i] = enif_make_uint(env, (unsigned int)(vt->data.new_reference_ext.ids[i]));
        }
        ids_term = enif_make_list_from_array(env, ids, (unsigned int)(vt->data.new_reference_ext.len));
        *term = enif_make_tuple5(env, ATOM(vterm_new_reference_ext),
                                 enif_make_uint(env, (unsigned int)(vt->data.new_reference_ext.len)), node_term,
                                 enif_make_uint(env, (unsigned int)(vt->data.new_reference_ext.creation)), ids_term);
        break;
    }
    case VTERM_TAG_NEWER_REFERENCE_EXT: {
        ERL_NIF_TERM node_term;
        ERL_NIF_TERM *ids = NULL;
        ERL_NIF_TERM ids_term;
        size_t i;
        MAKE_SUB_TERM(node_term, vt->data.newer_reference_ext.node);
        ids = vterm_env_heap_reserve(vtenv, sizeof(ERL_NIF_TERM) * vt->data.newer_reference_ext.len);
        for (i = 0; i < (size_t)(vt->data.newer_reference_ext.len); i++) {
            ids[i] = enif_make_uint(env, (unsigned int)(vt->data.newer_reference_ext.ids[i]));
        }
        ids_term = enif_make_list_from_array(env, ids, (unsigned int)(vt->data.newer_reference_ext.len));
        *term = enif_make_tuple5(env, ATOM(vterm_newer_reference_ext),
                                 enif_make_uint(env, (unsigned int)(vt->data.newer_reference_ext.len)), node_term,
                                 enif_make_uint(env, (unsigned int)(vt->data.newer_reference_ext.creation)), ids_term);
        break;
    }
    case VTERM_TAG_PORT_EXT: {
        ERL_NIF_TERM node_term;
        MAKE_SUB_TERM(node_term, vt->data.port_ext.node);
        *term = enif_make_tuple4(env, ATOM(vterm_port_ext), node_term, enif_make_uint(env, (unsigned int)(vt->data.port_ext.id)),
                                 enif_make_uint(env, (unsigned int)(vt->data.port_ext.creation)));
        break;
    }
    case VTERM_TAG_NEW_PORT_EXT: {
        ERL_NIF_TERM node_term;
        MAKE_SUB_TERM(node_term, vt->data.new_port_ext.node);
        *term = enif_make_tuple4(env, ATOM(vterm_new_port_ext), node_term,
                                 enif_make_uint(env, (unsigned int)(vt->data.new_port_ext.id)),
                                 enif_make_uint(env, (unsigned int)(vt->data.new_port_ext.creation)));
        break;
    }
    case VTERM_TAG_NEW_FLOAT_EXT: {
        ERL_NIF_TERM bin_term;
        COPY_BYTES(bin_term, vt->data.new_float_ext.ieee_float, 8);
        *term = enif_make_tuple2(env, ATOM(vterm_new_float_ext), bin_term);
        break;
    }
    case VTERM_TAG_PID_EXT: {
        ERL_NIF_TERM node_term;
        MAKE_SUB_TERM(node_term, vt->data.pid_ext.node);
        *term = enif_make_tuple5(env, ATOM(vterm_pid_ext), node_term, enif_make_uint(env, (unsigned int)(vt->data.pid_ext.id)),
                                 enif_make_uint(env, (unsigned int)(vt->data.pid_ext.serial)),
                                 enif_make_uint(env, (unsigned int)(vt->data.pid_ext.creation)));
        break;
    }
    case VTERM_TAG_NEW_PID_EXT: {
        ERL_NIF_TERM node_term;
        MAKE_SUB_TERM(node_term, vt->data.new_pid_ext.node);
        *term =
            enif_make_tuple5(env, ATOM(vterm_new_pid_ext), node_term, enif_make_uint(env, (unsigned int)(vt->data.new_pid_ext.id)),
                             enif_make_uint(env, (unsigned int)(vt->data.new_pid_ext.serial)),
                             enif_make_uint(env, (unsigned int)(vt->data.new_pid_ext.creation)));
        break;
    }
    case VTERM_TAG_SMALL_TUPLE_EXT: {
        ERL_NIF_TERM *elements = NULL;
        ERL_NIF_TERM elements_term;
        size_t i;
        elements = vterm_env_heap_reserve(vtenv, sizeof(ERL_NIF_TERM) * vt->data.small_tuple_ext.arity);
        for (i = 0; i < (size_t)(vt->data.small_tuple_ext.arity); i++) {
            MAKE_SUB_TERM(elements[i], vt->data.small_tuple_ext.elements[i]);
        }
        elements_term = enif_make_list_from_array(env, elements, (unsigned int)(vt->data.small_tuple_ext.arity));
        *term = enif_make_tuple3(env, ATOM(vterm_small_tuple_ext),
                                 enif_make_uint(env, (unsigned int)(vt->data.small_tuple_ext.arity)), elements_term);
        break;
    }
    case VTERM_TAG_LARGE_TUPLE_EXT: {
        ERL_NIF_TERM *elements = NULL;
        ERL_NIF_TERM elements_term;
        size_t i;
        elements = vterm_env_heap_reserve(vtenv, sizeof(ERL_NIF_TERM) * vt->data.large_tuple_ext.arity);
        for (i = 0; i < (size_t)(vt->data.large_tuple_ext.arity); i++) {
            MAKE_SUB_TERM(elements[i], vt->data.large_tuple_ext.elements[i]);
        }
        elements_term = enif_make_list_from_array(env, elements, (unsigned int)(vt->data.large_tuple_ext.arity));
        *term = enif_make_tuple3(env, ATOM(vterm_large_tuple_ext),
                                 enif_make_uint(env, (unsigned int)(vt->data.large_tuple_ext.arity)), elements_term);
        break;
    }
    case VTERM_TAG_NIL_EXT:
        *term = enif_make_tuple1(env, ATOM(vterm_nil_ext));
        break;
    case VTERM_TAG_STRING_EXT: {
        ERL_NIF_TERM bin_term;
        COPY_BYTES(bin_term, vt->data.string_ext.characters, (size_t)(vt->data.string_ext.len));
        *term =
            enif_make_tuple3(env, ATOM(vterm_string_ext), enif_make_uint(env, (unsigned int)(vt->data.string_ext.len)), bin_term);
        break;
    }
    case VTERM_TAG_LIST_EXT: {
        ERL_NIF_TERM *elements = NULL;
        ERL_NIF_TERM elements_term;
        ERL_NIF_TERM tail_term;
        size_t i;
        elements = vterm_env_heap_reserve(vtenv, sizeof(ERL_NIF_TERM) * vt->data.list_ext.len);
        for (i = 0; i < (size_t)(vt->data.list_ext.len); i++) {
            MAKE_SUB_TERM(elements[i], vt->data.list_ext.elements[i]);
        }
        MAKE_SUB_TERM(tail_term, vt->data.list_ext.tail);
        elements_term = enif_make_list_from_array(env, elements, (unsigned int)(vt->data.list_ext.len));
        *term = enif_make_tuple4(env, ATOM(vterm_list_ext), enif_make_uint(env, (unsigned int)(vt->data.list_ext.len)),
                                 elements_term, tail_term);
        break;
    }
    case VTERM_TAG_BINARY_EXT: {
        ERL_NIF_TERM bin_term;
        COPY_BYTES(bin_term, vt->data.binary_ext.data, (size_t)(vt->data.binary_ext.len));
        *term =
            enif_make_tuple3(env, ATOM(vterm_binary_ext), enif_make_uint(env, (unsigned int)(vt->data.binary_ext.len)), bin_term);
        break;
    }
    case VTERM_TAG_BIT_BINARY_EXT: {
        ERL_NIF_TERM bin_term;
        COPY_BYTES(bin_term, vt->data.bit_binary_ext.data, (size_t)(vt->data.bit_binary_ext.len));
        *term = enif_make_tuple4(env, ATOM(vterm_bit_binary_ext), enif_make_uint(env, (unsigned int)(vt->data.bit_binary_ext.len)),
                                 enif_make_uint(env, (unsigned int)(vt->data.bit_binary_ext.bits)), bin_term);
        break;
    }
    case VTERM_TAG_SMALL_BIG_EXT: {
        ERL_NIF_TERM bin_term;
        COPY_BYTES(bin_term, vt->data.small_big_ext.d, (size_t)(vt->data.small_big_ext.n));
        *term = enif_make_tuple4(env, ATOM(vterm_small_big_ext), enif_make_uint(env, (unsigned int)(vt->data.small_big_ext.n)),
                                 enif_make_uint(env, (unsigned int)(vt->data.small_big_ext.sign)), bin_term);
        break;
    }
    case VTERM_TAG_LARGE_BIG_EXT: {
        ERL_NIF_TERM bin_term;
        COPY_BYTES(bin_term, vt->data.large_big_ext.d, (size_t)(vt->data.large_big_ext.n));
        *term = enif_make_tuple4(env, ATOM(vterm_large_big_ext), enif_make_uint(env, (unsigned int)(vt->data.large_big_ext.n)),
                                 enif_make_uint(env, (unsigned int)(vt->data.large_big_ext.sign)), bin_term);
        break;
    }
    case VTERM_TAG_NEW_FUN_EXT: {
        ERL_NIF_TERM size_term;
        ERL_NIF_TERM arity_term;
        ERL_NIF_TERM uniq_term;
        ERL_NIF_TERM index_term;
        ERL_NIF_TERM num_free_term;
        ERL_NIF_TERM mod_term;
        ERL_NIF_TERM old_index_term;
        ERL_NIF_TERM old_uniq_term;
        ERL_NIF_TERM pid_term;
        ERL_NIF_TERM *free_vars = NULL;
        ERL_NIF_TERM free_vars_term;
        size_t i;
        free_vars = vterm_env_heap_reserve(vtenv, sizeof(ERL_NIF_TERM) * vt->data.new_fun_ext.num_free);
        size_term = enif_make_uint(env, (unsigned int)(vt->data.new_fun_ext.size));
        arity_term = enif_make_uint(env, (unsigned int)(vt->data.new_fun_ext.arity));
        COPY_BYTES(uniq_term, vt->data.new_fun_ext.uniq, 16);
        index_term = enif_make_uint(env, (unsigned int)(vt->data.new_fun_ext.index));
        num_free_term = enif_make_uint(env, (unsigned int)(vt->data.new_fun_ext.num_free));
        MAKE_SUB_TERM(mod_term, vt->data.new_fun_ext.mod);
        MAKE_SUB_TERM(old_index_term, vt->data.new_fun_ext.old_index);
        MAKE_SUB_TERM(old_uniq_term, vt->data.new_fun_ext.old_uniq);
        MAKE_SUB_TERM(pid_term, vt->data.new_fun_ext.pid);
        for (i = 0; i < (size_t)(vt->data.new_fun_ext.num_free); i++) {
            MAKE_SUB_TERM(free_vars[i], vt->data.new_fun_ext.free_vars[i]);
        }
        free_vars_term = enif_make_list_from_array(env, free_vars, (unsigned int)(vt->data.new_fun_ext.num_free));
        *term = enif_make_tuple(env, 11, ATOM(vterm_new_fun_ext), size_term, arity_term, uniq_term, index_term, num_free_term,
                                mod_term, old_index_term, old_uniq_term, pid_term, free_vars_term);
        break;
    }
    case VTERM_TAG_EXPORT_EXT: {
        ERL_NIF_TERM mod_term;
        ERL_NIF_TERM fun_term;
        ERL_NIF_TERM arity_term;
        MAKE_SUB_TERM(mod_term, vt->data.export_ext.mod);
        MAKE_SUB_TERM(fun_term, vt->data.export_ext.fun);
        MAKE_SUB_TERM(arity_term, vt->data.export_ext.arity);
        *term = enif_make_tuple4(env, ATOM(vterm_export_ext), mod_term, fun_term, arity_term);
        break;
    }
    case VTERM_TAG_MAP_EXT: {
        ERL_NIF_TERM *pairs = NULL;
        ERL_NIF_TERM key_term;
        ERL_NIF_TERM val_term;
        ERL_NIF_TERM pairs_term;
        size_t i;
        pairs = vterm_env_heap_reserve(vtenv, sizeof(ERL_NIF_TERM) * vt->data.map_ext.arity);
        for (i = 0; i < (size_t)(vt->data.map_ext.arity); i++) {
            MAKE_SUB_TERM(key_term, vt->data.map_ext.pairs[(i * 2) + 0]);
            MAKE_SUB_TERM(val_term, vt->data.map_ext.pairs[(i * 2) + 1]);
            pairs[i] = enif_make_tuple2(env, key_term, val_term);
        }
        pairs_term = enif_make_list_from_array(env, pairs, (unsigned int)(vt->data.map_ext.arity));
        *term = enif_make_tuple3(env, ATOM(vterm_map_ext), enif_make_uint(env, (unsigned int)(vt->data.map_ext.arity)), pairs_term);
        break;
    }
    case VTERM_TAG_ATOM_UTF8_EXT: {
        ERL_NIF_TERM bin_term;
        COPY_BYTES(bin_term, vt->data.atom_utf8_ext.name, (size_t)(vt->data.atom_utf8_ext.len));
        *term = enif_make_tuple3(env, ATOM(vterm_atom_utf8_ext), enif_make_uint(env, (unsigned int)(vt->data.atom_utf8_ext.len)),
                                 bin_term);
        break;
    }
    case VTERM_TAG_SMALL_ATOM_UTF8_EXT: {
        ERL_NIF_TERM bin_term;
        COPY_BYTES(bin_term, vt->data.small_atom_utf8_ext.name, (size_t)(vt->data.small_atom_utf8_ext.len));
        *term = enif_make_tuple3(env, ATOM(vterm_small_atom_utf8_ext),
                                 enif_make_uint(env, (unsigned int)(vt->data.small_atom_utf8_ext.len)), bin_term);
        break;
    }
    case VTERM_TAG_V4_PORT_EXT: {
        ERL_NIF_TERM node_term;
        MAKE_SUB_TERM(node_term, vt->data.v4_port_ext.node);
        *term = enif_make_tuple4(env, ATOM(vterm_v4_port_ext), node_term,
                                 enif_make_uint64(env, (ErlNifUInt64)(vt->data.v4_port_ext.id)),
                                 enif_make_uint(env, (unsigned int)(vt->data.v4_port_ext.creation)));
        break;
    }
    case VTERM_TAG_ATOM_CACHE_REF:
        *term =
            enif_make_tuple2(env, ATOM(vterm_atom_cache_ref), enif_make_uint(env, (unsigned int)(vt->data.atom_cache_ref.index)));
        break;
    case VTERM_TAG_ATOM_CACHE_REF_RESOLVED:
        *term = enif_make_tuple3(env, ATOM(vterm_atom_cache_ref_resolved),
                                 enif_make_uint(env, (unsigned int)(vt->data.atom_cache_ref_resolved.index)),
                                 enif_make_copy(env, vt->data.atom_cache_ref_resolved.term));
        break;
    default:
        goto error;
    }

    return 1;

error:
    return 0;

#undef MAKE_SUB_TERM
#undef COPY_BYTES
}
