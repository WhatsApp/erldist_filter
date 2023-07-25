/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "vterm.h"
#include "../etf/etf_decode_vterm.h"
#include "../core/unreachable.h"

static void vterm_decode_lazy_term_done_cb(ErlNifEnv *env, etf_decode_vterm_trap_t *trap, void *arg, edf_trap_result_t *result);

int
vterm_decode_lazy_term(vterm_env_t *vtenv, vterm_t *vtp, ERL_NIF_TERM *unsafe_err_termp)
{
    vterm_data_lazy_term_t *dp = NULL;
    vterm_data_lazy_term_t dpcopy[1];
    vterm_env_ctx_t old_ctx[1];
    vterm_env_ctx_t new_ctx[1];
    vec_t slice;
    edf_trap_result_t trap_result;
    if (!vterm_is_lazy_term(vtenv, vtp)) {
        if (unsafe_err_termp != NULL) {
            *unsafe_err_termp = THE_NON_VALUE;
        }
        return 0;
    }
    dp = &(*vtp)->data.lazy_term;
    *dpcopy = *dp;
    new_ctx->hp_start = (void *)*vtp;
    new_ctx->hp_end = new_ctx->hp_start + dpcopy->heap_size;
    new_ctx->hp = new_ctx->hp_start;
    (void)vec_init_free(&slice);
    if (!vec_create_from_slice(&slice, dpcopy->slice.head, dpcopy->slice.tail)) {
        if (unsafe_err_termp != NULL) {
            *unsafe_err_termp = THE_NON_VALUE;
        }
        return 0;
    }
    (void)vterm_env_ctx_swap(vtenv, new_ctx, old_ctx);
    trap_result = etf_decode_vterm_blocking(vtenv->nif_env, vtenv, false, vtenv->attab, &slice, 1, vterm_decode_lazy_term_done_cb,
                                            (void *)vtp);
    (void)vterm_env_ctx_swap(vtenv, old_ctx, new_ctx);
    (void)vec_destroy(&slice);
    switch (trap_result.tag) {
    case EDF_TRAP_RESULT_TAG_OK:
        return 1;
    case EDF_TRAP_RESULT_TAG_ERR: {
        if (unsafe_err_termp != NULL) {
            *unsafe_err_termp = trap_result.err;
        }
        return 0;
    }
    default:
        unreachable();
    }
}

void
vterm_decode_lazy_term_done_cb(ErlNifEnv *env, etf_decode_vterm_trap_t *trap, void *arg, edf_trap_result_t *result)
{
    vterm_t *vtp = (void *)arg;

    (void)env;

    if (result->tag != EDF_TRAP_RESULT_TAG_OK) {
        return;
    }

    *vtp = trap->vterm;

    return;
}

vterm_t
vterm_make_lazy_term(vterm_env_t *vtenv, const uint8_t *head, const uint8_t *tail, size_t heap_size)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, heap_size);
    vterm_data_lazy_term_t *dp = &vt->data.lazy_term;
    vt->tag = VTERM_TAG_LAZY_TERM;
    dp->slice.head = head;
    dp->slice.tail = tail;
    dp->heap_size = heap_size;
    return vt;
}

vterm_t
vterm_make_nif_term(vterm_env_t *vtenv, ERL_NIF_TERM term)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_NIF_TERM()); // 16
    vterm_data_nif_term_t *dp = &vt->data.nif_term;
    vt->tag = VTERM_TAG_NIF_TERM; // 8
    dp->term = term;              // 8
    return vt;
}
vterm_t
vterm_make_atom_cache_ref_resolved(vterm_env_t *vtenv, uint8_t index, ERL_NIF_TERM term)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_ATOM_CACHE_REF_RESOLVED()); // 17
    vterm_data_atom_cache_ref_resolved_t *dp = &vt->data.atom_cache_ref_resolved;
    vt->tag = VTERM_TAG_ATOM_CACHE_REF_RESOLVED; // 8
    dp->index = index;                           // 1
    dp->term = term;                             // 8
    return vt;
}
vterm_t
vterm_make_the_non_value(vterm_env_t *vtenv)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_THE_NON_VALUE()); // 8
    vt->tag = VTERM_TAG_THE_NON_VALUE;                                               // 8
    return vt;
}

vterm_t
vterm_make_small_integer_ext(vterm_env_t *vtenv, uint8_t value)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_SMALL_INTEGER_EXT()); // 9
    vterm_data_small_integer_ext_t *dp = &vt->data.small_integer_ext;
    vt->tag = VTERM_TAG_SMALL_INTEGER_EXT; // 8
    dp->value = value;                     // 1
    return vt;
}

vterm_t
vterm_make_integer_ext(vterm_env_t *vtenv, int32_t value)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_INTEGER_EXT()); // 12
    vterm_data_integer_ext_t *dp = &vt->data.integer_ext;
    vt->tag = VTERM_TAG_INTEGER_EXT; // 8
    dp->value = value;               // 4
    return vt;
}

vterm_t
vterm_make_float_ext(vterm_env_t *vtenv, uint8_t *float_string)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_FLOAT_EXT()); // 16
    vterm_data_float_ext_t *dp = &vt->data.float_ext;
    vt->tag = VTERM_TAG_FLOAT_EXT;   // 8
    dp->float_string = float_string; // 8
    return vt;
}

vterm_t
vterm_make_atom_ext(vterm_env_t *vtenv, uint16_t len, uint8_t *name)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_ATOM_EXT()); // 18
    vterm_data_atom_ext_t *dp = &vt->data.atom_ext;
    vt->tag = VTERM_TAG_ATOM_EXT; // 8
    dp->len = len;                // 2
    dp->name = name;              // 8
    return vt;
}

vterm_t
vterm_make_small_atom_ext(vterm_env_t *vtenv, uint8_t len, uint8_t *name)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_SMALL_ATOM_EXT()); // 17
    vterm_data_small_atom_ext_t *dp = &vt->data.small_atom_ext;
    vt->tag = VTERM_TAG_SMALL_ATOM_EXT; // 8
    dp->len = len;                      // 1
    dp->name = name;                    // 8
    return vt;
}

vterm_t
vterm_make_reference_ext(vterm_env_t *vtenv, vterm_t node, uint32_t id, uint8_t creation)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_REFERENCE_EXT()); // 21
    vterm_data_reference_ext_t *dp = &vt->data.reference_ext;
    vt->tag = VTERM_TAG_REFERENCE_EXT; // 8
    dp->node = node;                   // 8
    dp->id = id;                       // 4
    dp->creation = creation;           // 1
    return vt;
}

vterm_t
vterm_make_new_reference_ext(vterm_env_t *vtenv, uint16_t len, vterm_t node, uint8_t creation, uint32_t **ids)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_NEW_REFERENCE_EXT(len)); // 27
    vterm_data_new_reference_ext_t *dp = &vt->data.new_reference_ext;
    vt->tag = VTERM_TAG_NEW_REFERENCE_EXT; // 8
    dp->len = len;                         // 2
    dp->node = node;                       // 8
    dp->creation = creation;               // 1
    dp->ids = (void *)&dp->_dynamic[0];    // 8
    *ids = dp->ids;
    return vt;
}

vterm_t
vterm_make_newer_reference_ext(vterm_env_t *vtenv, uint16_t len, vterm_t node, uint32_t creation, uint32_t **ids)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_NEWER_REFERENCE_EXT(len)); // 30
    vterm_data_newer_reference_ext_t *dp = &vt->data.newer_reference_ext;
    vt->tag = VTERM_TAG_NEWER_REFERENCE_EXT; // 8
    dp->len = len;                           // 2
    dp->node = node;                         // 8
    dp->creation = creation;                 // 4
    dp->ids = (void *)&dp->_dynamic[0];      // 8
    *ids = dp->ids;
    return vt;
}

vterm_t
vterm_make_port_ext(vterm_env_t *vtenv, vterm_t node, uint32_t id, uint8_t creation)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_PORT_EXT()); // 21
    vterm_data_port_ext_t *dp = &vt->data.port_ext;
    vt->tag = VTERM_TAG_PORT_EXT; // 8
    dp->node = node;              // 8
    dp->id = id;                  // 4
    dp->creation = creation;      // 1
    return vt;
}

vterm_t
vterm_make_new_port_ext(vterm_env_t *vtenv, vterm_t node, uint32_t id, uint32_t creation)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_NEW_PORT_EXT()); // 24
    vterm_data_new_port_ext_t *dp = &vt->data.new_port_ext;
    vt->tag = VTERM_TAG_NEW_PORT_EXT; // 8
    dp->node = node;                  // 8
    dp->id = id;                      // 4
    dp->creation = creation;          // 4
    return vt;
}

vterm_t
vterm_make_new_float_ext(vterm_env_t *vtenv, uint8_t *ieee_float)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_NEW_FLOAT_EXT()); // 16
    vterm_data_new_float_ext_t *dp = &vt->data.new_float_ext;
    vt->tag = VTERM_TAG_NEW_FLOAT_EXT; // 8
    dp->ieee_float = ieee_float;       // 8
    return vt;
}

vterm_t
vterm_make_pid_ext(vterm_env_t *vtenv, vterm_t node, uint32_t id, uint32_t serial, uint8_t creation)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_PID_EXT()); // 25
    vterm_data_pid_ext_t *dp = &vt->data.pid_ext;
    vt->tag = VTERM_TAG_PID_EXT; // 8
    dp->node = node;             // 8
    dp->id = id;                 // 4
    dp->serial = serial;         // 4
    dp->creation = creation;     // 1
    return vt;
}

vterm_t
vterm_make_new_pid_ext(vterm_env_t *vtenv, vterm_t node, uint32_t id, uint32_t serial, uint32_t creation)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_NEW_PID_EXT()); // 28
    vterm_data_new_pid_ext_t *dp = &vt->data.new_pid_ext;
    vt->tag = VTERM_TAG_NEW_PID_EXT; // 8
    dp->node = node;                 // 8
    dp->id = id;                     // 4
    dp->serial = serial;             // 4
    dp->creation = creation;         // 4
    return vt;
}

vterm_t
vterm_make_small_tuple_ext(vterm_env_t *vtenv, uint8_t arity, vterm_t **elements)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_SMALL_TUPLE_EXT(arity)); // 17
    vterm_data_small_tuple_ext_t *dp = &vt->data.small_tuple_ext;
    vt->tag = VTERM_TAG_SMALL_TUPLE_EXT;     // 8
    dp->arity = arity;                       // 1
    dp->elements = (void *)&dp->_dynamic[0]; // 8
    *elements = dp->elements;
    return vt;
}

vterm_t
vterm_make_large_tuple_ext(vterm_env_t *vtenv, uint32_t arity, vterm_t **elements)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_LARGE_TUPLE_EXT(arity)); // 20
    vterm_data_large_tuple_ext_t *dp = &vt->data.large_tuple_ext;
    vt->tag = VTERM_TAG_LARGE_TUPLE_EXT;     // 8
    dp->arity = arity;                       // 4
    dp->elements = (void *)&dp->_dynamic[0]; // 8
    *elements = dp->elements;
    return vt;
}

vterm_t
vterm_make_nil_ext(vterm_env_t *vtenv)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_NIL_EXT()); // 8
    vt->tag = VTERM_TAG_NIL_EXT;                                               // 8
    return vt;
}

vterm_t
vterm_make_string_ext(vterm_env_t *vtenv, uint16_t len, uint8_t *characters)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_STRING_EXT()); // 18
    vterm_data_string_ext_t *dp = &vt->data.string_ext;
    vt->tag = VTERM_TAG_STRING_EXT; // 8
    dp->len = len;                  // 2
    dp->characters = characters;    // 8
    return vt;
}

vterm_t
vterm_make_list_ext(vterm_env_t *vtenv, uint32_t len, vterm_t **elements, vterm_t tail)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_LIST_EXT(len)); // 28
    vterm_data_list_ext_t *dp = &vt->data.list_ext;
    vt->tag = VTERM_TAG_LIST_EXT;            // 8
    dp->len = len;                           // 4
    dp->elements = (void *)&dp->_dynamic[0]; // 8
    *elements = dp->elements;
    dp->tail = tail; // 8
    return vt;
}

vterm_t
vterm_make_binary_ext(vterm_env_t *vtenv, uint32_t len, uint8_t *data)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_BINARY_EXT()); // 20
    vterm_data_binary_ext_t *dp = &vt->data.binary_ext;
    vt->tag = VTERM_TAG_BINARY_EXT; // 8
    dp->len = len;                  // 4
    dp->data = data;                // 8
    return vt;
}

vterm_t
vterm_make_bit_binary_ext(vterm_env_t *vtenv, uint32_t len, uint8_t bits, uint8_t *data)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_BIT_BINARY_EXT()); // 21
    vterm_data_bit_binary_ext_t *dp = &vt->data.bit_binary_ext;
    vt->tag = VTERM_TAG_BIT_BINARY_EXT; // 8
    dp->len = len;                      // 4
    dp->bits = bits;                    // 1
    dp->data = data;                    // 8
    return vt;
}

vterm_t
vterm_make_small_big_ext(vterm_env_t *vtenv, uint8_t n, uint8_t sign, uint8_t *d)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_SMALL_BIG_EXT()); // 18
    vterm_data_small_big_ext_t *dp = &vt->data.small_big_ext;
    vt->tag = VTERM_TAG_SMALL_BIG_EXT; // 8
    dp->n = n;                         // 1
    dp->sign = sign;                   // 1
    dp->d = d;                         // 8
    return vt;
}

vterm_t
vterm_make_large_big_ext(vterm_env_t *vtenv, uint32_t n, uint8_t sign, uint8_t *d)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_LARGE_BIG_EXT()); // 21
    vterm_data_large_big_ext_t *dp = &vt->data.large_big_ext;
    vt->tag = VTERM_TAG_LARGE_BIG_EXT; // 8
    dp->n = n;                         // 4
    dp->sign = sign;                   // 1
    dp->d = d;                         // 8
    return vt;
}

vterm_t
vterm_make_new_fun_ext(vterm_env_t *vtenv, uint32_t size, uint8_t arity, uint8_t *uniq, uint32_t index, uint32_t num_free,
                       vterm_t mod, vterm_t old_index, vterm_t old_uniq, vterm_t pid, vterm_t **free_vars)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_NEW_FUN_EXT(num_free)); // 69
    vterm_data_new_fun_ext_t *dp = &vt->data.new_fun_ext;
    vt->tag = VTERM_TAG_NEW_FUN_EXT;          // 8
    dp->size = size;                          // 4
    dp->arity = arity;                        // 1
    dp->uniq = uniq;                          // 8
    dp->index = index;                        // 4
    dp->num_free = num_free;                  // 4
    dp->mod = mod;                            // 8
    dp->old_index = old_index;                // 8
    dp->old_uniq = old_uniq;                  // 8
    dp->pid = pid;                            // 8
    dp->free_vars = (void *)&dp->_dynamic[0]; // 8
    *free_vars = dp->free_vars;
    return vt;
}

vterm_t
vterm_make_export_ext(vterm_env_t *vtenv, vterm_t mod, vterm_t fun, vterm_t arity)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_EXPORT_EXT()); // 32
    vterm_data_export_ext_t *dp = &vt->data.export_ext;
    vt->tag = VTERM_TAG_EXPORT_EXT; // 8
    dp->mod = mod;                  // 8
    dp->fun = fun;                  // 8
    dp->arity = arity;              // 8
    return vt;
}

vterm_t
vterm_make_map_ext(vterm_env_t *vtenv, uint32_t arity, vterm_t **pairs)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_MAP_EXT(arity)); // 20
    vterm_data_map_ext_t *dp = &vt->data.map_ext;
    vt->tag = VTERM_TAG_MAP_EXT;          // 8
    dp->arity = arity;                    // 4
    dp->pairs = (void *)&dp->_dynamic[0]; // 8
    *pairs = dp->pairs;
    return vt;
}

vterm_t
vterm_make_atom_utf8_ext(vterm_env_t *vtenv, uint16_t len, uint8_t *name)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_ATOM_UTF8_EXT()); // 18
    vterm_data_atom_utf8_ext_t *dp = &vt->data.atom_utf8_ext;
    vt->tag = VTERM_TAG_ATOM_UTF8_EXT; // 8
    dp->len = len;                     // 2
    dp->name = name;                   // 8
    return vt;
}

vterm_t
vterm_make_small_atom_utf8_ext(vterm_env_t *vtenv, uint8_t len, uint8_t *name)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_SMALL_ATOM_UTF8_EXT()); // 17
    vterm_data_small_atom_utf8_ext_t *dp = &vt->data.small_atom_utf8_ext;
    vt->tag = VTERM_TAG_SMALL_ATOM_UTF8_EXT; // 8
    dp->len = len;                           // 1
    dp->name = name;                         // 8
    return vt;
}

vterm_t
vterm_make_v4_port_ext(vterm_env_t *vtenv, vterm_t node, uint64_t id, uint32_t creation)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_V4_PORT_EXT()); // 28
    vterm_data_v4_port_ext_t *dp = &vt->data.v4_port_ext;
    vt->tag = VTERM_TAG_V4_PORT_EXT; // 8
    dp->node = node;                 // 8
    dp->id = id;                     // 8
    dp->creation = creation;         // 4
    return vt;
}

vterm_t
vterm_make_atom_cache_ref(vterm_env_t *vtenv, uint8_t index)
{
    vterm_t vt = vterm_env_heap_reserve_strict(vtenv, VTERM_SIZEOF_ATOM_CACHE_REF()); // 9
    vterm_data_atom_cache_ref_t *dp = &vt->data.atom_cache_ref;
    vt->tag = VTERM_TAG_ATOM_CACHE_REF; // 8
    dp->index = index;                  // 1
    return vt;
}
