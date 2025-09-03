/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef VTERM_H
#define VTERM_H

#ifdef __cplusplus
extern "C" {
#endif

#include "vterm_common.h"
#include "vterm_env.h"

/* Macro Definitions */

#define VTERM_ENCODE_FLAG_NONE (0)
#define VTERM_ENCODE_FLAG_IS_EXTERNAL (1 << 0)
#define VTERM_ENCODE_FLAG_ALLOW_ATOM_CACHE_REFS (1 << 1)

#define VTERM_SIZEOF_LAZY_TERM() (sizeof(vterm_tag_t) + sizeof(vterm_data_lazy_term_t))
#define VTERM_SIZEOF_NIF_TERM() (sizeof(vterm_tag_t) + sizeof(vterm_data_nif_term_t))
#define VTERM_SIZEOF_ATOM_CACHE_REF_RESOLVED() (sizeof(vterm_tag_t) + sizeof(vterm_data_atom_cache_ref_resolved_t))
#define VTERM_SIZEOF_THE_NON_VALUE() (sizeof(vterm_tag_t))
#define VTERM_SIZEOF_SMALL_INTEGER_EXT() (sizeof(vterm_tag_t) + sizeof(vterm_data_small_integer_ext_t))
#define VTERM_SIZEOF_INTEGER_EXT() (sizeof(vterm_tag_t) + sizeof(vterm_data_integer_ext_t))
#define VTERM_SIZEOF_FLOAT_EXT() (sizeof(vterm_tag_t) + sizeof(vterm_data_float_ext_t))
#define VTERM_SIZEOF_ATOM_EXT() (sizeof(vterm_tag_t) + sizeof(vterm_data_atom_ext_t))
#define VTERM_SIZEOF_SMALL_ATOM_EXT() (sizeof(vterm_tag_t) + sizeof(vterm_data_small_atom_ext_t))
#define VTERM_SIZEOF_REFERENCE_EXT() (sizeof(vterm_tag_t) + sizeof(vterm_data_reference_ext_t))
#define VTERM_SIZEOF_NEW_REFERENCE_EXT(len)                                                                                        \
    (sizeof(vterm_tag_t) + offsetof(vterm_data_new_reference_ext_t, _dynamic) + (sizeof(uint32_t) * (len)))
#define VTERM_SIZEOF_NEWER_REFERENCE_EXT(len)                                                                                      \
    (sizeof(vterm_tag_t) + offsetof(vterm_data_newer_reference_ext_t, _dynamic) + (sizeof(uint32_t) * (len)))
#define VTERM_SIZEOF_PORT_EXT() (sizeof(vterm_tag_t) + sizeof(vterm_data_port_ext_t))
#define VTERM_SIZEOF_NEW_PORT_EXT() (sizeof(vterm_tag_t) + sizeof(vterm_data_new_port_ext_t))
#define VTERM_SIZEOF_NEW_FLOAT_EXT() (sizeof(vterm_tag_t) + sizeof(vterm_data_new_float_ext_t))
#define VTERM_SIZEOF_PID_EXT() (sizeof(vterm_tag_t) + sizeof(vterm_data_pid_ext_t))
#define VTERM_SIZEOF_NEW_PID_EXT() (sizeof(vterm_tag_t) + sizeof(vterm_data_new_pid_ext_t))
#define VTERM_SIZEOF_SMALL_TUPLE_EXT(arity)                                                                                        \
    (sizeof(vterm_tag_t) + offsetof(vterm_data_small_tuple_ext_t, _dynamic) + (sizeof(vterm_t) * (arity)))
#define VTERM_SIZEOF_LARGE_TUPLE_EXT(arity)                                                                                        \
    (sizeof(vterm_tag_t) + offsetof(vterm_data_large_tuple_ext_t, _dynamic) + (sizeof(vterm_t) * (arity)))
#define VTERM_SIZEOF_NIL_EXT() (sizeof(vterm_tag_t))
#define VTERM_SIZEOF_STRING_EXT() (sizeof(vterm_tag_t) + sizeof(vterm_data_string_ext_t))
#define VTERM_SIZEOF_LIST_EXT(len) (sizeof(vterm_tag_t) + offsetof(vterm_data_list_ext_t, _dynamic) + (sizeof(vterm_t) * (len)))
#define VTERM_SIZEOF_BINARY_EXT() (sizeof(vterm_tag_t) + sizeof(vterm_data_binary_ext_t))
#define VTERM_SIZEOF_BIT_BINARY_EXT() (sizeof(vterm_tag_t) + sizeof(vterm_data_bit_binary_ext_t))
#define VTERM_SIZEOF_SMALL_BIG_EXT() (sizeof(vterm_tag_t) + sizeof(vterm_data_small_big_ext_t))
#define VTERM_SIZEOF_LARGE_BIG_EXT() (sizeof(vterm_tag_t) + sizeof(vterm_data_large_big_ext_t))
#define VTERM_SIZEOF_NEW_FUN_EXT(num_free)                                                                                         \
    (sizeof(vterm_tag_t) + offsetof(vterm_data_new_fun_ext_t, _dynamic) + (sizeof(vterm_t) * (num_free)))
#define VTERM_SIZEOF_EXPORT_EXT() (sizeof(vterm_tag_t) + sizeof(vterm_data_export_ext_t))
#define VTERM_SIZEOF_MAP_EXT(arity)                                                                                                \
    (sizeof(vterm_tag_t) + offsetof(vterm_data_map_ext_t, _dynamic) + (sizeof(vterm_t) * (arity * 2)))
#define VTERM_SIZEOF_ATOM_UTF8_EXT() (sizeof(vterm_tag_t) + sizeof(vterm_data_atom_utf8_ext_t))
#define VTERM_SIZEOF_SMALL_ATOM_UTF8_EXT() (sizeof(vterm_tag_t) + sizeof(vterm_data_small_atom_utf8_ext_t))
#define VTERM_SIZEOF_V4_PORT_EXT() (sizeof(vterm_tag_t) + sizeof(vterm_data_v4_port_ext_t))
#define VTERM_SIZEOF_ATOM_CACHE_REF() (sizeof(vterm_tag_t) + sizeof(vterm_data_atom_cache_ref_t))

/* Type Definitions */

typedef struct vterm_data_lazy_term_s vterm_data_lazy_term_t;
typedef struct vterm_data_nif_term_s vterm_data_nif_term_t;
typedef struct vterm_data_atom_cache_ref_resolved_s vterm_data_atom_cache_ref_resolved_t;
typedef struct vterm_data_small_integer_ext_s vterm_data_small_integer_ext_t;
typedef struct vterm_data_integer_ext_s vterm_data_integer_ext_t;
typedef struct vterm_data_float_ext_s vterm_data_float_ext_t;
typedef struct vterm_data_atom_ext_s vterm_data_atom_ext_t;
typedef struct vterm_data_small_atom_ext_s vterm_data_small_atom_ext_t;
typedef struct vterm_data_reference_ext_s vterm_data_reference_ext_t;
typedef struct vterm_data_new_reference_ext_s vterm_data_new_reference_ext_t;
typedef struct vterm_data_newer_reference_ext_s vterm_data_newer_reference_ext_t;
typedef struct vterm_data_port_ext_s vterm_data_port_ext_t;
typedef struct vterm_data_new_port_ext_s vterm_data_new_port_ext_t;
typedef struct vterm_data_new_float_ext_s vterm_data_new_float_ext_t;
typedef struct vterm_data_pid_ext_s vterm_data_pid_ext_t;
typedef struct vterm_data_new_pid_ext_s vterm_data_new_pid_ext_t;
typedef struct vterm_data_small_tuple_ext_s vterm_data_small_tuple_ext_t;
typedef struct vterm_data_large_tuple_ext_s vterm_data_large_tuple_ext_t;
typedef struct vterm_data_string_ext_s vterm_data_string_ext_t;
typedef struct vterm_data_list_ext_s vterm_data_list_ext_t;
typedef struct vterm_data_binary_ext_s vterm_data_binary_ext_t;
typedef struct vterm_data_bit_binary_ext_s vterm_data_bit_binary_ext_t;
typedef struct vterm_data_small_big_ext_s vterm_data_small_big_ext_t;
typedef struct vterm_data_large_big_ext_s vterm_data_large_big_ext_t;
typedef struct vterm_data_new_fun_ext_s vterm_data_new_fun_ext_t;
typedef struct vterm_data_export_ext_s vterm_data_export_ext_t;
typedef struct vterm_data_map_ext_s vterm_data_map_ext_t;
typedef struct vterm_data_atom_utf8_ext_s vterm_data_atom_utf8_ext_t;
typedef struct vterm_data_small_atom_utf8_ext_s vterm_data_small_atom_utf8_ext_t;
typedef struct vterm_data_v4_port_ext_s vterm_data_v4_port_ext_t;
typedef struct vterm_data_atom_cache_ref_s vterm_data_atom_cache_ref_t;

struct vterm_data_lazy_term_s {
    slice_t slice;
    size_t heap_size;
};

struct vterm_data_nif_term_s {
    ERL_NIF_TERM term;
};

struct vterm_data_atom_cache_ref_resolved_s {
    uint8_t index;
    ERL_NIF_TERM term;
};

struct vterm_data_small_integer_ext_s {
    uint8_t value;
};

struct vterm_data_integer_ext_s {
    int32_t value;
};

struct vterm_data_float_ext_s {
    uint8_t *float_string; // uint8_t[31]
};

struct vterm_data_atom_ext_s {
    uint16_t len;
    uint8_t *name; // uint8_t[len]
};

struct vterm_data_small_atom_ext_s {
    uint8_t len;
    uint8_t *name; // uint8_t[len]
};

struct vterm_data_reference_ext_s {
    vterm_t node; // atom
    uint32_t id;
    uint8_t creation;
};

struct vterm_data_new_reference_ext_s {
    uint16_t len;
    vterm_t node; // atom
    uint8_t creation;
    uint32_t *ids; // uint32_t[len]
    alignas(vword_t) vword_t _dynamic[1];
};

struct vterm_data_newer_reference_ext_s {
    uint16_t len;
    vterm_t node; // atom
    uint32_t creation;
    uint32_t *ids; // uint32_t[len]
    alignas(vword_t) vword_t _dynamic[1];
};

struct vterm_data_port_ext_s {
    vterm_t node; // atom
    uint32_t id;
    uint8_t creation;
};

struct vterm_data_new_port_ext_s {
    vterm_t node; // atom
    uint32_t id;
    uint32_t creation;
};

struct vterm_data_new_float_ext_s {
    uint8_t *ieee_float; // uint8_t[8]
};

struct vterm_data_pid_ext_s {
    vterm_t node; // atom
    uint32_t id;
    uint32_t serial;
    uint8_t creation;
};

struct vterm_data_new_pid_ext_s {
    vterm_t node; // atom
    uint32_t id;
    uint32_t serial;
    uint32_t creation;
};

struct vterm_data_small_tuple_ext_s {
    uint8_t arity;
    vterm_t *elements; // vterm_t[arity]
    alignas(vword_t) vword_t _dynamic[1];
};

struct vterm_data_large_tuple_ext_s {
    uint32_t arity;
    vterm_t *elements; // vterm_t[arity]
    alignas(vword_t) vword_t _dynamic[1];
};

struct vterm_data_string_ext_s {
    uint16_t len;
    uint8_t *characters; // uint8_t[len]
};

struct vterm_data_list_ext_s {
    uint32_t len;
    vterm_t *elements; // vterm_t[len]
    vterm_t tail;
    alignas(vword_t) vword_t _dynamic[1];
};

struct vterm_data_binary_ext_s {
    uint32_t len;
    uint8_t *data; // uint8_t[len]
};

struct vterm_data_bit_binary_ext_s {
    uint32_t len;
    uint8_t bits;
    uint8_t *data; // uint8_t[len]
};

struct vterm_data_small_big_ext_s {
    uint8_t n;
    uint8_t sign;
    uint8_t *d; // uint8_t[n]
};

struct vterm_data_large_big_ext_s {
    uint32_t n;
    uint8_t sign;
    uint8_t *d; // uint8_t[n]
};

struct vterm_data_new_fun_ext_s {
    uint32_t size;
    uint8_t arity;
    uint8_t *uniq; // uint8_t[16]
    uint32_t index;
    uint32_t num_free;
    vterm_t mod;        // atom
    vterm_t old_index;  // small_integer or integer
    vterm_t old_uniq;   // small_integer or integer
    vterm_t pid;        // pid
    vterm_t *free_vars; // vterm_t[num_free]
    alignas(vword_t) vword_t _dynamic[1];
};

struct vterm_data_export_ext_s {
    vterm_t mod;   // atom
    vterm_t fun;   // atom
    vterm_t arity; // small_integer
};

struct vterm_data_map_ext_s {
    uint32_t arity;
    vterm_t *pairs; // vterm_t[arity * 2]
    alignas(vword_t) vword_t _dynamic[1];
};

struct vterm_data_atom_utf8_ext_s {
    uint16_t len;
    uint8_t *name; // uint8_t[len]
};

struct vterm_data_small_atom_utf8_ext_s {
    uint8_t len;
    uint8_t *name; // uint8_t[len]
};

struct vterm_data_v4_port_ext_s {
    vterm_t node; // atom
    uint64_t id;
    uint32_t creation;
};

struct vterm_data_atom_cache_ref_s {
    uint8_t index;
};

struct __vterm_s {
    vterm_tag_t tag;
    union {
        vterm_data_lazy_term_t lazy_term;
        vterm_data_nif_term_t nif_term;
        vterm_data_atom_cache_ref_resolved_t atom_cache_ref_resolved;
        vterm_data_small_integer_ext_t small_integer_ext;
        vterm_data_integer_ext_t integer_ext;
        vterm_data_float_ext_t float_ext;
        vterm_data_atom_ext_t atom_ext;
        vterm_data_small_atom_ext_t small_atom_ext;
        vterm_data_reference_ext_t reference_ext;
        vterm_data_new_reference_ext_t new_reference_ext;
        vterm_data_newer_reference_ext_t newer_reference_ext;
        vterm_data_port_ext_t port_ext;
        vterm_data_new_port_ext_t new_port_ext;
        vterm_data_new_float_ext_t new_float_ext;
        vterm_data_pid_ext_t pid_ext;
        vterm_data_new_pid_ext_t new_pid_ext;
        vterm_data_small_tuple_ext_t small_tuple_ext;
        vterm_data_large_tuple_ext_t large_tuple_ext;
        vterm_data_string_ext_t string_ext;
        vterm_data_list_ext_t list_ext;
        vterm_data_binary_ext_t binary_ext;
        vterm_data_bit_binary_ext_t bit_binary_ext;
        vterm_data_small_big_ext_t small_big_ext;
        vterm_data_large_big_ext_t large_big_ext;
        vterm_data_new_fun_ext_t new_fun_ext;
        vterm_data_export_ext_t export_ext;
        vterm_data_map_ext_t map_ext;
        vterm_data_atom_utf8_ext_t atom_utf8_ext;
        vterm_data_small_atom_utf8_ext_t small_atom_utf8_ext;
        vterm_data_v4_port_ext_t v4_port_ext;
        vterm_data_atom_cache_ref_t atom_cache_ref;
    } data;
};

/* Function Declarations */

extern int vterm_debug_dump(ErlNifEnv *env, vterm_env_t *vtenv, vterm_t *vtp, ERL_NIF_TERM *term);
// extern int vterm_debug_fprint(FILE *stream, vterm_env_t *vtenv, vterm_t *vt);
// extern int vterm_debug_snprint(char *str, size_t size, vterm_env_t *vtenv, vterm_t *vt);
extern int vterm_decode_lazy_term(vterm_env_t *vtenv, vterm_t *vtp, ERL_NIF_TERM *unsafe_err_termp);
extern int vterm_encode(ErlNifEnv *caller_env, int flags, vterm_env_t *vtenv, vterm_t *vtp, vec_t *dst_vec);
extern int vterm_encode_length(ErlNifEnv *caller_env, int flags, vterm_env_t *vtenv, vterm_t *vtp, size_t *lenp);
extern int vterm_encode_and_try_resolve(ErlNifEnv *caller_env, vterm_env_t *vtenv, vterm_t *vtp, ERL_NIF_TERM *termp);

extern vterm_t vterm_make_lazy_term(vterm_env_t *vtenv, const uint8_t *head, const uint8_t *tail, size_t heap_size);
extern vterm_t vterm_make_nif_term(vterm_env_t *vtenv, ERL_NIF_TERM term);
extern vterm_t vterm_make_atom_cache_ref_resolved(vterm_env_t *vtenv, uint8_t index, ERL_NIF_TERM term);
extern vterm_t vterm_make_the_non_value(vterm_env_t *vtenv);
extern vterm_t vterm_make_small_integer_ext(vterm_env_t *vtenv, uint8_t value);
extern vterm_t vterm_make_integer_ext(vterm_env_t *vtenv, int32_t value);
extern vterm_t vterm_make_float_ext(vterm_env_t *vtenv, uint8_t *float_string);
extern vterm_t vterm_make_atom_ext(vterm_env_t *vtenv, uint16_t len, uint8_t *name);
extern vterm_t vterm_make_small_atom_ext(vterm_env_t *vtenv, uint8_t len, uint8_t *name);
extern vterm_t vterm_make_reference_ext(vterm_env_t *vtenv, vterm_t node, uint32_t id, uint8_t creation);
extern vterm_t vterm_make_new_reference_ext(vterm_env_t *vtenv, uint16_t len, vterm_t node, uint8_t creation, uint32_t **ids);
extern vterm_t vterm_make_newer_reference_ext(vterm_env_t *vtenv, uint16_t len, vterm_t node, uint32_t creation, uint32_t **ids);
extern vterm_t vterm_make_port_ext(vterm_env_t *vtenv, vterm_t node, uint32_t id, uint8_t creation);
extern vterm_t vterm_make_new_port_ext(vterm_env_t *vtenv, vterm_t node, uint32_t id, uint32_t creation);
extern vterm_t vterm_make_new_float_ext(vterm_env_t *vtenv, uint8_t *ieee_float);
extern vterm_t vterm_make_pid_ext(vterm_env_t *vtenv, vterm_t node, uint32_t id, uint32_t serial, uint8_t creation);
extern vterm_t vterm_make_new_pid_ext(vterm_env_t *vtenv, vterm_t node, uint32_t id, uint32_t serial, uint32_t creation);
extern vterm_t vterm_make_small_tuple_ext(vterm_env_t *vtenv, uint8_t arity, vterm_t **elements);
extern vterm_t vterm_make_large_tuple_ext(vterm_env_t *vtenv, uint32_t arity, vterm_t **elements);
extern vterm_t vterm_make_nil_ext(vterm_env_t *vtenv);
extern vterm_t vterm_make_string_ext(vterm_env_t *vtenv, uint16_t len, uint8_t *characters);
extern vterm_t vterm_make_list_ext(vterm_env_t *vtenv, uint32_t len, vterm_t **elements, vterm_t tail);
extern vterm_t vterm_make_binary_ext(vterm_env_t *vtenv, uint32_t len, uint8_t *data);
extern vterm_t vterm_make_bit_binary_ext(vterm_env_t *vtenv, uint32_t len, uint8_t bits, uint8_t *data);
extern vterm_t vterm_make_small_big_ext(vterm_env_t *vtenv, uint8_t n, uint8_t sign, uint8_t *d);
extern vterm_t vterm_make_large_big_ext(vterm_env_t *vtenv, uint32_t n, uint8_t sign, uint8_t *d);
extern vterm_t vterm_make_new_fun_ext(vterm_env_t *vtenv, uint32_t size, uint8_t arity, uint8_t *uniq, uint32_t index,
                                      uint32_t num_free, vterm_t mod, vterm_t old_index, vterm_t old_uniq, vterm_t pid,
                                      vterm_t **free_vars);
extern vterm_t vterm_make_export_ext(vterm_env_t *vtenv, vterm_t mod, vterm_t fun, vterm_t arity);
extern vterm_t vterm_make_map_ext(vterm_env_t *vtenv, uint32_t arity, vterm_t **pairs);
extern vterm_t vterm_make_atom_utf8_ext(vterm_env_t *vtenv, uint16_t len, uint8_t *name);
extern vterm_t vterm_make_small_atom_utf8_ext(vterm_env_t *vtenv, uint8_t len, uint8_t *name);
extern vterm_t vterm_make_v4_port_ext(vterm_env_t *vtenv, vterm_t node, uint64_t id, uint32_t creation);
extern vterm_t vterm_make_atom_cache_ref(vterm_env_t *vtenv, uint8_t index);

static int vterm_is_lazy_term(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_nif_term(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_atom_cache_ref_resolved(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_the_non_value(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_small_integer_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_integer_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_float_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_atom_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_small_atom_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_reference_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_new_reference_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_newer_reference_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_port_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_new_port_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_new_float_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_pid_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_new_pid_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_small_tuple_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_large_tuple_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_nil_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_string_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_list_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_binary_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_bit_binary_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_small_big_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_large_big_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_new_fun_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_export_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_map_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_atom_utf8_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_small_atom_utf8_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_v4_port_ext(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_atom_cache_ref(vterm_env_t *vtenv, const vterm_t *vtp);

static int vterm_is_atom(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_fixed_integer(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_integer(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_list(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_pid(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_port(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_reference(vterm_env_t *vtenv, const vterm_t *vtp);
static int vterm_is_tuple(vterm_env_t *vtenv, const vterm_t *vtp);

static int vterm_is_likely_gen_reply_tag(vterm_env_t *vtenv, vterm_t *vtp);
static int vterm_is_proper_list(vterm_env_t *vtenv, vterm_t *vtp);
static int vterm_get_atom(vterm_env_t *vtenv, vterm_t *vtp, ERL_NIF_TERM *atomp);
static int vterm_get_atom_text(vterm_env_t *vtenv, vterm_t *vtp, ErlNifCharEncoding *encodingp, const uint8_t **namep,
                               size_t *lenp);
static int vterm_get_fixed_integer(vterm_env_t *vtenv, vterm_t *vtp, int32_t *fixed_integer);
static int vterm_get_list(vterm_env_t *vtenv, vterm_t *vtp, uint32_t *list_length, vterm_t *elements[], vterm_t *tail);
static int vterm_get_pid(vterm_env_t *vtenv, vterm_t *vtp, ERL_NIF_TERM *pidp);
static int vterm_get_port(vterm_env_t *vtenv, vterm_t *vtp, ERL_NIF_TERM *portp);
static int vterm_get_proc(vterm_env_t *vtenv, vterm_t *vtp, ERL_NIF_TERM *procp);
static int vterm_get_proper_list(vterm_env_t *vtenv, vterm_t *vtp, uint32_t *list_length, vterm_t *elements[]);
static int vterm_get_proper_list_length(vterm_env_t *vtenv, vterm_t *vtp, uint32_t *list_length);
static int vterm_get_reference(vterm_env_t *vtenv, vterm_t *vtp, ERL_NIF_TERM *refp);
static int vterm_get_tuple(vterm_env_t *vtenv, vterm_t *vtp, uint32_t *arity, vterm_t *elements[]);
static int vterm_maybe_decode_lazy_term(vterm_env_t *vtenv, vterm_t *vtp);
static int vterm_read_tag(vterm_env_t *vtenv, const vterm_t *vtp, vterm_tag_t *tagp);

/* Inline Function Definitions */

inline int
vterm_is_lazy_term(vterm_env_t *vtenv, const vterm_t *vtp)
{
    (void)vtenv;
    return (vtp != NULL && *vtp != NULL && (*vtp)->tag == VTERM_TAG_LAZY_TERM);
}

inline int
vterm_is_nif_term(vterm_env_t *vtenv, const vterm_t *vtp)
{
    (void)vtenv;
    return (vtp != NULL && *vtp != NULL && (*vtp)->tag == VTERM_TAG_NIF_TERM);
}

inline int
vterm_is_atom_cache_ref_resolved(vterm_env_t *vtenv, const vterm_t *vtp)
{
    (void)vtenv;
    return (vtp != NULL && *vtp != NULL && (*vtp)->tag == VTERM_TAG_ATOM_CACHE_REF_RESOLVED);
}

inline int
vterm_is_the_non_value(vterm_env_t *vtenv, const vterm_t *vtp)
{
    (void)vtenv;
    return (vtp != NULL && *vtp != NULL && (*vtp)->tag == VTERM_TAG_THE_NON_VALUE);
}

inline int
vterm_is_small_integer_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_SMALL_INTEGER_EXT);
}

inline int
vterm_is_integer_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_INTEGER_EXT);
}

inline int
vterm_is_float_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_FLOAT_EXT);
}

inline int
vterm_is_atom_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_ATOM_EXT);
}

inline int
vterm_is_small_atom_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_SMALL_ATOM_EXT);
}

inline int
vterm_is_reference_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_REFERENCE_EXT);
}

inline int
vterm_is_new_reference_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_NEW_REFERENCE_EXT);
}

inline int
vterm_is_newer_reference_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_NEWER_REFERENCE_EXT);
}

inline int
vterm_is_port_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_PORT_EXT);
}

inline int
vterm_is_new_port_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_NEW_PORT_EXT);
}

inline int
vterm_is_new_float_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_NEW_FLOAT_EXT);
}

inline int
vterm_is_pid_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_PID_EXT);
}

inline int
vterm_is_new_pid_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_NEW_PID_EXT);
}

inline int
vterm_is_small_tuple_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_SMALL_TUPLE_EXT);
}

inline int
vterm_is_large_tuple_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_LARGE_TUPLE_EXT);
}

inline int
vterm_is_nil_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_NIL_EXT);
}

inline int
vterm_is_string_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_STRING_EXT);
}

inline int
vterm_is_list_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_LIST_EXT);
}

inline int
vterm_is_binary_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_BINARY_EXT);
}

inline int
vterm_is_bit_binary_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_BIT_BINARY_EXT);
}

inline int
vterm_is_small_big_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_SMALL_BIG_EXT);
}

inline int
vterm_is_large_big_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_LARGE_BIG_EXT);
}

inline int
vterm_is_new_fun_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_NEW_FUN_EXT);
}

inline int
vterm_is_export_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_EXPORT_EXT);
}

inline int
vterm_is_map_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_MAP_EXT);
}

inline int
vterm_is_atom_utf8_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_ATOM_UTF8_EXT);
}

inline int
vterm_is_small_atom_utf8_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_SMALL_ATOM_UTF8_EXT);
}

inline int
vterm_is_v4_port_ext(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_V4_PORT_EXT);
}

inline int
vterm_is_atom_cache_ref(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    return (vterm_read_tag(vtenv, vtp, &tag) && tag == VTERM_TAG_ATOM_CACHE_REF);
}

inline int
vterm_is_atom(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    if (!vterm_read_tag(vtenv, vtp, &tag)) {
        return 0;
    }
    switch (tag) {
    case VTERM_TAG_NIF_TERM:
        return (enif_is_atom(vtenv->nif_env, (*vtp)->data.nif_term.term));
    case VTERM_TAG_ATOM_EXT:
        [[fallthrough]];
    case VTERM_TAG_SMALL_ATOM_EXT:
        [[fallthrough]];
    case VTERM_TAG_ATOM_UTF8_EXT:
        [[fallthrough]];
    case VTERM_TAG_SMALL_ATOM_UTF8_EXT:
        [[fallthrough]];
    case VTERM_TAG_ATOM_CACHE_REF:
        [[fallthrough]];
    case VTERM_TAG_ATOM_CACHE_REF_RESOLVED:
        return 1;
    default:
        return 0;
    }
}
inline int
vterm_is_fixed_integer(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    if (!vterm_read_tag(vtenv, vtp, &tag)) {
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
vterm_is_integer(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    if (!vterm_read_tag(vtenv, vtp, &tag)) {
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
vterm_is_list(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    if (!vterm_read_tag(vtenv, vtp, &tag)) {
        return 0;
    }
    switch (tag) {
    case VTERM_TAG_NIF_TERM:
        return (enif_is_empty_list(vtenv->nif_env, (*vtp)->data.nif_term.term) ||
                enif_is_list(vtenv->nif_env, (*vtp)->data.nif_term.term));
    case VTERM_TAG_NIL_EXT:
        [[fallthrough]];
    case VTERM_TAG_LIST_EXT:
        return 1;
    default:
        return 0;
    }
}
inline int
vterm_is_pid(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    if (!vterm_read_tag(vtenv, vtp, &tag)) {
        return 0;
    }
    switch (tag) {
    case VTERM_TAG_NIF_TERM:
        return (enif_is_pid(vtenv->nif_env, (*vtp)->data.nif_term.term));
    case VTERM_TAG_PID_EXT:
        [[fallthrough]];
    case VTERM_TAG_NEW_PID_EXT:
        return 1;
    default:
        return 0;
    }
}
inline int
vterm_is_port(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    if (!vterm_read_tag(vtenv, vtp, &tag)) {
        return 0;
    }
    switch (tag) {
    case VTERM_TAG_NIF_TERM:
        return (enif_is_port(vtenv->nif_env, (*vtp)->data.nif_term.term));
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
vterm_is_reference(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    if (!vterm_read_tag(vtenv, vtp, &tag)) {
        return 0;
    }
    switch (tag) {
    case VTERM_TAG_NIF_TERM:
        return (enif_is_ref(vtenv->nif_env, (*vtp)->data.nif_term.term));
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
vterm_is_tuple(vterm_env_t *vtenv, const vterm_t *vtp)
{
    vterm_tag_t tag;
    if (!vterm_read_tag(vtenv, vtp, &tag)) {
        return 0;
    }
    switch (tag) {
    case VTERM_TAG_NIF_TERM:
        return (enif_is_tuple(vtenv->nif_env, (*vtp)->data.nif_term.term));
    case VTERM_TAG_SMALL_TUPLE_EXT:
        [[fallthrough]];
    case VTERM_TAG_LARGE_TUPLE_EXT:
        return 1;
    default:
        return 0;
    }
}

inline int
vterm_is_likely_gen_reply_tag(vterm_env_t *vtenv, vterm_t *vtp)
{
    ERL_NIF_TERM atom_term;
    if (*vtp == NULL) {
        return 0;
    }
    if (vterm_is_reference(vtenv, vtp)) {
        // MATCH: reference()
        return 1;
    }
    if (!vterm_maybe_decode_lazy_term(vtenv, vtp)) {
        return 0;
    }
    switch ((*vtp)->tag) {
    case VTERM_TAG_NIL_EXT:
        return 0;
    case VTERM_TAG_LIST_EXT: {
        vterm_data_list_ext_t *dp = &(*vtp)->data.list_ext;
        if (dp->len == 1 && vterm_is_atom(vtenv, &dp->elements[0]) && vterm_is_reference(vtenv, &dp->tail) &&
            vterm_get_atom(vtenv, &dp->elements[0], &atom_term) && atom_term == ATOM(alias)) {
            // MATCH: [alias | reference()]
            return 1;
        } else if (dp->len >= 1 && vterm_is_list_ext(vtenv, &dp->elements[0]) &&
                   vterm_maybe_decode_lazy_term(vtenv, &dp->elements[0])) {
            dp = &dp->elements[0]->data.list_ext;
            if (dp->len == 1 && vterm_is_atom(vtenv, &dp->elements[0]) && vterm_is_reference(vtenv, &dp->tail) &&
                vterm_get_atom(vtenv, &dp->elements[0], &atom_term) && atom_term == ATOM(alias)) {
                // MATCH: [[alias | reference()] | dynamic()]
                return 1;
            }
        }
        return 0;
    }
    default:
        return 0;
    }
}

inline int
vterm_is_proper_list(vterm_env_t *vtenv, vterm_t *vtp)
{
    if (!vterm_is_list(vtenv, vtp)) {
        return 0;
    }
    if (!vterm_maybe_decode_lazy_term(vtenv, vtp)) {
        return 0;
    }
    switch ((*vtp)->tag) {
    case VTERM_TAG_NIF_TERM: {
        ERL_NIF_TERM nif_term = (*vtp)->data.nif_term.term;
        unsigned len;
        return (enif_is_empty_list(vtenv->nif_env, nif_term) ||
                (enif_is_list(vtenv->nif_env, nif_term) && enif_get_list_length(vtenv->nif_env, nif_term, &len)));
    }
    case VTERM_TAG_NIL_EXT:
        return 1;
    case VTERM_TAG_LIST_EXT:
        return vterm_is_nil_ext(vtenv, &(*vtp)->data.list_ext.tail);
    default:
        return 0;
    }
}

inline int
vterm_get_atom(vterm_env_t *vtenv, vterm_t *vtp, ERL_NIF_TERM *atomp)
{
    if (!vterm_is_atom(vtenv, vtp)) {
        return 0;
    }
    if (!vterm_maybe_decode_lazy_term(vtenv, vtp)) {
        return 0;
    }
    switch ((*vtp)->tag) {
    case VTERM_TAG_NIF_TERM: {
        ERL_NIF_TERM nif_term = (*vtp)->data.nif_term.term;
        if (!enif_is_atom(vtenv->nif_env, nif_term)) {
            return 0;
        }
        *atomp = nif_term;
        return 1;
    }
    case VTERM_TAG_ATOM_CACHE_REF_RESOLVED:
        *atomp = (*vtp)->data.atom_cache_ref_resolved.term;
        return 1;
    case VTERM_TAG_ATOM_EXT:
        return vterm_env_resolve(vtenv, vtp, atomp);
    case VTERM_TAG_SMALL_ATOM_EXT:
        return vterm_env_resolve(vtenv, vtp, atomp);
    case VTERM_TAG_ATOM_UTF8_EXT:
        return vterm_env_resolve(vtenv, vtp, atomp);
    case VTERM_TAG_SMALL_ATOM_UTF8_EXT:
        return vterm_env_resolve(vtenv, vtp, atomp);
    default:
        return 0;
    }
}

inline int
vterm_get_atom_text(vterm_env_t *vtenv, vterm_t *vtp, ErlNifCharEncoding *encodingp, const uint8_t **namep, size_t *lenp)
{
    if (!vterm_is_atom(vtenv, vtp)) {
        return 0;
    }
    if (!vterm_maybe_decode_lazy_term(vtenv, vtp)) {
        return 0;
    }
    switch ((*vtp)->tag) {
    case VTERM_TAG_ATOM_EXT: {
        vterm_data_atom_ext_t *dp = &(*vtp)->data.atom_ext;
        if (encodingp != NULL) {
            *encodingp = ERL_NIF_LATIN1;
        }
        if (namep != NULL) {
            *namep = dp->name;
        }
        if (lenp != NULL) {
            *lenp = (size_t)dp->len;
        }
        return 1;
    }
    case VTERM_TAG_SMALL_ATOM_EXT: {
        vterm_data_small_atom_ext_t *dp = &(*vtp)->data.small_atom_ext;
        if (encodingp != NULL) {
            *encodingp = ERL_NIF_LATIN1;
        }
        if (namep != NULL) {
            *namep = dp->name;
        }
        if (lenp != NULL) {
            *lenp = (size_t)dp->len;
        }
        return 1;
    }
    case VTERM_TAG_ATOM_UTF8_EXT: {
        vterm_data_atom_utf8_ext_t *dp = &(*vtp)->data.atom_utf8_ext;
        if (encodingp != NULL) {
            *encodingp = ERL_NIF_UTF8;
        }
        if (namep != NULL) {
            *namep = dp->name;
        }
        if (lenp != NULL) {
            *lenp = (size_t)dp->len;
        }
        return 1;
    }
    case VTERM_TAG_SMALL_ATOM_UTF8_EXT: {
        vterm_data_small_atom_utf8_ext_t *dp = &(*vtp)->data.small_atom_utf8_ext;
        if (encodingp != NULL) {
            *encodingp = ERL_NIF_UTF8;
        }
        if (namep != NULL) {
            *namep = dp->name;
        }
        if (lenp != NULL) {
            *lenp = (size_t)dp->len;
        }
        return 1;
    }
    default:
        return 0;
    }
}

inline int
vterm_get_fixed_integer(vterm_env_t *vtenv, vterm_t *vtp, int32_t *fixed_integer)
{
    if (!vterm_is_fixed_integer(vtenv, vtp)) {
        return 0;
    }
    if (!vterm_maybe_decode_lazy_term(vtenv, vtp)) {
        return 0;
    }
    switch ((*vtp)->tag) {
    case VTERM_TAG_SMALL_INTEGER_EXT:
        *fixed_integer = (int32_t)((*vtp)->data.small_integer_ext.value);
        return 1;
    case VTERM_TAG_INTEGER_EXT:
        *fixed_integer = (int32_t)((*vtp)->data.integer_ext.value);
        return 1;
    default:
        return 0;
    }
}

inline int
vterm_get_list(vterm_env_t *vtenv, vterm_t *vtp, uint32_t *list_length, vterm_t *elements[], vterm_t *tail)
{
    if (*vtp == NULL) {
        return 0;
    }
    if (!vterm_maybe_decode_lazy_term(vtenv, vtp)) {
        return 0;
    }
    switch ((*vtp)->tag) {
    case VTERM_TAG_NIL_EXT: {
        if (list_length != NULL) {
            *list_length = 0;
        }
        if (elements != NULL) {
            *elements = NULL;
        }
        if (tail != NULL) {
            *tail = NULL;
        }
        return 1;
    }
    case VTERM_TAG_LIST_EXT: {
        vterm_data_list_ext_t *dp = &(*vtp)->data.list_ext;
        if (list_length != NULL) {
            *list_length = dp->len;
        }
        if (elements != NULL) {
            *elements = dp->elements;
        }
        if (tail != NULL) {
            *tail = dp->tail;
        }
        return 1;
    }
    default:
        return 0;
    }
}

inline int
vterm_get_pid(vterm_env_t *vtenv, vterm_t *vtp, ERL_NIF_TERM *pidp)
{
    if (*vtp == NULL) {
        return 0;
    }
    if (!vterm_maybe_decode_lazy_term(vtenv, vtp)) {
        return 0;
    }
    switch ((*vtp)->tag) {
    case VTERM_TAG_NIF_TERM: {
        ERL_NIF_TERM nif_term = (*vtp)->data.nif_term.term;
        if (!enif_is_pid(vtenv->nif_env, nif_term)) {
            return 0;
        }
        *pidp = nif_term;
        return 1;
    }
    case VTERM_TAG_PID_EXT:
        return vterm_env_resolve(vtenv, vtp, pidp);
    case VTERM_TAG_NEW_PID_EXT:
        return vterm_env_resolve(vtenv, vtp, pidp);
    default:
        return 0;
    }
}

inline int
vterm_get_port(vterm_env_t *vtenv, vterm_t *vtp, ERL_NIF_TERM *portp)
{
    if (*vtp == NULL) {
        return 0;
    }
    if (!vterm_maybe_decode_lazy_term(vtenv, vtp)) {
        return 0;
    }
    switch ((*vtp)->tag) {
    case VTERM_TAG_NIF_TERM: {
        ERL_NIF_TERM nif_term = (*vtp)->data.nif_term.term;
        if (!enif_is_port(vtenv->nif_env, nif_term)) {
            return 0;
        }
        *portp = nif_term;
        return 1;
    }
    case VTERM_TAG_PORT_EXT:
        return vterm_env_resolve(vtenv, vtp, portp);
    case VTERM_TAG_NEW_PORT_EXT:
        return vterm_env_resolve(vtenv, vtp, portp);
    case VTERM_TAG_V4_PORT_EXT:
        return vterm_env_resolve(vtenv, vtp, portp);
    default:
        return 0;
    }
}

inline int
vterm_get_proc(vterm_env_t *vtenv, vterm_t *vtp, ERL_NIF_TERM *procp)
{
    if (*vtp == NULL) {
        return 0;
    }
    if (!vterm_maybe_decode_lazy_term(vtenv, vtp)) {
        return 0;
    }
    if (vterm_is_atom(vtenv, vtp)) {
        return vterm_get_atom(vtenv, vtp, procp);
    } else if (vterm_is_pid(vtenv, vtp)) {
        return vterm_get_pid(vtenv, vtp, procp);
    }
    return 0;
}

inline int
vterm_get_proper_list(vterm_env_t *vtenv, vterm_t *vtp, uint32_t *list_length, vterm_t *elements[])
{
    if (!vterm_is_proper_list(vtenv, vtp)) {
        return 0;
    }
    return vterm_get_list(vtenv, vtp, list_length, elements, NULL);
}

inline int
vterm_get_proper_list_length(vterm_env_t *vtenv, vterm_t *vtp, uint32_t *list_length)
{
    return vterm_get_proper_list(vtenv, vtp, list_length, NULL);
}

inline int
vterm_get_reference(vterm_env_t *vtenv, vterm_t *vtp, ERL_NIF_TERM *refp)
{
    if (!vterm_is_reference(vtenv, vtp)) {
        return 0;
    }
    if (!vterm_maybe_decode_lazy_term(vtenv, vtp)) {
        return 0;
    }
    switch ((*vtp)->tag) {
    case VTERM_TAG_NIF_TERM: {
        ERL_NIF_TERM nif_term = (*vtp)->data.nif_term.term;
        if (!enif_is_ref(vtenv->nif_env, nif_term)) {
            return 0;
        }
        *refp = nif_term;
        return 1;
    }
    case VTERM_TAG_REFERENCE_EXT:
        return vterm_env_resolve(vtenv, vtp, refp);
    case VTERM_TAG_NEW_REFERENCE_EXT:
        return vterm_env_resolve(vtenv, vtp, refp);
    case VTERM_TAG_NEWER_REFERENCE_EXT:
        return vterm_env_resolve(vtenv, vtp, refp);
    default:
        return 0;
    }
}

inline int
vterm_get_tuple(vterm_env_t *vtenv, vterm_t *vtp, uint32_t *arity, vterm_t *elements[])
{
    if (!vterm_is_tuple(vtenv, vtp)) {
        return 0;
    }
    if (!vterm_maybe_decode_lazy_term(vtenv, vtp)) {
        return 0;
    }
    switch ((*vtp)->tag) {
    case VTERM_TAG_SMALL_TUPLE_EXT: {
        vterm_data_small_tuple_ext_t *dp = &(*vtp)->data.small_tuple_ext;
        if (arity != NULL) {
            *arity = (uint32_t)(dp->arity);
        }
        if (elements != NULL) {
            *elements = dp->elements;
        }
        return 1;
    }
    case VTERM_TAG_LARGE_TUPLE_EXT: {
        vterm_data_large_tuple_ext_t *dp = &(*vtp)->data.large_tuple_ext;
        if (arity != NULL) {
            *arity = (uint32_t)(dp->arity);
        }
        if (elements != NULL) {
            *elements = dp->elements;
        }
        return 1;
    }
    default:
        return 0;
    }
}

inline int
vterm_maybe_decode_lazy_term(vterm_env_t *vtenv, vterm_t *vtp)
{
    if (vtp == NULL || *vtp == NULL) {
        return 0;
    }
    if (!vterm_is_lazy_term(vtenv, vtp)) {
        return 1;
    }
    if (!vterm_decode_lazy_term(vtenv, vtp, NULL)) {
        return 0;
    }
    return 1;
}

inline int
vterm_read_tag(vterm_env_t *vtenv, const vterm_t *vtp, vterm_tag_t *tagp)
{
    vterm_tag_t tag;
    (void)vtenv;
    if (vtp == NULL || *vtp == NULL) {
        return 0;
    }
    tag = (*vtp)->tag;
    if (tag == VTERM_TAG_LAZY_TERM) {
        tag = (vterm_tag_t)((*vtp)->data.lazy_term.slice.head[0]);
    }
    *tagp = tag;
    return 1;
}

#ifdef __cplusplus
}
#endif

#endif
