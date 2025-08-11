%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% @author Andrew Bennett <potatosaladx@meta.com>
%%% @copyright (c) Meta Platforms, Inc. and affiliates.
%%% @doc
%%%
%%% @end
%%% Created :  19 Jun 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%% @oncall whatsapp_clr
-ifndef(VTERM_HRL).

-define(VTERM_HRL, 1).

-record(vterm_the_non_value, {}).

-record(vterm_atom_cache_ref_resolved, {
    index = 0 :: vterm:u8(),
    term = undefined :: atom()
}).

-record(vterm_nif_term, {
    term = #vterm_the_non_value{} :: vterm:t() | dynamic()
}).

-record(vterm_lazy_term, {
    slice = <<>> :: binary()
}).

-record(vterm_small_integer_ext, {
    value = 0 :: vterm:u8()
}).

-record(vterm_integer_ext, {
    value = 0 :: vterm:i32()
}).

-record(vterm_float_ext, {
    float_string = <<"0.00000000000000000000e+00", 0, 0, 0, 0, 0>> :: <<_:248>>
}).

-record(vterm_atom_ext, {
    len = 0 :: vterm:u16(),
    name = <<>> :: binary()
}).

-record(vterm_small_atom_ext, {
    len = 0 :: vterm:u8(),
    name = <<>> :: binary()
}).

-record(vterm_reference_ext, {
    node = #vterm_atom_ext{} :: vterm:atom_t(),
    id = 0 :: vterm:u32(),
    creation = 0 :: vterm:u8()
}).

-record(vterm_new_reference_ext, {
    len = 0 :: vterm:u16(),
    node = #vterm_atom_ext{} :: vterm:atom_t(),
    creation = 0 :: vterm:u8(),
    ids = [] :: [vterm:u32()]
}).

-record(vterm_newer_reference_ext, {
    len = 0 :: vterm:u16(),
    node = #vterm_atom_ext{} :: vterm:atom_t(),
    creation = 0 :: vterm:u32(),
    ids = [] :: [vterm:u32()]
}).

-record(vterm_port_ext, {
    node = #vterm_atom_ext{} :: vterm:atom_t(),
    id = 0 :: vterm:u32(),
    creation = 0 :: vterm:u8()
}).

-record(vterm_new_port_ext, {
    node = #vterm_atom_ext{} :: vterm:atom_t(),
    id = 0 :: vterm:u32(),
    creation = 0 :: vterm:u32()
}).

-record(vterm_new_float_ext, {
    ieee_float = <<0:64>> :: <<_:64>>
}).

-record(vterm_pid_ext, {
    node = #vterm_atom_ext{} :: vterm:atom_t(),
    id = 0 :: vterm:u32(),
    serial = 0 :: vterm:u32(),
    creation = 0 :: vterm:u8()
}).

-record(vterm_new_pid_ext, {
    node = #vterm_atom_ext{} :: vterm:atom_t(),
    id = 0 :: vterm:u32(),
    serial = 0 :: vterm:u32(),
    creation = 0 :: vterm:u32()
}).

-record(vterm_small_tuple_ext, {
    arity = 0 :: vterm:u8(),
    elements = [] :: [vterm:t()]
}).

-record(vterm_large_tuple_ext, {
    arity = 0 :: vterm:u32(),
    elements = [] :: [vterm:t()]
}).

-record(vterm_nil_ext, {}).

-record(vterm_string_ext, {
    len = 0 :: vterm:u16(),
    characters = <<>> :: binary()
}).

-record(vterm_list_ext, {
    len = 0 :: vterm:u32(),
    elements = [] :: [vterm:t()],
    tail = #vterm_nil_ext{} :: vterm:t()
}).

-record(vterm_binary_ext, {
    len = 0 :: vterm:u32(),
    data = <<>> :: binary()
}).

-record(vterm_bit_binary_ext, {
    len = 0 :: vterm:u32(),
    bits = 8 :: 1..8,
    data = <<>> :: binary()
}).

-record(vterm_small_big_ext, {
    n = 0 :: vterm:u8(),
    sign = 0 :: 0..1,
    d = <<>> :: binary()
}).

-record(vterm_large_big_ext, {
    n = 0 :: vterm:u32(),
    sign = 0 :: 0..1,
    d = <<>> :: binary()
}).

-record(vterm_new_fun_ext, {
    size = 0 :: vterm:u32(),
    arity = 0 :: vterm:u8(),
    uniq = <<0:128>> :: <<_:128>>,
    index = 0 :: vterm:u32(),
    num_free = 0 :: vterm:u32(),
    module = #vterm_atom_ext{} :: vterm:atom_t(),
    old_index = #vterm_small_integer_ext{} :: vterm:fixed_integer_t(),
    old_uniq = #vterm_small_integer_ext{} :: vterm:fixed_integer_t(),
    pid = #vterm_pid_ext{} :: vterm:pid_t(),
    free_vars = [] :: [vterm:t()]
}).

-record(vterm_export_ext, {
    module = #vterm_atom_ext{} :: vterm:atom_t(),
    function = #vterm_atom_ext{} :: vterm:atom_t(),
    arity = #vterm_small_integer_ext{} :: vterm:small_integer_t()
}).

-record(vterm_map_ext, {
    arity = 0 :: vterm:u32(),
    pairs = [] :: [{vterm:t(), vterm:t()}]
}).

-record(vterm_atom_utf8_ext, {
    len = 0 :: vterm:u16(),
    name = <<>> :: binary()
}).

-record(vterm_small_atom_utf8_ext, {
    len = 0 :: vterm:u8(),
    name = <<>> :: binary()
}).

-record(vterm_v4_port_ext, {
    node = #vterm_atom_ext{} :: vterm:atom_t(),
    id = 0 :: vterm:u64(),
    creation = 0 :: vterm:u32()
}).

-record(vterm_atom_cache_ref, {
    index = 0 :: vterm:u8()
}).

-define(is_i32(V), (is_integer(V) andalso (V) >= -2147483648 andalso (V) =< 2147483647)).
-define(is_u8(V), (is_integer(V) andalso (V) >= 0 andalso (V) =< 255)).
-define(is_u16(V), (is_integer(V) andalso (V) >= 0 andalso (V) =< 65535)).
-define(is_u32(V), (is_integer(V) andalso (V) >= 0 andalso (V) =< 4294967295)).
-define(is_u64(V), (is_integer(V) andalso (V) >= 0 andalso (V) =< 18446744073709551615)).

-define(is_vterm_atom_t(T),
    ((is_record(T, vterm_nif_term) andalso is_atom(element(2, T))) orelse
        is_record(T, vterm_atom_ext) orelse
        is_record(T, vterm_small_atom_ext) orelse
        is_record(T, vterm_atom_utf8_ext) orelse
        is_record(T, vterm_small_atom_utf8_ext) orelse
        is_record(T, vterm_atom_cache_ref) orelse
        is_record(T, vterm_atom_cache_ref_resolved))
).
-define(is_vterm_binary_t(T),
    ((is_record(T, vterm_nif_term) andalso is_binary(element(2, T))) orelse
        is_record(T, vterm_binary_ext))
).
-define(is_vterm_bitstring_t(T),
    ((is_record(T, vterm_nif_term) andalso is_bitstring(element(2, T))) orelse
        is_record(T, vterm_binary_ext) orelse
        is_record(T, vterm_bit_binary_ext))
).
-define(is_vterm_fixed_integer_t(T),
    ((is_record(T, vterm_nif_term) andalso ?is_i32(element(2, T))) orelse
        is_record(T, vterm_small_integer_ext) orelse
        is_record(T, vterm_integer_ext))
).
-define(is_vterm_float_t(T),
    ((is_record(T, vterm_nif_term) andalso is_float(element(2, T))) orelse
        is_record(T, vterm_float_ext) orelse
        is_record(T, vterm_new_float_ext))
).
-define(is_vterm_integer_t(T),
    ((is_record(T, vterm_nif_term) andalso is_integer(element(2, T))) orelse
        is_record(T, vterm_small_integer_ext) orelse
        is_record(T, vterm_integer_ext) orelse
        is_record(T, vterm_small_big_ext) orelse
        is_record(T, vterm_large_big_ext))
).
-define(is_vterm_lazy_term_t(T),
    (is_record(T, vterm_lazy_term) andalso is_binary(element(2, T)))
).
-define(is_vterm_list_t(T),
    ((is_record(T, vterm_nif_term) andalso is_list(element(2, T))) orelse
        is_record(T, vterm_string_ext) orelse
        is_record(T, vterm_list_ext) orelse
        is_record(T, vterm_nil_ext))
).
-define(is_vterm_map_t(T),
    ((is_record(T, vterm_nif_term) andalso is_map(element(2, T))) orelse
        is_record(T, vterm_map_ext))
).
-define(is_vterm_nif_term_t(T), (is_record(T, vterm_nif_term))).
-define(is_vterm_nil_t(T),
    ((is_record(T, vterm_nif_term) andalso [] =:= element(2, T)) orelse
        is_record(T, vterm_nil_ext))
).
-define(is_vterm_number_t(T),
    ((is_record(T, vterm_nif_term) andalso is_number(element(2, T))) orelse
        ?is_vterm_float_t(T) orelse
        ?is_vterm_integer_t(T))
).
-define(is_vterm_pid_t(T),
    ((is_record(T, vterm_nif_term) andalso is_pid(element(2, T))) orelse
        is_record(T, vterm_pid_ext) orelse
        is_record(T, vterm_new_pid_ext))
).
-define(is_vterm_port_t(T),
    ((is_record(T, vterm_nif_term) andalso is_port(element(2, T))) orelse
        is_record(T, vterm_port_ext) orelse
        is_record(T, vterm_new_port_ext) orelse
        is_record(T, vterm_v4_port_ext))
).
-define(is_vterm_reference_t(T),
    ((is_record(T, vterm_nif_term) andalso is_reference(element(2, T))) orelse
        is_record(T, vterm_reference_ext) orelse
        is_record(T, vterm_new_reference_ext) orelse
        is_record(T, vterm_newer_reference_ext))
).
-define(is_vterm_small_integer_t(T), (is_record(T, vterm_small_integer_ext))).
-define(is_vterm_the_non_value_t(T), (is_record(T, vterm_the_non_value))).
-define(is_vterm_tuple_t(T),
    ((is_record(T, vterm_nif_term) andalso is_tuple(element(2, T))) orelse
        is_record(T, vterm_small_tuple_ext) orelse
        is_record(T, vterm_large_tuple_ext))
).
-define(is_vterm_t(T),
    (is_record(T, vterm_lazy_term) orelse
        is_record(T, vterm_nif_term) orelse
        is_record(T, vterm_atom_cache_ref_resolved) orelse
        is_record(T, vterm_the_non_value) orelse
        is_record(T, vterm_small_integer_ext) orelse
        is_record(T, vterm_integer_ext) orelse
        is_record(T, vterm_float_ext) orelse
        is_record(T, vterm_atom_ext) orelse
        is_record(T, vterm_small_atom_ext) orelse
        is_record(T, vterm_reference_ext) orelse
        is_record(T, vterm_new_reference_ext) orelse
        is_record(T, vterm_newer_reference_ext) orelse
        is_record(T, vterm_port_ext) orelse
        is_record(T, vterm_new_port_ext) orelse
        is_record(T, vterm_new_float_ext) orelse
        is_record(T, vterm_pid_ext) orelse
        is_record(T, vterm_new_pid_ext) orelse
        is_record(T, vterm_small_tuple_ext) orelse
        is_record(T, vterm_large_tuple_ext) orelse
        is_record(T, vterm_nil_ext) orelse
        is_record(T, vterm_string_ext) orelse
        is_record(T, vterm_list_ext) orelse
        is_record(T, vterm_binary_ext) orelse
        is_record(T, vterm_bit_binary_ext) orelse
        is_record(T, vterm_small_big_ext) orelse
        is_record(T, vterm_large_big_ext) orelse
        is_record(T, vterm_new_fun_ext) orelse
        is_record(T, vterm_export_ext) orelse
        is_record(T, vterm_map_ext) orelse
        is_record(T, vterm_atom_utf8_ext) orelse
        is_record(T, vterm_small_atom_utf8_ext) orelse
        is_record(T, vterm_v4_port_ext) orelse
        is_record(T, vterm_atom_cache_ref))
).

-endif.
