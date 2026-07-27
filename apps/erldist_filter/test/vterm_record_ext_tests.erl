%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(vterm_record_ext_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erldist_filter/include/vterm.hrl").

%% RECORD_EXT (tag 67 = $C) was introduced in OTP 29.0. See
%% https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#record_ext
%%
%%   | 1  | 4       | 1     | N1     | N2   | N3          | N4     |
%%   | 67 | #Fields | Flags | Module | Name | Field Names | Values |
%%
%% This encodes the native record -record(r, {a, b}) defined in module `m',
%% with values #r{a = 1, b = 2}, not exported (Flags = 0).
record_ext_internal_bytes() ->
    <<
        %% tag=RECORD_EXT, #Fields=2, Flags=0 (not exported)
        $C,
        2:32,
        0:8,
        %% Module = 'm', Name = 'r' (SMALL_ATOM_UTF8_EXT = $w)
        $w,
        1,
        "m",
        $w,
        1,
        "r",
        %% Field Names = ['a', 'b']
        $w,
        1,
        "a",
        $w,
        1,
        "b",
        %% Values = [1, 2] (SMALL_INTEGER_EXT = $a)
        $a,
        1,
        $a,
        2
    >>.

record_ext_roundtrip_test() ->
    Bin = record_ext_internal_bytes(),
    {ok, VTerm, <<>>} = vterm_decode:internal_binary_to_vterm(Bin),
    ?assertEqual(Bin, vterm_encode:internal_vterm_to_binary(VTerm, [])).

record_ext_decode_structure_test() ->
    Bin = record_ext_internal_bytes(),
    {ok, VTerm, <<>>} = vterm_decode:internal_binary_to_vterm(Bin),
    ?assertMatch(
        #vterm_record_ext{
            num_fields = 2,
            exported = false,
            module = #vterm_small_atom_utf8_ext{name = <<"m">>},
            name = #vterm_small_atom_utf8_ext{name = <<"r">>},
            field_names = [
                #vterm_small_atom_utf8_ext{name = <<"a">>},
                #vterm_small_atom_utf8_ext{name = <<"b">>}
            ],
            values = [
                #vterm_small_integer_ext{value = 1},
                #vterm_small_integer_ext{value = 2}
            ]
        },
        VTerm
    ).

%% Flags least-significant-bit set => exported record.
record_ext_exported_flag_roundtrip_test() ->
    Bin = <<
        $C,
        0:32,
        1:8,
        $w,
        1,
        "m",
        $w,
        1,
        "r"
    >>,
    {ok, VTerm, <<>>} = vterm_decode:internal_binary_to_vterm(Bin),
    ?assertMatch(#vterm_record_ext{num_fields = 0, exported = true}, VTerm),
    ?assertEqual(Bin, vterm_encode:internal_vterm_to_binary(VTerm, [])).

%% A record value that is itself a container (a map) must round-trip.
record_ext_nested_value_roundtrip_test() ->
    Bin = <<
        $C,
        1:32,
        0:8,
        $w,
        1,
        "m",
        $w,
        1,
        "r",
        %% field name 'a'
        $w,
        1,
        "a",
        %% value: #{1 => 2} (MAP_EXT = $t)
        $t,
        1:32,
        $a,
        1,
        $a,
        2
    >>,
    {ok, VTerm, <<>>} = vterm_decode:internal_binary_to_vterm(Bin),
    ?assertMatch(#vterm_record_ext{values = [#vterm_map_ext{}]}, VTerm),
    ?assertEqual(Bin, vterm_encode:internal_vterm_to_binary(VTerm, [])).

%% resolve_atoms/2 must recurse into module, name, and field names of a record,
%% replacing ATOM_CACHE_REF entries with resolved atoms. This is the core path
%% the filter uses to rewrite the atom cache.
record_ext_resolve_atoms_test() ->
    VTerm0 = vterm_record_ext:new(
        1,
        false,
        vterm_atom_cache_ref:new(0),
        vterm_atom_cache_ref:new(1),
        [vterm_atom_cache_ref:new(2)],
        [vterm_small_integer_ext:new(42)]
    ),
    Atoms = {'m', 'r', 'a'},
    VTerm1 = vterm:resolve_atoms(Atoms, VTerm0),
    ?assertMatch(
        #vterm_record_ext{
            module = #vterm_atom_cache_ref_resolved{term = 'm'},
            name = #vterm_atom_cache_ref_resolved{term = 'r'},
            field_names = [#vterm_atom_cache_ref_resolved{term = 'a'}],
            values = [#vterm_small_integer_ext{value = 42}]
        },
        VTerm1
    ).
