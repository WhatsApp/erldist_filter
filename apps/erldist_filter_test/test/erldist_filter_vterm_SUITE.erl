%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(erldist_filter_vterm_SUITE).
-moduledoc """
# Erlang Distribution Filter VTerm Test Suite

This Common Test suite verifies encoding, decoding, and atom resolution for
virtual Erlang terms.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2026-07-27", modified => "2026-07-27"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("stdlib/include/assert.hrl").
-include_lib("erldist_filter/include/vterm.hrl").

-behaviour(ct_suite).

%% ct_suite callbacks
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2
]).

%% Test Cases
-export([
    record_ext_roundtrip/0,
    record_ext_roundtrip/1,
    record_ext_decode_structure/0,
    record_ext_decode_structure/1,
    record_ext_exported_flag_roundtrip/0,
    record_ext_exported_flag_roundtrip/1,
    record_ext_nested_value_roundtrip/0,
    record_ext_nested_value_roundtrip/1,
    record_ext_resolve_atoms/0,
    record_ext_resolve_atoms/1
]).

%%%=============================================================================
%%% ct_suite callbacks
%%%=============================================================================

-spec all() -> erldist_filter_test:all().
all() ->
    [
        {group, vterm}
    ].

-spec groups() -> erldist_filter_test:groups().
groups() ->
    [
        {vterm, [parallel], [
            record_ext_roundtrip,
            record_ext_decode_structure,
            record_ext_exported_flag_roundtrip,
            record_ext_nested_value_roundtrip,
            record_ext_resolve_atoms
        ]}
    ].

-spec init_per_suite(Config :: ct_suite:ct_config()) -> erldist_filter_test:init_per_suite().
init_per_suite(Config) ->
    Config.

-spec end_per_suite(Config :: ct_suite:ct_config()) -> erldist_filter_test:end_per_suite().
end_per_suite(_Config) ->
    ok.

-spec init_per_group(GroupName :: ct_suite:ct_groupname(), Config :: ct_suite:ct_config()) ->
    erldist_filter_test:init_per_group().
init_per_group(_Group, Config) ->
    Config.

-spec end_per_group(GroupName :: ct_suite:ct_groupname(), Config :: ct_suite:ct_config()) ->
    erldist_filter_test:end_per_group().
end_per_group(_Group, _Config) ->
    ok.

%%%=============================================================================
%%% Test Cases
%%%=============================================================================

-spec record_ext_roundtrip() -> erldist_filter_test:testcase_info().
record_ext_roundtrip() ->
    [
        {doc, "Verifies that RECORD_EXT terms round-trip through the VTerm codec"},
        {timetrap, {seconds, 30}}
    ].

-spec record_ext_roundtrip(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
record_ext_roundtrip(_Config) ->
    Bin = record_ext_internal_bytes(),
    {ok, VTerm, <<>>} = vterm_decode:internal_binary_to_vterm(Bin),
    ?assertEqual(Bin, vterm_encode:internal_vterm_to_binary(VTerm, [])).

-spec record_ext_decode_structure() -> erldist_filter_test:testcase_info().
record_ext_decode_structure() ->
    [
        {doc, "Verifies the decoded structure of a RECORD_EXT term"},
        {timetrap, {seconds, 30}}
    ].

-spec record_ext_decode_structure(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
record_ext_decode_structure(_Config) ->
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

-spec record_ext_exported_flag_roundtrip() -> erldist_filter_test:testcase_info().
record_ext_exported_flag_roundtrip() ->
    [
        {doc, "Verifies that the RECORD_EXT exported flag round-trips"},
        {timetrap, {seconds, 30}}
    ].

-spec record_ext_exported_flag_roundtrip(Config :: ct_suite:ct_config()) ->
    erldist_filter_test:testcase().
record_ext_exported_flag_roundtrip(_Config) ->
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

-spec record_ext_nested_value_roundtrip() -> erldist_filter_test:testcase_info().
record_ext_nested_value_roundtrip() ->
    [
        {doc, "Verifies that nested RECORD_EXT values round-trip"},
        {timetrap, {seconds, 30}}
    ].

-spec record_ext_nested_value_roundtrip(Config :: ct_suite:ct_config()) ->
    erldist_filter_test:testcase().
record_ext_nested_value_roundtrip(_Config) ->
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

-spec record_ext_resolve_atoms() -> erldist_filter_test:testcase_info().
record_ext_resolve_atoms() ->
    [
        {doc, "Verifies atom-cache resolution within RECORD_EXT terms"},
        {timetrap, {seconds, 30}}
    ].

-spec record_ext_resolve_atoms(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
record_ext_resolve_atoms(_Config) ->
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

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% RECORD_EXT (tag 67 = $C) was introduced in OTP 29.0. See
%% https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#record_ext
%%
%%   | 1  | 4       | 1     | N1     | N2   | N3          | N4     |
%%   | 67 | #Fields | Flags | Module | Name | Field Names | Values |
%%
%% This encodes the native record -record(r, {a, b}) defined in module `m',
%% with values #r{a = 1, b = 2}, not exported (Flags = 0).
-spec record_ext_internal_bytes() -> binary().
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
