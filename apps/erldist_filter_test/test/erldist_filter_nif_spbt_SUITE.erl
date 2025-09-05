%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(erldist_filter_nif_spbt_SUITE).
-moduledoc """
Serial and Parallel Stateful Property-Based Test (SBPT) checking basic `erldist_filter_nif` functionality.

These tests are broken down into several modules:

1. **Model:** deterministic data structure used to check the running system.
2. **StateM:** generator state machine that generates commands and state.
3. **Property-Based Test:** the actual test involving `?FORALL(...)`.
4. **Shim:** interface to commands that interact with the system.

- See `erldist_filter_nif_spbt_model`
- See `erldist_filter_nif_spbt_statem`
- See `erldist_filter_nif_spbt_prop`
- See `erldist_filter_nif_spbt_shim`
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2022-09-27", modified => "2025-09-05"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

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
    prop_serial_statem/0,
    prop_serial_statem/1,
    prop_parallel_statem/0,
    prop_parallel_statem/1
]).

%%%=============================================================================
%%% ct_suite callbacks
%%%=============================================================================

-spec all() -> erldist_filter_test:all().
all() ->
    [
        % {group, deep_packet_inspection},
        {group, default}
    ].

-spec groups() -> erldist_filter_test:groups().
groups() ->
    [
        % {deep_packet_inspection, [parallel], [
        %     prop_serial_statem,
        %     prop_parallel_statem
        % ]},
        {default, [parallel], [
            prop_serial_statem,
            prop_parallel_statem
        ]}
    ].

-spec init_per_suite(Config :: ct_suite:ct_config()) -> erldist_filter_test:init_per_suite().
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(erldist_filter_test),
    Config.

-spec end_per_suite(Config :: ct_suite:ct_config()) -> erldist_filter_test:end_per_suite().
end_per_suite(_Config) ->
    ok.

-spec init_per_group(GroupName :: ct_suite:ct_groupname(), Config :: ct_suite:ct_config()) ->
    erldist_filter_test:init_per_group().
init_per_group(deep_packet_inspection, Config) ->
    ConfigMapSet = #{
        compact_fragments => true,
        deep_packet_inspection => true
    },
    ok = config_set(ConfigMapSet),
    [{config_map_set, ConfigMapSet} | Config];
init_per_group(default, Config) ->
    ConfigMapSet = #{},
    ok = config_set_default(),
    [{config_map_set, ConfigMapSet} | Config].

-spec end_per_group(GroupName :: ct_suite:ct_groupname(), Config :: ct_suite:ct_config()) ->
    erldist_filter_test:end_per_group().
end_per_group(_Group, _Config) ->
    ok.

%%%=============================================================================
%%% Test Cases
%%%=============================================================================

-spec prop_serial_statem() -> erldist_filter_test:testcase_info().
prop_serial_statem() ->
    [
        {doc, "Serial Stateful Property-Based Test (SBPT) checking basic erldist_filter_nif functionality"},
        {timetrap, {seconds, 600}}
    ].

-spec prop_serial_statem(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
prop_serial_statem(Config) ->
    erldist_filter_proper:quickcheck(erldist_filter_nif_spbt_prop:prop_serial_statem(Config), [
        verbose,
        {max_shrinks, 10},
        {numtests, 100}
    ]).

-spec prop_parallel_statem() -> erldist_filter_test:testcase_info().
prop_parallel_statem() ->
    [
        {doc, "Parallel Stateful Property-Based Test (SBPT) checking basic erldist_filter_nif functionality"},
        {timetrap, {seconds, 600}}
    ].

-spec prop_parallel_statem(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
prop_parallel_statem(Config) ->
    erldist_filter_proper:quickcheck(erldist_filter_nif_spbt_prop:prop_parallel_statem(Config), [
        verbose,
        % These settings are only for sanity testing on CI.
        % For load testing, run it overnight locally with higher numbers.
        {max_shrinks, 1},
        {numtests, 1}
    ]).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec config_set_default() -> ok.
config_set_default() ->
    config_set(#{}).

%% @private
-spec config_set(ConfigMapSet) -> ok when
    ConfigMapSet :: erldist_filter_nif_types:config_map_set().
config_set(ConfigMapSet1) ->
    DefaultConfigMapSet =
        #{
            ConfigKey =>
                case ConfigVal of
                    _ when is_boolean(ConfigVal) -> false;
                    _ when is_integer(ConfigVal) -> 0
                end
         || ConfigKey := ConfigVal <- erldist_filter_nif:config_get()
        },
    ConfigMapSet2 = maps:merge(DefaultConfigMapSet, ConfigMapSet1),
    erldist_filter_nif:config_set(ConfigMapSet2).
