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
%%% @doc Serial and Parallel Stateful Property-Based Test (SBPT) checking basic
%%% erldist_filter_nif functionality.
%%%
%%% These tests are broken down into several modules:
%%%
%%% 1. **Model:** deterministic data structure used to check the running system.
%%% 2. **StateM:** generator state machine that generates commands and state.
%%% 3. **Property-Based Test:** the actual test involving `?FORALL(...)'.
%%% 4. **Shim:** interface to commands that interact with the system.
%%%
%%% @see erldist_filter_nif_spbt_model
%%% @see erldist_filter_nif_spbt_statem
%%% @see erldist_filter_nif_spbt_prop
%%% @see erldist_filter_nif_spbt_shim
%%%
%%% @end
%%% Created :  27 Sep 2022 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
-module(erldist_filter_nif_spbt_SUITE).
-typing([eqwalizer]).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("stdlib/include/assert.hrl").

%% ct callbacks
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
%%% ct callbacks
%%%=============================================================================

all() ->
    [
        {group, stateful_property_based_tests}
    ].

groups() ->
    [
        {stateful_property_based_tests, [parallel], [
            prop_serial_statem,
            prop_parallel_statem
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

%%%=============================================================================
%%% Test Cases
%%%=============================================================================

prop_serial_statem() ->
    [
        {doc, "Serial Stateful Property-Based Test (SBPT) checking basic erldist_filter_nif functionality"},
        {timetrap, {seconds, 600}}
    ].

prop_serial_statem(Config) ->
    erldist_filter_proper:quickcheck(erldist_filter_nif_spbt_prop:prop_serial_statem(Config), [
        verbose,
        {max_shrinks, 10},
        {numtests, 100}
    ]).

prop_parallel_statem() ->
    [
        {doc, "Parallel Stateful Property-Based Test (SBPT) checking basic erldist_filter_nif functionality"},
        {timetrap, {seconds, 600}}
    ].

prop_parallel_statem(Config) ->
    erldist_filter_proper:quickcheck(erldist_filter_nif_spbt_prop:prop_parallel_statem(Config), [
        verbose,
        % These settings are only for sanity testing on CI.
        % For load testing, run it overnight locally with higher numbers.
        {max_shrinks, 1},
        {numtests, 1}
    ]).
