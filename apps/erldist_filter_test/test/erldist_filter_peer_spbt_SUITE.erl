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
%%% erldist_filter functionality.
%%%
%%% These tests are broken down into several modules:
%%%
%%% 1. **Model:** deterministic data structure used to check the running system.
%%% 2. **StateM:** generator state machine that generates commands and state.
%%% 3. **Property-Based Test:** the actual test involving `?FORALL(...)'.
%%% 4. **Shim:** interface to commands that interact with the system.
%%% 5. **System:** `peer` nodes themselves (supervised by `erldist_filter_peer_spbt_sup').
%%%
%%% @see erldist_filter_peer_spbt_model
%%% @see erldist_filter_peer_spbt_statem
%%% @see erldist_filter_peer_spbt_prop
%%% @see erldist_filter_peer_spbt_shim
%%% @see erldist_filter_peer_spbt_sup
%%%
%%% @end
%%% Created :  22 Sep 2022 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
-module(erldist_filter_peer_spbt_SUITE).
-typing([eqwalizer]).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("stdlib/include/assert.hrl").

-behaviour(ct_suite).

%% ct_suite callbacks
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
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
        {group, stateful_property_based_tests}
    ].

-spec groups() -> erldist_filter_test:groups().
groups() ->
    [
        {stateful_property_based_tests, [shuffle], [
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
init_per_group(_Group, Config) ->
    Config.

-spec end_per_group(GroupName :: ct_suite:ct_groupname(), Config :: ct_suite:ct_config()) ->
    erldist_filter_test:end_per_group().
end_per_group(_Group, _Config) ->
    ok.

-spec init_per_testcase(TestCase :: ct_suite:ct_testname(), Config :: ct_suite:ct_config()) ->
    erldist_filter_test:init_per_testcase().
init_per_testcase(prop_serial_statem, Config) ->
    P2P = erldist_filter_test_p2p:open(<<"edf-serial-statem">>),
    [{p2p, P2P} | Config];
init_per_testcase(prop_parallel_statem, Config) ->
    P2P = erldist_filter_test_p2p:open(<<"edf-parallel-statem">>),
    [{p2p, P2P} | Config].

-spec end_per_testcase(TestCase :: ct_suite:ct_testname(), Config :: ct_suite:ct_config()) ->
    erldist_filter_test:end_per_testcase().
end_per_testcase(_TestCase, Config) ->
    case lists:keyfind(p2p, 1, Config) of
        {p2p, P2P} when is_pid(P2P) ->
            ok = erldist_filter_test_p2p:close(P2P),
            ok
    end.

%%%=============================================================================
%%% Test Cases
%%%=============================================================================

prop_serial_statem() ->
    [
        {doc, "Serial Stateful Property-Based Test (SBPT) checking basic erldist_filter functionality"},
        {timetrap, {seconds, 600}}
    ].

prop_serial_statem(Config) ->
    erldist_filter_proper:quickcheck(erldist_filter_peer_spbt_prop:prop_serial_statem(Config), [
        verbose,
        {max_shrinks, 1},
        {numtests, 100}
    ]).

prop_parallel_statem() ->
    [
        {doc, "Parallel Stateful Property-Based Test (SBPT) checking basic erldist_filter functionality"},
        {timetrap, {seconds, 600}}
    ].

prop_parallel_statem(Config) ->
    case is_running_in_sandcastle() of
        true ->
            {skip, "Skipping parallel test for erldist_filter_peer_spbt_SUITE on Sandcastle"};
        false ->
            erldist_filter_proper:quickcheck(erldist_filter_peer_spbt_prop:prop_parallel_statem(Config), [
                verbose,
                {max_shrinks, 1},
                {numtests, 100}
            ])
    end.

%% @private
-spec is_running_in_sandcastle() -> boolean().
is_running_in_sandcastle() ->
    % elp:ignore WA014 (no_os_getenv): used for skipping tests in Sandcastle that always get marked as flaky due to test infra issues
    case os:getenv("SANDCASTLE_DIFF_ID") of
        [$D | _] ->
            true;
        _ ->
            % elp:ignore WA014 (no_os_getenv): used for skipping tests in Sandcastle that always get marked as flaky due to test infra issues
            case os:getenv("SANDCASTLE") of
                false -> false;
                _ -> true
            end
    end.
