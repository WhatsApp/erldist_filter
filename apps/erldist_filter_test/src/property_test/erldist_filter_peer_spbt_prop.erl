%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% Created :  22 Sep 2022 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
-module(erldist_filter_peer_spbt_prop).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(warn_missing_spec_all).

-include_lib("erldist_filter_test/include/proper_erldist_filter_test.hrl").

%% Properties
-export([
    prop_serial_statem/1,
    prop_parallel_statem/1
]).

%% Macros
-define(MODEL, erldist_filter_peer_spbt_model).
-define(STATEM, erldist_filter_peer_spbt_statem).

%%%=============================================================================
%%% Properties
%%%=============================================================================

%% The peers are reused across PropEr trials. Each trial starts connected and must leave the connection intact.

-spec prop_serial_statem(ct_suite:ct_config()) -> proper:test().
prop_serial_statem(Config) ->
    P2P = find_p2p(Config),
    ?FORALL(
        Commands,
        commands(?STATEM, ?MODEL:initial_state(#{p2p => P2P})),
        begin
            ok = erldist_filter_peer_spbt_shim:ensure_connected(P2P),
            RunResult = {_History, _State, _Result} = run_commands(?STATEM, Commands),
            PresentResult = erldist_filter_proper:present_result(?MODULE, Commands, RunResult, Config),
            case erldist_filter_peer_spbt_shim:is_connected(P2P) of
                true -> PresentResult;
                false -> false
            end
        end
    ).

-spec prop_parallel_statem(ct_suite:ct_config()) -> proper:test().
prop_parallel_statem(Config) ->
    P2P = find_p2p(Config),
    ?FORALL(
        Commands,
        parallel_commands(?STATEM, ?MODEL:initial_state(#{p2p => P2P})),
        begin
            ok = erldist_filter_peer_spbt_shim:ensure_connected(P2P),
            RunResult = {_History, _State, _Result} = run_parallel_commands(?STATEM, Commands),
            PresentResult = erldist_filter_proper:present_result(?MODULE, Commands, RunResult, Config),
            case erldist_filter_peer_spbt_shim:is_connected(P2P) of
                true -> PresentResult;
                false -> false
            end
        end
    ).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-spec find_p2p(ct_suite:ct_config()) -> erldist_filter_peer_spbt_shim:p2p().
find_p2p(Config) ->
    case lists:keyfind(p2p, 1, Config) of
        {p2p, P2P} when is_pid(P2P) -> P2P
    end.
