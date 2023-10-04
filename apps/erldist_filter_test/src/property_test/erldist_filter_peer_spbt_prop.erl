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
%%% Created :  22 Sep 2022 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(erldist_filter_peer_spbt_prop).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(warn_missing_spec_all).

-include_lib("proper/include/proper.hrl").

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

-spec prop_serial_statem(ct_suite:ct_config()) -> proper:test().
prop_serial_statem(Config) ->
    UPeer = test_server:lookup_config(upeer, Config),
    VPeer = test_server:lookup_config(vpeer, Config),
    ?FORALL(
        Commands,
        commands(?STATEM, ?MODEL:initial_state(#{upeer => UPeer, vpeer => VPeer})),
        begin
            RunResult = {_History, _State, _Result} = run_commands(?STATEM, Commands),
            ct_property_test:present_result(?MODULE, Commands, RunResult, Config)
        end
    ).

-spec prop_parallel_statem(ct_suite:ct_config()) -> proper:test().
prop_parallel_statem(Config) ->
    UPeer = test_server:lookup_config(upeer, Config),
    VPeer = test_server:lookup_config(vpeer, Config),
    ?FORALL(
        Commands,
        parallel_commands(?STATEM, ?MODEL:initial_state(#{upeer => UPeer, vpeer => VPeer})),
        begin
            RunResult = {_History, _State, _Result} = run_parallel_commands(?STATEM, Commands),
            ct_property_test:present_result(?MODULE, Commands, RunResult, Config)
        end
    ).
