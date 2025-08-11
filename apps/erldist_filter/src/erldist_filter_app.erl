%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% Created :  10 Aug 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
-module(erldist_filter_app).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(application).

%% application callbacks
-export([
    start/2,
    stop/1
]).

%%%=============================================================================
%%% application callbacks
%%%=============================================================================

-spec start(StartType, StartArgs) -> {ok, Pid} | {ok, Pid, State} | {error, Reason} when
    StartType :: application:start_type(),
    StartArgs :: term(),
    Pid :: pid(),
    State :: term(),
    Reason :: term().
start(_StartType, _StartArgs) ->
    {ok, SupPid} = erldist_filter_sup:start_link(),
    {ok, SupPid}.

-spec stop(State) -> Ignored when
    State :: term(),
    Ignored :: term().
stop(_State) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
