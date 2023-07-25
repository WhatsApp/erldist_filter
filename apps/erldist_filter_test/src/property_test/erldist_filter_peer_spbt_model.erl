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
-module(erldist_filter_peer_spbt_model).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

%% StateM API
-export([
    initial_state/0,
    initial_state/1,
    categorize_call/3,
    dynamic_call/4,
    symbolic_call/3
]).
%% Model API
-export([
    upeer/1,
    vpeer/1
]).
%% Internal API
-export([
    unwrap/1
]).

%% Types
-type upeer() :: {node(), pid()}.
-type vpeer() :: {node(), pid()}.
-type t() :: #{
    '__type__' := ?MODULE,
    upeer := nil | upeer(),
    vpeer := nil | vpeer()
}.

-export_type([
    upeer/0,
    vpeer/0,
    t/0
]).

%%%=============================================================================
%%% StateM API functions
%%%=============================================================================

-spec initial_state() -> Model when
    Model :: t().
initial_state() ->
    #{
        '__type__' => ?MODULE,
        upeer => nil,
        vpeer => nil
    }.

-spec initial_state([{atom(), term()}] | #{atom() => term()}) -> Model when
    Model :: t().
initial_state(List) when is_list(List) ->
    initial_state(maps:from_list(List));
initial_state(Map) when is_map(Map) ->
    maps:fold(fun maps:update/3, initial_state(), Map).

-spec categorize_call(Model, Func, Args) -> {ok, dynamic | symbolic} | error when
    Model :: t(),
    Func :: atom(),
    Args :: [term()].
categorize_call(#{'__type__' := ?MODULE, upeer := OldUPeer}, start_upeer, [_UPeer]) ->
    case OldUPeer of
        nil ->
            {ok, dynamic};
        _ ->
            error
    end;
categorize_call(#{'__type__' := ?MODULE, vpeer := OldVPeer}, start_vpeer, [_VPeer]) ->
    case OldVPeer of
        nil ->
            {ok, dynamic};
        _ ->
            error
    end;
categorize_call(#{'__type__' := ?MODULE}, _Func, _Args) ->
    {ok, symbolic}.

-spec dynamic_call(Model, Func, Args, Result) -> {Model, ExpectedResult} when
    Model :: t(),
    Func :: atom(),
    Args :: [term()],
    Result :: term(),
    ExpectedResult :: term().
dynamic_call(State0 = #{'__type__' := ?MODULE, upeer := nil}, start_upeer, [_UPeerNode], Result) ->
    UPeer = unwrap(Result),
    State1 = State0#{
        upeer := UPeer
    },
    {State1, {ok, UPeer}};
dynamic_call(State0 = #{'__type__' := ?MODULE, vpeer := nil}, start_vpeer, [_VPeerNode], Result) ->
    VPeer = unwrap(Result),
    State1 = State0#{
        vpeer := VPeer
    },
    {State1, {ok, VPeer}}.

-spec symbolic_call(Model, Func, Args) -> {Model, ExpectedResult} when
    Model :: t(),
    Func :: atom(),
    Args :: [term()],
    ExpectedResult :: term().
symbolic_call(State0 = #{'__type__' := ?MODULE}, noop, []) ->
    {State0, ok};
symbolic_call(State0 = #{'__type__' := ?MODULE, upeer := UPeer, vpeer := VPeer}, Func, Args) when
    UPeer =/= nil andalso VPeer =/= nil
->
    case {Func, Args} of
        {ping, [UPeer, VPeer]} ->
            {State0, pong};
        {alias_send, [UPeer, VPeer, _Term]} ->
            {State0, pong};
        {reg_send, [UPeer, VPeer, _RegName, _Term]} ->
            {State0, pong};
        {send_sender, [UPeer, VPeer, _Term]} ->
            {State0, pong}
    end.

%%%=============================================================================
%%% Model API functions
%%%=============================================================================

-spec upeer(Model) -> {ok, UPeer} | error when
    Model :: t(),
    UPeer :: upeer().
upeer(#{'__type__' := ?MODULE, upeer := nil}) ->
    error;
upeer(#{'__type__' := ?MODULE, upeer := UPeer}) ->
    {ok, UPeer}.

-spec vpeer(Model) -> {ok, VPeer} | error when
    Model :: t(),
    VPeer :: vpeer().
vpeer(#{'__type__' := ?MODULE, vpeer := nil}) ->
    error;
vpeer(#{'__type__' := ?MODULE, vpeer := VPeer}) ->
    {ok, VPeer}.

%%%=============================================================================
%%% Internal API functions
%%%=============================================================================

-spec unwrap
    ({ok, Result}) -> Result when Result :: term();
    (Var) -> {call, ?MODULE, unwrap, [Var]} when Var :: {var, term()};
    (Call) -> {call, ?MODULE, unwrap, [Call]} when Call :: {call, module(), atom(), [term()]}.
unwrap({ok, Result}) ->
    Result;
unwrap(Var = {var, _}) ->
    {call, ?MODULE, unwrap, [Var]};
unwrap(Call = {call, _, _, _}) ->
    {call, ?MODULE, unwrap, [Call]}.
