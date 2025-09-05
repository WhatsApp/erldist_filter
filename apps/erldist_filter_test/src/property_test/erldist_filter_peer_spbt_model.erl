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
-module(erldist_filter_peer_spbt_model).
-compile(warn_missing_spec_all).
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
    p2p/1
]).
%% Internal API
-export([
    unwrap/1
]).

%% Types
-type p2p() :: pid().
-type t() :: #{
    '__type__' := ?MODULE,
    p2p := nil | p2p()
}.

-export_type([
    p2p/0,
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
        p2p => nil
    }.

-spec initial_state([{atom(), dynamic()}] | #{atom() => dynamic()}) -> Model when
    Model :: t().
initial_state(List) when is_list(List) ->
    initial_state(maps:from_list(List));
initial_state(Map) when is_map(Map) ->
    maps:fold(fun maps:update/3, initial_state(), Map).

-spec categorize_call(Model, Func, Args) -> {ok, dynamic | symbolic} | error when
    Model :: t(),
    Func :: atom(),
    Args :: [dynamic()].
categorize_call(#{'__type__' := ?MODULE, p2p := OldP2P}, open_p2p, [_Label]) ->
    case OldP2P of
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
    Result :: {ok, term()} | {var, term()} | {call, module(), atom(), [term()]},
    ExpectedResult :: term().
dynamic_call(State0 = #{'__type__' := ?MODULE, p2p := nil}, open_p2p, [_Label], Result) ->
    P2P = unwrap(Result),
    State1 = State0#{
        p2p := P2P
    },
    {State1, {ok, P2P}}.

-spec symbolic_call(Model, Func, Args) -> {Model, ExpectedResult} when
    Model :: t(),
    Func :: atom(),
    Args :: [term()],
    ExpectedResult :: term().
symbolic_call(State0 = #{'__type__' := ?MODULE}, noop, []) ->
    {State0, ok};
symbolic_call(State0 = #{'__type__' := ?MODULE, p2p := P2P}, Func, Args) when P2P =/= nil ->
    case {Func, Args} of
        {ping, [P2P]} ->
            {State0, pong};
        {alias_priority_send, [P2P, _Term]} ->
            {State0, pong};
        {alias_send, [P2P, _Term]} ->
            {State0, pong};
        {exit2_priority_signal, [P2P, _Term]} ->
            {State0, pong};
        {exit2_signal, [P2P, _Term]} ->
            {State0, pong};
        {reg_send, [P2P, _RegName, _Term]} ->
            {State0, pong};
        {send_sender, [P2P, _Term]} ->
            {State0, pong}
    end.

%%%=============================================================================
%%% Model API functions
%%%=============================================================================

-spec p2p(Model) -> {ok, P2P} | error when
    Model :: t(),
    P2P :: p2p().
p2p(#{'__type__' := ?MODULE, p2p := nil}) ->
    error;
p2p(#{'__type__' := ?MODULE, p2p := P2P}) ->
    {ok, P2P}.

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
