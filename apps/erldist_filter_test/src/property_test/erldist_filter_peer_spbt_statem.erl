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
-module(erldist_filter_peer_spbt_statem).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(warn_missing_spec_all).

-behaviour(proper_statem).

-include_lib("common_test/include/ct.hrl").
-include_lib("erldist_filter_test/include/proper_erldist_filter_test.hrl").

%% proper_statem callbacks
-export([
    initial_state/0,
    command/1,
    precondition/2,
    postcondition/3,
    next_state/3
]).
%% Helper API
-export([
    gen_payload/0,
    gen_reg_name/0,
    gen_reg_name/1
]).

%% Types
-type symbolic_state() :: erldist_filter_peer_spbt_model:t().
-type dynamic_state() :: erldist_filter_peer_spbt_model:t().

-export_type([
    symbolic_state/0,
    dynamic_state/0
]).

%% Macros.
-define(MODEL, erldist_filter_peer_spbt_model).
-define(SHIM, erldist_filter_peer_spbt_shim).
-define(is_symbol(X),
    (is_tuple(X) andalso
        ((tuple_size(X) =:= 2 andalso element(1, (X)) =:= var) orelse
            (tuple_size(X) =:= 4 andalso element(1, (X)) =:= call)))
).

%%%=============================================================================
%%% proper_statem callbacks
%%%=============================================================================

-spec initial_state() -> symbolic_state().
initial_state() ->
    ?MODEL:initial_state().

-spec command(symbolic_state()) -> proper_types:type().
command(SymbolicState) ->
    case ?MODEL:p2p(SymbolicState) of
        {ok, P2P} ->
            command(SymbolicState, P2P);
        error ->
            return({call, ?SHIM, open_p2p, [<<>>]})
    end.

-spec command(
    symbolic_state(),
    erldist_filter_peer_spbt_model:p2p()
) -> proper_types:type().
command(_SymbolicState, P2P) ->
    X = exactly(P2P),
    oneof([
        {call, ?SHIM, ping, [X]},
        {call, ?SHIM, alias_priority_send, [X, gen_payload()]},
        {call, ?SHIM, alias_send, [X, gen_payload()]},
        {call, ?SHIM, exit2_priority_signal, [X, gen_payload()]},
        {call, ?SHIM, exit2_signal, [X, gen_payload()]},
        {call, ?SHIM, reg_send, [X, gen_reg_name(P2P), gen_payload()]},
        {call, ?SHIM, send_sender, [X, gen_payload()]}
    ]).

-spec precondition(SymbolicState, SymbolicCall) -> boolean() when
    SymbolicState :: dynamic_state(),
    SymbolicCall :: proper_statem:symbolic_call().
precondition(SymbolicState, SymbolicCall) ->
    case ?MODEL:p2p(SymbolicState) of
        {ok, P2P} ->
            precondition(SymbolicState, SymbolicCall, P2P);
        error ->
            % P2P HAS NOT been started, only VALID command is `open_p2p`
            case SymbolicCall of
                {call, _, noop, _} ->
                    true;
                {call, _, open_p2p, _} ->
                    true;
                {call, _, _, _} ->
                    false
            end
    end.

-spec precondition(
    dynamic_state(),
    proper_statem:symbolic_call(),
    erldist_filter_peer_spbt_model:p2p()
) -> boolean().
precondition(_SymbolicState, SymbolicCall, _P2P) ->
    % P2P HAS been started, only INVALID commands are `open_p2p`
    case SymbolicCall of
        {call, _, open_p2p, _} ->
            false;
        {call, _, _, _} ->
            true
    end.

-spec postcondition(DynamicState, SymbolicCall, Result) -> boolean() when
    DynamicState :: dynamic_state(),
    SymbolicCall :: proper_statem:symbolic_call(),
    Result :: dynamic().
postcondition(DynamicState, SymbolicCall = {call, _, Func, Args}, Result) ->
    case ?MODEL:categorize_call(DynamicState, Func, Args) of
        {ok, dynamic} ->
            {_NewState, _ExpectedResult} = ?MODEL:dynamic_call(DynamicState, Func, Args, Result),
            true;
        {ok, symbolic} ->
            {_NewState, ExpectedResult} = ?MODEL:symbolic_call(DynamicState, Func, Args),
            case Result =:= ExpectedResult of
                true ->
                    true;
                false ->
                    ct:log(
                        error,
                        ?MAX_IMPORTANCE,
                        "Error: postcondition failure in ~w:~s/~w~nState = ~tp~nSymbolicCall = ~tp~nResult = ~tp~nExpectedResult = ~tp~n",
                        [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, DynamicState, SymbolicCall, Result, ExpectedResult]
                    ),
                    false
            end
    end.

-spec next_state(State, Result, SymbolicCall) -> State when
    State :: symbolic_state() | dynamic_state(),
    Result :: dynamic(),
    SymbolicCall :: proper_statem:symbolic_call().
next_state(State, Result, _SymbolicCall = {call, _, Func, Args}) ->
    case ?MODEL:categorize_call(State, Func, Args) of
        {ok, dynamic} ->
            {NewState, _ExpectedResult} = ?MODEL:dynamic_call(State, Func, Args, Result),
            NewState;
        {ok, symbolic} ->
            {NewState, _ExpectedResult} = ?MODEL:symbolic_call(State, Func, Args),
            NewState
    end.

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

-spec gen_payload() -> proper_types:type().
gen_payload() ->
    frequency([
        {1, term()},
        {1, proper_vterm:vterm_simplify_into_term(#{large_binaries => true})}
    ]).

-spec gen_reg_name() -> proper_types:type().
gen_reg_name() ->
    ?SUCHTHAT(
        RegName,
        atom(),
        RegName =/= '' andalso RegName =/= nil andalso RegName =/= undefined andalso
            erlang:whereis(RegName) =:= undefined
    ).

-spec gen_reg_name(P2P) -> Type when
    P2P :: erldist_filter_peer_spbt_model:p2p() | proper_statem:symbolic_var() | proper_statem:symbolic_call(),
    Type :: proper_types:type() | proper_statem:symbolic_call().
gen_reg_name(P2P) when is_pid(P2P) ->
    #{upeer := UPeer, vpeer := VPeer} = erldist_filter_test_p2p:peers(P2P),
    ?SUCHTHAT(
        RegName,
        gen_reg_name(),
        ?SHIM:rpc(UPeer, erlang, whereis, [RegName]) =:= undefined andalso
            ?SHIM:rpc(VPeer, erlang, whereis, [RegName]) =:= undefined
    );
gen_reg_name(P2P) when ?is_symbol(P2P) ->
    {call, ?MODULE, gen_reg_name, [P2P]}.
