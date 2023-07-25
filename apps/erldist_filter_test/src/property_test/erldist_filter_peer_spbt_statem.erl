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
-module(erldist_filter_peer_spbt_statem).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(nowarn_missing_spec).

-behaviour(proper_statem).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").

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
    gen_reg_name/2,
    gen_upeer_node/0,
    gen_vpeer_node/0,
    gen_vpeer_node/1
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

initial_state() ->
    ?MODEL:initial_state().

command(SymbolicState) ->
    case ?MODEL:upeer(SymbolicState) of
        {ok, UPeer} ->
            command(SymbolicState, UPeer);
        error ->
            UPeerNode = gen_upeer_node(),
            return({call, ?SHIM, start_upeer, [UPeerNode]})
    end.

%% @private
command(SymbolicState, UPeer) ->
    case ?MODEL:vpeer(SymbolicState) of
        {ok, VPeer} ->
            command(SymbolicState, UPeer, VPeer);
        error ->
            VPeerNode = gen_vpeer_node(UPeer),
            return({call, ?SHIM, start_vpeer, [VPeerNode]})
    end.

%% @private
command(_SymbolicState, UPeer, VPeer) ->
    U = exactly(UPeer),
    V = exactly(VPeer),
    oneof([
        {call, ?SHIM, ping, [U, V]},
        {call, ?SHIM, alias_send, [U, V, gen_payload()]},
        {call, ?SHIM, reg_send, [U, V, gen_reg_name(UPeer, VPeer), gen_payload()]},
        {call, ?SHIM, send_sender, [U, V, gen_payload()]}
    ]).

precondition(SymbolicState, SymbolicCall) ->
    case ?MODEL:upeer(SymbolicState) of
        {ok, UPeer} ->
            precondition(SymbolicState, SymbolicCall, UPeer);
        error ->
            % UPeer HAS NOT been started, only VALID command is `start_upeer'
            case SymbolicCall of
                {call, _, noop, _} ->
                    true;
                {call, _, start_upeer, _} ->
                    true;
                {call, _, _, _} ->
                    false
            end
    end.

%% @private
precondition(SymbolicState, SymbolicCall, UPeer) ->
    case ?MODEL:vpeer(SymbolicState) of
        {ok, VPeer} ->
            precondition(SymbolicState, SymbolicCall, UPeer, VPeer);
        error ->
            % VPeer HAS NOT been started, only VALID command is `start_vpeer'
            case SymbolicCall of
                {call, _, noop, _} ->
                    true;
                {call, _, start_vpeer, _} ->
                    true;
                {call, _, _, _} ->
                    false
            end
    end.

%% @private
precondition(_SymbolicState, SymbolicCall, _UPeer, _VPeer) ->
    % UPeer and VPeer HAVE been started, only INVALID commands are `start_upeer' and `start_vpeer`
    case SymbolicCall of
        {call, _, start_upeer, _} ->
            false;
        {call, _, start_vpeer, _} ->
            false;
        {call, _, _, _} ->
            true
    end.

postcondition(State, SymbolicCall = {call, _, Func, Args}, Result) ->
    case ?MODEL:categorize_call(State, Func, Args) of
        {ok, dynamic} ->
            {_NewState, _ExpectedResult} = ?MODEL:dynamic_call(State, Func, Args, Result),
            true;
        {ok, symbolic} ->
            {_NewState, ExpectedResult} = ?MODEL:symbolic_call(State, Func, Args),
            case Result =:= ExpectedResult of
                true ->
                    true;
                false ->
                    ct:log(
                        error,
                        ?MAX_IMPORTANCE,
                        "Error: postcondition failure in ~w:~s/~w~nState = ~tp~nSymbolicCall = ~tp~nResult = ~tp~nExpectedResult = ~tp~n",
                        [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, State, SymbolicCall, Result, ExpectedResult]
                    ),
                    false
            end
    end.

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

gen_payload() ->
    frequency([
        {1, term()},
        {1, proper_vterm:vterm_simplify_into_term(#{large_binaries => true})}
    ]).

gen_reg_name() ->
    ?SUCHTHAT(
        RegName,
        atom(),
        RegName =/= '' andalso RegName =/= nil andalso RegName =/= undefined andalso
            erlang:whereis(RegName) =:= undefined
    ).

gen_reg_name(UPeer = {UPeerNode, _UPeerPid}, VPeer = {VPeerNode, _VPeerPid}) when
    is_atom(UPeerNode) andalso is_atom(VPeerNode)
->
    ?SUCHTHAT(
        RegName,
        gen_reg_name(),
        ?SHIM:rpc(UPeer, erlang, whereis, [RegName]) =:= undefined andalso
            ?SHIM:rpc(VPeer, erlang, whereis, [RegName]) =:= undefined
    );
gen_reg_name(US, VS) when ?is_symbol(US) orelse ?is_symbol(VS) ->
    {call, ?MODULE, gen_reg_name, [US, VS]}.

gen_upeer_node() ->
    ?LET(
        {Uniq, OsPid},
        {pos_integer(), pos_integer()},
        begin
            erlang:list_to_atom(lists:concat(["upeer-", Uniq, "-", OsPid, "@127.0.0.1"]))
        end
    ).

gen_vpeer_node() ->
    ?LET(
        {Uniq, OsPid},
        {pos_integer(), pos_integer()},
        begin
            erlang:list_to_atom(lists:concat(["vpeer-", Uniq, "-", OsPid, "@127.0.0.1"]))
        end
    ).

gen_vpeer_node({UPeerNode, _UPeerPid}) when is_atom(UPeerNode) ->
    [<<"upeer">>, Uniq, OsPid, Host = <<"127.0.0.1">>] = binary:split(
        erlang:atom_to_binary(UPeerNode, unicode), [<<"-">>, <<"@">>], [global, trim_all]
    ),
    erlang:binary_to_atom(<<"vpeer-", Uniq/binary, "-", OsPid/binary, "@", Host/binary>>, unicode);
gen_vpeer_node(S) when ?is_symbol(S) ->
    {call, ?MODULE, gen_vpeer_node, [S]}.
