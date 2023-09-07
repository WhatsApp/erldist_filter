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
-module(erldist_filter_peer_spbt_shim).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(nowarn_missing_spec).

%% Shim API
-export([
    noop/0,
    start_upeer/1,
    start_vpeer/1,
    start_random_suffix_upeer_and_vpeer_from_label/1,
    ping/2,
    alias_send/3,
    reg_send/4,
    send_sender/3
]).
%% UPeer API
-export([
    upeer_ping/1,
    upeer_alias_send/2,
    upeer_reg_send/3,
    upeer_send_sender/2
]).
%% UNode/VNode Internal API
-export([
    vnode_aliased_process_init/2,
    vnode_registered_process_init/2,
    vnode_send_sender_process_init/2
]).
%% Internal functions
-export([
    rpc/4,
    rpc/5,
    wait_until_net_kernel_started/1
]).

%% Macros
-define(SUP, erldist_filter_peer_spbt_sup).
-define(RPC_DEFAULT_TIMEOUT, timer:minutes(1)).

%%%=============================================================================
%%% Shim API functions
%%%=============================================================================

noop() ->
    ok.

start_upeer(UPeerNode) ->
    start_peer(UPeerNode, ?MODULE, ?FUNCTION_NAME).

start_vpeer(VPeerNode) ->
    start_peer(VPeerNode, ?MODULE, ?FUNCTION_NAME).

start_random_suffix_upeer_and_vpeer_from_label(Label) when is_list(Label) ->
    Suffix = io_lib:format("~s-~4..0B", [os:getpid(), rand:uniform(9999)]),
    UName = list_to_atom(lists:flatten(io_lib:format("upeer-~s-~s@127.0.0.1", [Label, Suffix]))),
    VName = list_to_atom(lists:flatten(io_lib:format("vpeer-~s-~s@127.0.0.1", [Label, Suffix]))),
    {ok, UPeer} = start_upeer(UName),
    {ok, VPeer} = start_vpeer(VName),
    true = rpc(UPeer, net_kernel, connect_node, [VName]),
    true = rpc(VPeer, net_kernel, connect_node, [UName]),
    {ok, {UPeer, VPeer}}.

%% @private
start_peer(PeerNode, _Module, _FunctionName) ->
    [PeerName, PeerHost] = string:lexemes(atom_to_list(PeerNode), "@"),
    PeerOptions = #{
        connection => 0,
        name => PeerName,
        host => PeerHost,
        args => flatten_peer_args([
            {"-setcookie", "connect_cookie"},
            {"-connect_all", "false"},
            {"-kernel", "dist_auto_connect", "never"},
            {"-kernel", "start_distribution", "false"},
            {"-proto_dist", "erldist_filter_inet_tcp"}
        ])
    },
    ChildSpec = #{
        id => PeerNode,
        start => {peer, start_link, [PeerOptions]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [peer]
    },
    {ok, PeerPid, _ActualPeerNode} = ?SUP:start_child(ChildSpec),
    Peer = {PeerNode, PeerPid},
    _ = rpc(Peer, code, add_pathsa, [code:get_path()]),
    _ = rpc(Peer, net_kernel, start, [PeerNode, #{name_domain => longnames}]),
    _ = rpc(Peer, ?MODULE, wait_until_net_kernel_started, [PeerNode]),
    {ok, Peer}.

%% @private
flatten_peer_args([T | Rest]) when is_tuple(T) ->
    tuple_to_list(T) ++ flatten_peer_args(Rest);
% flatten_peer_args([L | Rest]) when is_list(L) ->
%     [L | flatten_peer_args(Rest)];
flatten_peer_args([]) ->
    [].

ping(U, _V = {VPeerNode, _VPeerPid}) ->
    rpc(U, ?MODULE, upeer_ping, [VPeerNode]).

alias_send(U, _V = {VPeerNode, _VPeerPid}, Term) ->
    rpc(U, ?MODULE, upeer_alias_send, [VPeerNode, Term]).

reg_send(U, _V = {VPeerNode, _VPeerPid}, RegName, Term) ->
    rpc(U, ?MODULE, upeer_reg_send, [VPeerNode, RegName, Term]).

send_sender(U, _V = {VPeerNode, _VPeerPid}, Term) ->
    rpc(U, ?MODULE, upeer_send_sender, [VPeerNode, Term]).

%%%=============================================================================
%%% UPeer API functions
%%%=============================================================================

upeer_ping(VPeerNode) ->
    net_adm:ping(VPeerNode).

upeer_alias_send(VNode, Term) ->
    upeer_gen_spawn(fun() ->
        UAlias = erlang:alias(),
        {_ReqId, VAlias} = unode_start_aliased_process(VNode, UAlias),
        UNode = node(),
        VAlias ! {UAlias, UNode, {ping, Term}},
        ok =
            receive
                {VAlias, VNode, {pong, Term}} -> ok
            end,
        VAlias ! {UAlias, UNode, stop},
        ok =
            receive
                {VAlias, VNode, stopped} -> ok
            end,
        true = erlang:unalias(UAlias),
        pong
    end).

upeer_reg_send(VNode, RegName, Term) ->
    upeer_gen_spawn(fun() ->
        true = erlang:register(RegName, self()),
        _MonitorRef = unode_start_registered_process(VNode, RegName),
        UNode = node(),
        {RegName, VNode} ! {RegName, UNode, {ping, Term}},
        ok =
            receive
                {RegName, VNode, {pong, Term}} -> ok
            end,
        {RegName, VNode} ! {RegName, UNode, stop},
        ok =
            receive
                {RegName, VNode, stopped} -> ok
            end,
        true = erlang:unregister(RegName),
        pong
    end).

upeer_send_sender(VNode, Term) ->
    upeer_gen_spawn(fun() ->
        UPid = erlang:self(),
        {_MonitorRef, VPid} = unode_start_send_sender_process(VNode, UPid),
        ThisNode = node(),
        VPid ! {UPid, ThisNode, {ping, Term}},
        ok =
            receive
                {VPid, VNode, {pong, Term}} -> ok
            end,
        VPid ! {UPid, ThisNode, stop},
        ok =
            receive
                {VPid, VNode, stopped} -> ok
            end,
        pong
    end).

%%%=============================================================================
%%% UNode/VNode Internal API functions
%%%=============================================================================

unode_start_aliased_process(VNode, UAlias) ->
    UParent = self(),
    ReqId = erlang:spawn_request(VNode, ?MODULE, vnode_aliased_process_init, [UParent, UAlias], [{reply, yes}]),
    receive
        {spawn_reply, ReqId, ok, VPid} ->
            receive
                {UParent, VPid, VAlias} ->
                    {ReqId, VAlias}
            end;
        {spawn_reply, ReqId, error, Reason} ->
            erlang:error(Reason)
    end.

vnode_aliased_process_init(UParent, UAlias) ->
    VAlias = erlang:alias(),
    UParent ! {UParent, self(), VAlias},
    vnode_aliased_process_loop(VAlias, UAlias).

%% @private
vnode_aliased_process_loop(VAlias, UAlias) ->
    receive
        {UAlias, UNode, {ping, Term}} when UNode =/= node() ->
            UAlias ! {VAlias, node(), {pong, Term}},
            vnode_aliased_process_loop(VAlias, UAlias);
        {UAlias, UNode, stop} when UNode =/= node() ->
            true = erlang:unalias(VAlias),
            UAlias ! {VAlias, node(), stopped},
            exit(normal)
    end.

%% @private
unode_start_registered_process(VNode, RegName) ->
    UParent = self(),
    ReqId = erlang:spawn_request(VNode, ?MODULE, vnode_registered_process_init, [UParent, RegName], [{reply, yes}]),
    receive
        {spawn_reply, ReqId, ok, VPid} ->
            receive
                {UParent, VPid, RegName} ->
                    ReqId
            end;
        {spawn_reply, ReqId, error, Reason} ->
            erlang:error(Reason)
    end.

vnode_registered_process_init(UParent, RegName) ->
    true = erlang:register(RegName, self()),
    UParent ! {UParent, self(), RegName},
    vnode_registered_process_loop(RegName).

%% @private
vnode_registered_process_loop(RegName) ->
    receive
        {RegName, UNode, {ping, Term}} when UNode =/= node() ->
            {RegName, UNode} ! {RegName, node(), {pong, Term}},
            vnode_registered_process_loop(RegName);
        {RegName, UNode, stop} when UNode =/= node() ->
            true = erlang:unregister(RegName),
            {RegName, UNode} ! {RegName, node(), stopped},
            exit(normal)
    end.

%% @private
unode_start_send_sender_process(VNode, UPid) ->
    UParent = self(),
    ReqId = erlang:spawn_request(VNode, ?MODULE, vnode_send_sender_process_init, [UParent, UPid], [{reply, yes}]),
    receive
        {spawn_reply, ReqId, ok, VPid} ->
            receive
                {UParent, VPid, VPid} ->
                    {ReqId, VPid}
            end;
        {spawn_reply, ReqId, error, Reason} ->
            erlang:error(Reason)
    end.

vnode_send_sender_process_init(UParent, UPid) ->
    VPid = erlang:self(),
    UParent ! {UParent, self(), VPid},
    vnode_send_sender_process_loop(VPid, UPid).

%% @private
vnode_send_sender_process_loop(VPid, UPid) ->
    receive
        {UPid, UNode, {ping, Term}} when UNode =/= node() ->
            UPid ! {VPid, node(), {pong, Term}},
            vnode_send_sender_process_loop(VPid, UPid);
        {UPid, UNode, stop} when UNode =/= node() ->
            UPid ! {VPid, node(), stopped},
            exit(normal)
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
rpc(Peer = {_PeerNode, _PeerPid}, Module, FunctionName, Arguments) ->
    rpc(Peer, Module, FunctionName, Arguments, ?RPC_DEFAULT_TIMEOUT).

%% @private
rpc(_Peer = {_PeerNode, PeerPid}, Module, FunctionName, Arguments, Timeout) ->
    peer:call(PeerPid, Module, FunctionName, Arguments, Timeout).

%% @private
upeer_gen_spawn(Fun) ->
    UParent = self(),
    {UPid, UMon} = spawn_opt(
        fun() ->
            UMon =
                receive
                    {UParent, UStartMon} -> UStartMon
                end,
            Reply = Fun(),
            UMon ! {UMon, self(), Reply},
            exit(normal)
        end,
        [{monitor, [{alias, reply_demonitor}]}]
    ),
    UPid ! {UParent, UMon},
    receive
        {UMon, UPid, Reply} -> Reply;
        {'DOWN', UMon, process, UPid, Reason} -> exit(Reason)
    end.

wait_until_net_kernel_started(Node) ->
    case net_kernel:get_state() of
        #{started := no} ->
            _ = net_kernel:start(Node, #{name_domain => longnames}),
            ok =
                receive
                after 100 -> ok
                end,
            wait_until_net_kernel_started(Node);
        #{started := Started, name := Node} when Started =/= no ->
            ok
    end.
