%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(erldist_filter_peer_spbt_shim).
-moduledoc """
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2022-09-22", modified => "2025-08-29"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% Shim API
-export([
    noop/0,
    open_p2p/1,
    ping/1,
    alias_priority_send/2,
    alias_send/2,
    exit2_priority_signal/2,
    exit2_signal/2,
    reg_send/3,
    send_sender/2
]).
%% UPeer API
-export([
    upeer_ping/1,
    upeer_alias_priority_send/2,
    upeer_alias_send/2,
    upeer_exit2_priority_signal/2,
    upeer_exit2_signal/2,
    upeer_reg_send/3,
    upeer_send_sender/2
]).
%% UNode/VNode Internal API
-export([
    vnode_aliased_priority_process_init/2,
    vnode_aliased_process_init/2,
    vnode_exit2_priority_process_init/1,
    vnode_exit2_process_init/1,
    vnode_registered_process_init/2,
    vnode_send_sender_process_init/2
]).
%% Internal functions
-export([
    rpc/4,
    rpc/5
]).

%% Types
-type p2p() :: pid().

-export_type([
    p2p/0
]).

%% Macros
-define(RPC_DEFAULT_TIMEOUT, timer:minutes(1)).

%%%=============================================================================
%%% Shim API functions
%%%=============================================================================

-spec noop() -> ok.
noop() ->
    ok.

-spec open_p2p(Label) -> {ok, P2P} when Label :: erldist_filter_test_p2p:label(), P2P :: p2p().
open_p2p(Label) ->
    P2P = erldist_filter_test_p2p:open(Label),
    {ok, P2P}.

-spec ping(P2P) -> pong | pang when P2P :: p2p().
ping(P2P) ->
    #{upeer := U, vpeer := {VPeerNode, _VPeerPid}} = erldist_filter_test_p2p:peers(P2P),
    rpc(U, ?MODULE, upeer_ping, [VPeerNode]).

-spec alias_priority_send(P2P, Term) -> pong when P2P :: p2p(), Term :: term().
alias_priority_send(P2P, Term) ->
    #{upeer := U, vpeer := {VPeerNode, _VPeerPid}} = erldist_filter_test_p2p:peers(P2P),
    rpc(U, ?MODULE, upeer_alias_priority_send, [VPeerNode, Term]).

-spec alias_send(P2P, Term) -> pong when P2P :: p2p(), Term :: term().
alias_send(P2P, Term) ->
    #{upeer := U, vpeer := {VPeerNode, _VPeerPid}} = erldist_filter_test_p2p:peers(P2P),
    rpc(U, ?MODULE, upeer_alias_send, [VPeerNode, Term]).

-spec exit2_priority_signal(P2P, Term) -> pong when P2P :: p2p(), Term :: term().
exit2_priority_signal(P2P, Term) ->
    #{upeer := U, vpeer := {VPeerNode, _VPeerPid}} = erldist_filter_test_p2p:peers(P2P),
    rpc(U, ?MODULE, upeer_exit2_priority_signal, [VPeerNode, Term]).

-spec exit2_signal(P2P, Term) -> pong when P2P :: p2p(), Term :: term().
exit2_signal(P2P, Term) ->
    #{upeer := U, vpeer := {VPeerNode, _VPeerPid}} = erldist_filter_test_p2p:peers(P2P),
    rpc(U, ?MODULE, upeer_exit2_signal, [VPeerNode, Term]).

-spec reg_send(P2P, RegName, Term) -> pong when P2P :: p2p(), RegName :: atom(), Term :: term().
reg_send(P2P, RegName, Term) ->
    #{upeer := U, vpeer := {VPeerNode, _VPeerPid}} = erldist_filter_test_p2p:peers(P2P),
    rpc(U, ?MODULE, upeer_reg_send, [VPeerNode, RegName, Term]).

-spec send_sender(P2P, Term) -> pong when P2P :: p2p(), Term :: term().
send_sender(P2P, Term) ->
    #{upeer := U, vpeer := {VPeerNode, _VPeerPid}} = erldist_filter_test_p2p:peers(P2P),
    rpc(U, ?MODULE, upeer_send_sender, [VPeerNode, Term]).

%%%=============================================================================
%%% UPeer API functions
%%%=============================================================================

-spec upeer_ping(node()) -> pong | pang.
upeer_ping(VPeerNode) ->
    net_adm:ping(VPeerNode).

-spec upeer_alias_priority_send(node(), term()) -> pong.
upeer_alias_priority_send(VNode, Term) ->
    upeer_gen_spawn(fun() ->
        UAlias = erlang:alias([priority]),
        {_ReqId, VAlias} = unode_start_aliased_priority_process(VNode, UAlias),
        UNode = node(),
        _ = erlang:send(VAlias, {UAlias, UNode, {ping, Term}}, [priority]),
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

-spec upeer_alias_send(node(), term()) -> pong.
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

-spec upeer_exit2_priority_signal(node(), term()) -> pong.
upeer_exit2_priority_signal(VNode, Term) ->
    upeer_gen_spawn(fun() ->
        {VPid, VMon, VAlias} = unode_start_exit2_priority_process(VNode),
        true = erlang:exit(VAlias, {ping, Term}, [priority]),
        ok =
            receive
                {'DOWN', VMon, process, VPid, {ping, Term}} -> ok
            end,
        pong
    end).

-spec upeer_exit2_signal(node(), term()) -> pong.
upeer_exit2_signal(VNode, Term) ->
    upeer_gen_spawn(fun() ->
        {VPid, VMon} = unode_start_exit2_process(VNode),
        true = erlang:exit(VPid, {ping, Term}),
        ok =
            receive
                {'DOWN', VMon, process, VPid, {ping, Term}} -> ok
            end,
        pong
    end).

-spec upeer_reg_send(node(), atom(), term()) -> pong.
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

-spec upeer_send_sender(node(), term()) -> pong.
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

-spec unode_start_aliased_priority_process(VNode, UAlias) -> {ReqId, VAlias} when
    VNode :: node(),
    UAlias :: reference(),
    ReqId :: reference(),
    VAlias :: reference().
unode_start_aliased_priority_process(VNode, UAlias) ->
    UParent = self(),
    ReqId = erlang:spawn_request(VNode, ?MODULE, vnode_aliased_priority_process_init, [UParent, UAlias], [{reply, yes}]),
    receive
        {spawn_reply, ReqId, ok, VPid} ->
            receive
                {UParent, VPid, VAlias} ->
                    {ReqId, VAlias}
            end;
        {spawn_reply, ReqId, error, Reason} ->
            erlang:error(Reason)
    end.

-spec unode_start_aliased_process(VNode, UAlias) -> {ReqId, VAlias} when
    VNode :: node(),
    UAlias :: reference(),
    ReqId :: reference(),
    VAlias :: reference().
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

-spec unode_start_exit2_priority_process(VNode) -> {VPid, VMon, VAlias} when
    VNode :: node(),
    VPid :: pid(),
    VMon :: reference(),
    VAlias :: reference().
unode_start_exit2_priority_process(VNode) ->
    UParent = self(),
    ReqId = erlang:spawn_request(VNode, ?MODULE, vnode_exit2_priority_process_init, [UParent], [monitor, {reply, yes}]),
    receive
        {spawn_reply, ReqId, ok, VPid} ->
            VMon = ReqId,
            receive
                {UParent, VPid, VAlias} ->
                    {VPid, VMon, VAlias}
            end;
        {spawn_reply, ReqId, error, Reason} ->
            erlang:error(Reason)
    end.

-spec unode_start_exit2_process(VNode) -> {VPid, VMon} when
    VNode :: node(),
    VPid :: pid(),
    VMon :: reference().
unode_start_exit2_process(VNode) ->
    UParent = self(),
    ReqId = erlang:spawn_request(VNode, ?MODULE, vnode_exit2_process_init, [UParent], [monitor, {reply, yes}]),
    receive
        {spawn_reply, ReqId, ok, VPid} ->
            VMon = ReqId,
            {VPid, VMon};
        {spawn_reply, ReqId, error, Reason} ->
            erlang:error(Reason)
    end.

-spec vnode_aliased_priority_process_init(UParent :: pid(), UAlias :: reference()) -> no_return().
vnode_aliased_priority_process_init(UParent, UAlias) ->
    VAlias = erlang:alias([priority]),
    UParent ! {UParent, self(), VAlias},
    vnode_aliased_priority_process_loop(VAlias, UAlias).

-spec vnode_aliased_priority_process_loop(VAlias, UAlias) -> no_return() when
    VAlias :: reference(),
    UAlias :: reference().
vnode_aliased_priority_process_loop(VAlias, UAlias) ->
    receive
        {UAlias, UNode, {ping, Term}} when UNode =/= node() ->
            _ = erlang:send(UAlias, {VAlias, node(), {pong, Term}}, [priority]),
            vnode_aliased_process_loop(VAlias, UAlias);
        {UAlias, UNode, stop} when UNode =/= node() ->
            true = erlang:unalias(VAlias),
            UAlias ! {VAlias, node(), stopped},
            exit(normal)
    end.

-spec vnode_aliased_process_init(UParent :: pid(), UAlias :: reference()) -> no_return().
vnode_aliased_process_init(UParent, UAlias) ->
    VAlias = erlang:alias(),
    UParent ! {UParent, self(), VAlias},
    vnode_aliased_process_loop(VAlias, UAlias).

-spec vnode_aliased_process_loop(VAlias, UAlias) -> no_return() when
    VAlias :: reference(),
    UAlias :: reference().
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

-spec vnode_exit2_priority_process_init(UParent) -> no_return() when UParent :: pid().
vnode_exit2_priority_process_init(UParent) ->
    VAlias = erlang:alias([priority]),
    UParent ! {UParent, self(), VAlias},
    vnode_exit2_priority_process_loop(UParent).

-spec vnode_exit2_priority_process_loop(UParent) -> no_return() when UParent :: pid().
vnode_exit2_priority_process_loop(UParent) ->
    receive
        _ ->
            vnode_exit2_priority_process_loop(UParent)
    end.

-spec vnode_exit2_process_init(UParent) -> no_return() when UParent :: pid().
vnode_exit2_process_init(UParent) ->
    vnode_exit2_process_loop(UParent).

-spec vnode_exit2_process_loop(UParent) -> no_return() when UParent :: pid().
vnode_exit2_process_loop(UParent) ->
    receive
        _ ->
            vnode_exit2_process_loop(UParent)
    end.

-spec unode_start_registered_process(VNode, RegName) -> ReqId when
    VNode :: node(),
    RegName :: atom(),
    ReqId :: reference().
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

-spec vnode_registered_process_init(UParent :: pid(), RegName :: atom()) -> no_return().
vnode_registered_process_init(UParent, RegName) ->
    true = erlang:register(RegName, self()),
    UParent ! {UParent, self(), RegName},
    vnode_registered_process_loop(RegName).

-spec vnode_registered_process_loop(RegName) -> no_return() when RegName :: atom().
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

-spec unode_start_send_sender_process(VNode, UPid) -> {ReqId, VPid} when
    VNode :: node(),
    UPid :: pid(),
    ReqId :: reference(),
    VPid :: pid().
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

-spec vnode_send_sender_process_init(UParent :: pid(), UPid :: pid()) -> no_return().
vnode_send_sender_process_init(UParent, UPid) ->
    VPid = erlang:self(),
    UParent ! {UParent, self(), VPid},
    vnode_send_sender_process_loop(VPid, UPid).

-spec vnode_send_sender_process_loop(VPid, UPid) -> no_return() when
    VPid :: pid(),
    UPid :: pid().
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

-doc false.
-spec rpc(Peer, Module, FunctionName, Arguments) -> Result when
    Peer :: erldist_filter_test_p2p:peer(),
    Module :: module(),
    FunctionName :: atom(),
    Arguments :: nil() | [dynamic()],
    Result :: dynamic().
rpc(Peer = {_PeerNode, _PeerPid}, Module, FunctionName, Arguments) ->
    rpc(Peer, Module, FunctionName, Arguments, ?RPC_DEFAULT_TIMEOUT).

-doc false.
-spec rpc(Peer, Module, FunctionName, Arguments, Timeout) -> Result when
    Peer :: erldist_filter_test_p2p:peer(),
    Module :: module(),
    FunctionName :: atom(),
    Arguments :: nil() | [dynamic()],
    Timeout :: timeout(),
    Result :: dynamic().
rpc(_Peer = {_PeerNode, PeerPid}, Module, FunctionName, Arguments, Timeout) ->
    peer:call(PeerPid, Module, FunctionName, Arguments, Timeout).

-doc false.
-spec upeer_gen_spawn(Fun) -> Reply when Fun :: fun(() -> Reply), Reply :: dynamic().
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
