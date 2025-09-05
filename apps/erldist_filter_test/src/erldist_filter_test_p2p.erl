%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(erldist_filter_test_p2p).
-moduledoc """
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-08-26", modified => "2025-08-26"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-behaviour(gen_statem).

%% Public API
-export([
    close/1,
    open/1,
    open/2,
    open/3,
    peers/1
]).
%% OTP callbacks
-export([
    child_spec/0,
    start_link/4
]).
%% gen_statem callbacks
-export([
    callback_mode/0,
    init/1,
    terminate/3
]).
%% gen_statem states
-export([
    booting/3,
    connected/3
]).

%% Records
-record(boot_data, {
    owner :: monitor(),
    label :: label(),
    setup :: setup(),
    teardown :: teardown()
}).
-record(data, {
    owner :: monitor(),
    label :: label(),
    setup :: setup(),
    teardown :: teardown(),
    upeer :: peer_data(),
    vpeer :: peer_data()
}).
-record(init_data, {
    owner_pid :: pid(),
    label :: label(),
    setup :: setup(),
    teardown :: teardown()
}).
-record(monitor, {
    pid :: pid(),
    ref :: reference()
}).
-record(peer_data, {
    monitor :: monitor(),
    peer :: peer(),
    state :: dynamic()
}).

%% Internal Types
-type boot_data() :: #boot_data{}.
-type data() :: #data{}.
-type init_data() :: #init_data{}.
-type monitor() :: #monitor{}.
-type peer_data() :: #peer_data{}.

%% Types
-type event_content() :: dynamic().
-type label() :: erldist_filter_test_peer:name().
-type peer() :: {node(), pid()}.
-type peers() :: #{upeer := peer(), vpeer := peer()}.
-type setup() :: fun((peer()) -> State :: dynamic()).
-type stop_reason() :: dynamic().
-type teardown() :: fun((peer(), State :: dynamic()) -> Ignored :: dynamic()).

-export_type([
    event_content/0,
    label/0,
    peer/0,
    peers/0,
    setup/0,
    stop_reason/0,
    teardown/0
]).

%%%=============================================================================
%%% Public API functions
%%%=============================================================================

-spec close(Pid) -> ok when Pid :: pid().
close(Pid) when is_pid(Pid) ->
    case erlang:is_process_alive(Pid) of
        false ->
            ok;
        true ->
            gen_statem:call(Pid, close)
    end.

-spec open(Label) -> Pid when Label :: label(), Pid :: pid().
open(Label) when is_binary(Label) ->
    open(Label, fun noop_setup/1).

-spec open(Label, Setup) -> Pid when Label :: label(), Setup :: setup(), Pid :: pid().
open(Label, Setup) when is_binary(Label) andalso is_function(Setup, 1) ->
    open(Label, Setup, fun noop_teardown/2).

-spec open(Label, Setup, Teardown) -> Pid when Label :: label(), Setup :: setup(), Teardown :: teardown(), Pid :: pid().
open(Label, Setup, Teardown) when is_binary(Label) andalso is_function(Setup, 1) andalso is_function(Teardown, 2) ->
    OwnerPid = erlang:self(),
    case erldist_filter_test_p2p_sup:start_child(OwnerPid, Label, Setup, Teardown) of
        {ok, Pid} when is_pid(Pid) ->
            Pid
    end.

-spec peers(Pid) -> Peers when
    Pid :: pid(),
    Peers :: peers().
peers(Pid) when is_pid(Pid) ->
    gen_statem:call(Pid, peers).

%%%=============================================================================
%%% OTP callbacks
%%%=============================================================================

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    #{
        id => undefined,
        start => {?MODULE, start_link, []},
        restart => temporary,
        shutdown => brutal_kill,
        type => worker,
        modules => [?MODULE]
    }.

-spec start_link(OwnerPid, Label, Setup, Teardown) -> gen_statem:start_ret() when
    OwnerPid :: pid(),
    Label :: label(),
    Setup :: setup(),
    Teardown :: teardown().
start_link(OwnerPid, Label, Setup, Teardown) when
    is_pid(OwnerPid) andalso is_binary(Label) andalso is_function(Setup, 1) andalso is_function(Teardown, 2)
->
    InitData = #init_data{
        owner_pid = OwnerPid,
        label = Label,
        setup = Setup,
        teardown = Teardown
    },
    gen_statem:start_link(?MODULE, InitData, []).

%%%=============================================================================
%%% gen_statem callbacks
%%%=============================================================================

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    [state_functions].

-spec init(InitData) -> InitResult when
    InitData :: init_data(),
    State :: booting,
    BootData :: boot_data(),
    InitResult :: gen_statem:init_result(State, BootData).
init(#init_data{
    owner_pid = OwnerPid,
    label = Label,
    setup = Setup,
    teardown = Teardown
}) ->
    OwnerMon = erlang:monitor(process, OwnerPid),
    Owner = #monitor{pid = OwnerPid, ref = OwnerMon},
    BootData = #boot_data{
        owner = Owner,
        label = Label,
        setup = Setup,
        teardown = Teardown
    },
    Actions = [
        {next_event, internal, boot}
    ],
    {ok, booting, BootData, Actions}.

-spec terminate(Reason, State, BootData | Data) -> Ignored when
    Reason :: stop_reason(), State :: booting | connected, BootData :: boot_data(), Data :: data(), Ignored :: term().
terminate(_Reason, _State = booting, _BootData = #boot_data{}) ->
    ok;
terminate(_Reason, _State = connected, _Data = #data{}) ->
    ok.

%%%=============================================================================
%%% gen_statem states
%%%=============================================================================

-spec booting(EventType, EventContent, BootData) -> HandleEventResult when
    EventType :: gen_statem:event_type(),
    EventContent :: event_content(),
    State :: booting | connected,
    BootData :: boot_data(),
    Data :: data(),
    HandleEventResult :: gen_statem:event_handler_result(State, BootData | Data).
booting(internal, boot, #boot_data{owner = Owner, label = Label, setup = Setup, teardown = Teardown}) ->
    UPeerInfo = erldist_filter_test_peer:new(<<"upeer-", Label/bytes>>),
    VPeerInfo = UPeerInfo#{name := <<"vpeer-", Label/bytes>>},
    UPeerNode = erldist_filter_test_peer:encode(UPeerInfo),
    VPeerNode = erldist_filter_test_peer:encode(VPeerInfo),
    case erldist_filter_test_peer_sup:start_child(UPeerNode) of
        {ok, UPeerPid} when is_pid(UPeerPid) ->
            UPeer = {UPeerNode, UPeerPid},
            case erldist_filter_test_peer_sup:start_child(VPeerNode) of
                {ok, VPeerPid} when is_pid(VPeerPid) ->
                    VPeer = {VPeerNode, VPeerPid},
                    ok = ensure_connected(UPeer, VPeer),
                    UPeerState = Setup(UPeer),
                    VPeerState = Setup(VPeer),
                    UPeerMon = erlang:monitor(process, UPeerPid),
                    UPeerData = #peer_data{
                        monitor = #monitor{pid = UPeerPid, ref = UPeerMon},
                        peer = UPeer,
                        state = UPeerState
                    },
                    VPeerMon = erlang:monitor(process, VPeerPid),
                    VPeerData = #peer_data{
                        monitor = #monitor{pid = VPeerPid, ref = VPeerMon},
                        peer = VPeer,
                        state = VPeerState
                    },
                    Data = #data{
                        owner = Owner,
                        label = Label,
                        setup = Setup,
                        teardown = Teardown,
                        upeer = UPeerData,
                        vpeer = VPeerData
                    },
                    {next_state, connected, Data}
            end
    end;
booting({call, From}, close, _BootData = #boot_data{}) ->
    Replies = [{reply, From, ok}],
    {stop_and_reply, normal, Replies};
booting({call, _From}, peers, _BootData = #boot_data{}) ->
    Actions = [postpone],
    {keep_state_and_data, Actions}.

-spec connected(EventType, EventContent, Data) -> HandleEventResult when
    EventType :: gen_statem:event_type(),
    EventContent :: event_content(),
    State :: connected | booting,
    Data :: data(),
    HandleEventResult :: gen_statem:event_handler_result(State, Data | BootData),
    BootData :: boot_data().
connected(
    info,
    {'DOWN', OwnerMon, process, OwnerPid, _Reason},
    _Data = #data{
        owner = #monitor{pid = OwnerPid, ref = OwnerMon},
        upeer = #peer_data{monitor = #monitor{pid = UPeerPid, ref = UPeerMon}},
        vpeer = #peer_data{monitor = #monitor{pid = VPeerPid, ref = VPeerMon}}
    }
) ->
    _ = erlang:demonitor(UPeerMon, [flush]),
    _ = erlang:demonitor(VPeerMon, [flush]),
    ok = peer:stop(UPeerPid),
    ok = peer:stop(VPeerPid),
    stop;
connected(
    info,
    {'DOWN', UPeerMon, process, UPeerPid, _Reason},
    _Data = #data{
        owner = Owner,
        label = Label,
        setup = Setup,
        teardown = Teardown,
        upeer = #peer_data{monitor = #monitor{pid = UPeerPid, ref = UPeerMon}},
        vpeer = #peer_data{monitor = #monitor{pid = VPeerPid, ref = VPeerMon}}
    }
) ->
    _ = erlang:demonitor(VPeerMon, [flush]),
    ok = peer:stop(VPeerPid),
    BootData = #boot_data{
        owner = Owner,
        label = Label,
        setup = Setup,
        teardown = Teardown
    },
    Actions = [
        {next_event, internal, boot}
    ],
    {next_state, booting, BootData, Actions};
connected(
    info,
    {'DOWN', VPeerMon, process, VPeerPid, _Reason},
    _Data = #data{
        owner = Owner,
        label = Label,
        setup = Setup,
        teardown = Teardown,
        upeer = #peer_data{monitor = #monitor{pid = UPeerPid, ref = UPeerMon}},
        vpeer = #peer_data{monitor = #monitor{pid = VPeerPid, ref = VPeerMon}}
    }
) ->
    _ = erlang:demonitor(UPeerMon, [flush]),
    ok = peer:stop(UPeerPid),
    BootData = #boot_data{
        owner = Owner,
        label = Label,
        setup = Setup,
        teardown = Teardown
    },
    Actions = [
        {next_event, internal, boot}
    ],
    {next_state, booting, BootData, Actions};
connected(
    {call, From},
    close,
    _Data = #data{
        teardown = Teardown,
        upeer = #peer_data{monitor = #monitor{pid = UPeerPid, ref = UPeerMon}, peer = UPeer, state = UPeerState},
        vpeer = #peer_data{monitor = #monitor{pid = VPeerPid, ref = VPeerMon}, peer = VPeer, state = VPeerState}
    }
) ->
    _ = Teardown(UPeer, UPeerState),
    _ = Teardown(VPeer, VPeerState),
    _ = erlang:demonitor(UPeerMon, [flush]),
    _ = erlang:demonitor(VPeerMon, [flush]),
    ok = peer:stop(UPeerPid),
    ok = peer:stop(VPeerPid),
    Replies = [{reply, From, ok}],
    {stop_and_reply, normal, Replies};
connected(
    {call, From},
    peers,
    _Data = #data{upeer = #peer_data{peer = UPeer}, vpeer = #peer_data{peer = VPeer}}
) ->
    % ok = ensure_connected(UPeer, VPeer),
    Actions = [{reply, From, #{upeer => UPeer, vpeer => VPeer}}],
    {keep_state_and_data, Actions}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-spec ensure_connected(UPeer, VPeer) -> ok when UPeer :: peer(), VPeer :: peer().
ensure_connected({UPeerNode, UPeerPid}, {VPeerNode, VPeerPid}) ->
    true = peer:call(UPeerPid, net_kernel, connect_node, [VPeerNode]),
    true = peer:call(VPeerPid, net_kernel, connect_node, [UPeerNode]),
    ok.

-spec noop_setup(peer()) -> State :: dynamic().
noop_setup(_Peer) ->
    ignored.

-spec noop_teardown(peer(), State :: dynamic()) -> Ignored :: dynamic().
noop_teardown(_Peer, _State) ->
    ignored.
