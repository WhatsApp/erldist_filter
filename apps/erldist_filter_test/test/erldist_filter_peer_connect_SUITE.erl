%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(erldist_filter_peer_connect_SUITE).
-moduledoc """
## Connection Setup Regression Tests for Peers Connected Through erldist_filter

### Test Cases

- **first_frame_after_connect**: repeatedly connects `upeer` to `vpeer` and
  sends a `spawn_request` as the first frame on each new connection while
  `vpeer` runs a single busy scheduler, so its distribution receiver is slow
  to take over the socket. If the socket is active before the receiver owns
  it, the frame arrives as `{Port, {data, _}}`, the receiver exits with
  `{badmsg, _}`, and the spawn request fails with `noconnection`.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2026-10-01", modified => "2026-10-01"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("stdlib/include/assert.hrl").

-behaviour(ct_suite).

%% ct_suite callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test Cases
-export([
    first_frame_after_connect/0,
    first_frame_after_connect/1
]).

%% Peer helpers
-export([
    busy_start/1,
    busy_stop/1,
    connect_and_spawn/1,
    disconnect/1,
    wait_disconnected/1
]).

%% Macros
-define(ITERATIONS, 20).
-define(BUSY_PROCESSES, 8).

%%%=============================================================================
%%% ct_suite callbacks
%%%=============================================================================

-spec all() -> erldist_filter_test:all().
all() ->
    [
        first_frame_after_connect
    ].

-spec init_per_suite(Config :: ct_suite:ct_config()) -> erldist_filter_test:init_per_suite().
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(erldist_filter_test),
    Config.

-spec end_per_suite(Config :: ct_suite:ct_config()) -> erldist_filter_test:end_per_suite().
end_per_suite(_Config) ->
    ok.

-spec init_per_testcase(TestCase :: ct_suite:ct_testname(), Config :: ct_suite:ct_config()) ->
    erldist_filter_test:init_per_testcase().
init_per_testcase(first_frame_after_connect, Config) ->
    P2P = erldist_filter_test_p2p:open(<<"edf-first-frame">>),
    [{p2p, P2P} | Config].

-spec end_per_testcase(TestCase :: ct_suite:ct_testname(), Config :: ct_suite:ct_config()) ->
    erldist_filter_test:end_per_testcase().
end_per_testcase(_TestCase, Config) ->
    case lists:keyfind(p2p, 1, Config) of
        {p2p, P2P} when is_pid(P2P) ->
            ok = erldist_filter_test_p2p:close(P2P),
            ok
    end.

%%%=============================================================================
%%% Test Cases
%%%=============================================================================

-spec first_frame_after_connect() -> erldist_filter_test:testcase_info().
first_frame_after_connect() ->
    [
        {doc, "The first frame on a new connection survives a slow receiver handoff"},
        {timetrap, {seconds, 300}}
    ].

-spec first_frame_after_connect(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
first_frame_after_connect(Config) ->
    {p2p, P2P} = lists:keyfind(p2p, 1, Config),
    #{upeer := {UNode, UPid}, vpeer := {VNode, VPid}} = erldist_filter_test_p2p:peers(P2P),
    ok = load_helpers(UPid),
    ok = load_helpers(VPid),
    Results = [first_frame(UPid, UNode, VPid, VNode) || _ <- lists:seq(1, ?ITERATIONS)],
    ?assertEqual([], [Result || Result <- Results, Result =/= ok]),
    ok.

%%%=============================================================================
%%% Peer helpers
%%%=============================================================================

-spec busy_start(Count) -> SchedulersOnline when Count :: pos_integer(), SchedulersOnline :: pos_integer().
busy_start(Count) ->
    SchedulersOnline = erlang:system_flag(schedulers_online, 1),
    Pids = [erlang:spawn(fun spin/0) || _ <- lists:seq(1, Count)],
    persistent_term:put({?MODULE, busy}, Pids),
    SchedulersOnline.

-spec busy_stop(SchedulersOnline) -> ok when SchedulersOnline :: pos_integer().
busy_stop(SchedulersOnline) ->
    _ = [erlang:exit(Pid, kill) || Pid <- persistent_term:get({?MODULE, busy})],
    _ = persistent_term:erase({?MODULE, busy}),
    _ = erlang:system_flag(schedulers_online, SchedulersOnline),
    ok.

-spec connect_and_spawn(Node) -> ok | {error, Reason} when Node :: node(), Reason :: dynamic().
connect_and_spawn(Node) ->
    true = net_kernel:connect_node(Node),
    ReqId = erlang:spawn_request(Node, erlang, node, [], [{reply, yes}]),
    receive
        {spawn_reply, ReqId, ok, _Pid} -> ok;
        {spawn_reply, ReqId, error, Reason} -> {error, Reason}
    end.

-spec disconnect(Node) -> ok when Node :: node().
disconnect(Node) ->
    _ = erlang:disconnect_node(Node),
    ok.

-spec wait_disconnected(Node) -> ok when Node :: node().
wait_disconnected(Node) ->
    case lists:member(Node, erlang:nodes(connected)) of
        true ->
            ok =
                receive
                after 5 -> ok
                end,
            wait_disconnected(Node);
        false ->
            ok
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec first_frame(UPid, UNode, VPid, VNode) -> ok | {error, Reason} when
    UPid :: pid(), UNode :: node(), VPid :: pid(), VNode :: node(), Reason :: dynamic().
first_frame(UPid, UNode, VPid, VNode) ->
    ok = peer:call(UPid, ?MODULE, disconnect, [VNode]),
    ok = peer:call(UPid, ?MODULE, wait_disconnected, [VNode]),
    ok = peer:call(VPid, ?MODULE, wait_disconnected, [UNode]),
    SchedulersOnline = peer:call(VPid, ?MODULE, busy_start, [?BUSY_PROCESSES]),
    try
        peer:call(UPid, ?MODULE, connect_and_spawn, [VNode])
    after
        ok = peer:call(VPid, ?MODULE, busy_stop, [SchedulersOnline])
    end.

%% @private
-spec load_helpers(PeerPid) -> ok when PeerPid :: pid().
load_helpers(PeerPid) ->
    {?MODULE, Binary, Filename} = code:get_object_code(?MODULE),
    {module, ?MODULE} = peer:call(PeerPid, code, load_binary, [?MODULE, Filename, Binary]),
    ok.

%% @private
-spec spin() -> no_return().
spin() ->
    spin().
