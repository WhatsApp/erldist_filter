%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% Created :  13 Oct 2022 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
-module(erldist_filter_proxy).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(warn_missing_spec_all).

-behaviour(gen_server).

%% OTP API
-export([
    start/5,
    start_link/5
]).
%% API
-export([
    channel_close/1,
    channel_inspect/1,
    channel_recv/2
]).
%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

%% Records
-record(state, {
    channel = undefined :: undefined | erldist_filter_nif:channel(),
    parent = undefined :: undefined | {pid(), reference()}
}).

%% Types
-type error_reason() :: closed | not_owner.
-type exception() :: {Class :: 'error' | 'exit' | 'throw', Reason :: dynamic(), Stacktrace :: erlang:stacktrace()}.

-export_type([
    error_reason/0,
    exception/0
]).

%%%=============================================================================
%%% OTP API functions
%%%=============================================================================

-spec start(PacketSize, Sysname, Creation, ConnectionId, DistributionFlags) -> gen_server:start_ret() when
    PacketSize :: erldist_filter_nif:packet_size(),
    Sysname :: erldist_filter_nif:sysname(),
    Creation :: erldist_filter_nif:creation(),
    ConnectionId :: erldist_filter_nif:connection_id(),
    DistributionFlags :: erldist_filter_nif:distribution_flags().
start(PacketSize, Sysname, Creation, ConnectionId, DistributionFlags) ->
    gen_server:start(?MODULE, {self(), PacketSize, Sysname, Creation, ConnectionId, DistributionFlags}, []).

-spec start_link(PacketSize, Sysname, Creation, ConnectionId, DistributionFlags) -> gen_server:start_ret() when
    PacketSize :: erldist_filter_nif:packet_size(),
    Sysname :: erldist_filter_nif:sysname(),
    Creation :: erldist_filter_nif:creation(),
    ConnectionId :: erldist_filter_nif:connection_id(),
    DistributionFlags :: erldist_filter_nif:distribution_flags().
start_link(PacketSize, Sysname, Creation, ConnectionId, DistributionFlags) ->
    gen_server:start_link(?MODULE, {self(), PacketSize, Sysname, Creation, ConnectionId, DistributionFlags}, []).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec channel_close(Pid) -> ok | {error, not_owner | closed} when Pid :: pid().
channel_close(Pid) ->
    case erlang:is_process_alive(Pid) of
        true ->
            gen_server:call(Pid, channel_close);
        false ->
            {error, closed}
    end.

-spec channel_inspect(Pid) -> erldist_filter_nif:channel_inspection() | {error, closed} when Pid :: pid().
channel_inspect(Pid) ->
    case erlang:is_process_alive(Pid) of
        true ->
            gen_server:call(Pid, channel_inspect);
        false ->
            {error, closed}
    end.

-spec channel_recv(Pid, IoVec) -> [erldist_filter_nif:action()] | {error, error_reason()} | exception() when
    Pid :: pid(), IoVec :: erlang:iovec().
channel_recv(Pid, IoVec) ->
    case erlang:is_process_alive(Pid) of
        true ->
            gen_server:call(Pid, {channel_recv, IoVec});
        false ->
            {error, closed}
    end.

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

-spec init(tuple()) -> {ok, #state{}}.
init({ParentPid, PacketSize, Sysname, Creation, ConnectionId, DistributionFlags}) ->
    ParentMon = erlang:monitor(process, ParentPid),
    Channel = erldist_filter_nif:channel_open(PacketSize, Sysname, Creation, ConnectionId, DistributionFlags),
    State = #state{channel = Channel, parent = {ParentPid, ParentMon}},
    {ok, State}.

-spec handle_call(dynamic(), gen_server:from(), #state{}) ->
    {reply, term(), #state{}} | {stop, normal, term(), #state{}}.
handle_call(channel_close, _From, State0 = #state{channel = Channel}) when Channel =/= undefined ->
    try erldist_filter_nif:channel_close(Channel) of
        Reply ->
            State1 = State0#state{channel = undefined},
            {stop, normal, Reply, State1}
    catch
        Class:Reason:Stacktrace ->
            Reply = {Class, Reason, Stacktrace},
            State1 = State0#state{channel = undefined},
            {stop, normal, Reply, State1}
    end;
handle_call(channel_inspect, _From, State0 = #state{channel = Channel}) when Channel =/= undefined ->
    try erldist_filter_nif:channel_inspect(Channel) of
        Reply ->
            {reply, Reply, State0}
    catch
        Class:Reason:Stacktrace ->
            Reply = {Class, Reason, Stacktrace},
            _ = catch erldist_filter_nif:channel_close(Channel),
            State1 = State0#state{channel = undefined},
            {stop, normal, Reply, State1}
    end;
handle_call({channel_recv, IoVec}, _From, State0 = #state{channel = Channel}) when Channel =/= undefined ->
    try erldist_filter_nif:channel_recv(Channel, IoVec) of
        Reply ->
            {reply, Reply, State0}
    catch
        Class:Reason:Stacktrace ->
            Reply = {Class, Reason, Stacktrace},
            _ = catch erldist_filter_nif:channel_close(Channel),
            State1 = State0#state{channel = undefined},
            {stop, normal, Reply, State1}
    end.

-spec handle_cast(term(), #state{}) ->
    {noreply, #state{}}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) ->
    {stop, normal, #state{}}.
handle_info(
    {'DOWN', ParentMon, process, ParentPid, _Reason},
    State0 = #state{channel = Channel, parent = {ParentPid, ParentMon}}
) when Channel =/= undefined ->
    _ = catch erldist_filter_nif:channel_close(Channel),
    State1 = State0#state{channel = undefined, parent = undefined},
    {stop, normal, State1}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
