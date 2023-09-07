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
%%% Created :  10 Aug 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(erldist_filter_router).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(gen_statem).

%% OTP callbacks
-export([
    child_name/1,
    child_spec/1,
    start_link/2
]).

%% gen_statem callbacks
-export([
    callback_mode/0,
    init/1,
    terminate/3
]).
%% gen_statem states
-export([
    route/3
]).

%% Records
-record(init_data, {
    server_name :: gen_statem:server_name(),
    router_number :: router_number()
}).
-record(data, {
    server_name :: gen_statem:server_name(),
    router_number :: router_number(),
    handler_sup :: atom(),
    handlers = #{} :: #{node() => pid()},
    monitors = #{} :: #{reference() => {node(), pid()}}
}).

%% Types
-type init_data() :: #init_data{}.
-type data() :: #data{}.

%% Macros
-define(is_router_number(X), (is_integer(X) andalso (X) >= 1)).

%% Types
-type gen_statem_event_content() :: eqwalizer:dynamic().
-type stop_reason() :: normal | shutdown | {shutdown, term()} | term() | eqwalizer:dynamic().
-type router_number() :: pos_integer().

-export_type([
    router_number/0
]).

%%%=============================================================================
%%% OTP callbacks
%%%=============================================================================

-spec child_name(RouterNumber) -> atom() when RouterNumber :: router_number().
child_name(RouterNumber) when ?is_router_number(RouterNumber) ->
    erlang:binary_to_atom(
        <<"erldist_filter_router_", (erlang:integer_to_binary(RouterNumber))/bytes>>,
        utf8
    ).

-spec child_spec(RouterNumber) -> supervisor:child_spec() when
    RouterNumber :: router_number().
child_spec(RouterNumber) when ?is_router_number(RouterNumber) ->
    ChildName = child_name(RouterNumber),
    #{
        id => ChildName,
        start => {?MODULE, start_link, [{local, ChildName}, RouterNumber]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [?MODULE]
    }.

-spec start_link(ServerName, RouterNumber) -> gen_statem:start_ret() when
    ServerName :: gen_statem:server_name(),
    RouterNumber :: router_number().
start_link(ServerName, RouterNumber) when ?is_router_number(RouterNumber) ->
    InitData = #init_data{
        server_name = ServerName,
        router_number = RouterNumber
    },
    gen_statem:start_link(ServerName, ?MODULE, InitData, [{hibernate_after, 5000}]).

%%%=============================================================================
%%% gen_statem callbacks
%%%=============================================================================

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    [state_functions].

-spec init(InitData) -> InitResult when
    InitData :: init_data(),
    State :: route,
    Data :: data(),
    InitResult :: gen_statem:init_result(State, Data).
init(#init_data{
    server_name = ServerName,
    router_number = RouterNumber
}) ->
    HandlerSup = erldist_filter_handler_sup:child_name(RouterNumber),
    Data = #data{
        server_name = ServerName,
        router_number = RouterNumber,
        handler_sup = HandlerSup
    },
    {ok, route, Data}.

-spec terminate(Reason, State, Data) -> Ignored when
    Reason :: stop_reason(), State :: route, Data :: data(), Ignored :: term().
terminate(_Reason, _State, _Data = #data{}) ->
    ok.

%%%=============================================================================
%%% gen_statem states
%%%=============================================================================

-spec route(EventType, EventContent, Data) -> HandleEventResult when
    EventType :: gen_statem:event_type(),
    EventContent :: gen_statem_event_content(),
    State :: route,
    Data :: data(),
    HandleEventResult :: gen_statem:event_handler_result(State, Data).
route(info, Info, Data0 = #data{handlers = Handlers0, monitors = Monitors0}) ->
    case Info of
        {Sysname, _Sort, _Control} ->
            route_to_handler(Sysname, Info, Data0);
        {Sysname, _Sort, _Control, _Payload} ->
            route_to_handler(Sysname, Info, Data0);
        {'DOWN', HandlerMon, process, HandlerPid, _Reason} ->
            case maps:take(HandlerMon, Monitors0) of
                {{Sysname, HandlerPid}, Monitors1} ->
                    Handlers1 = maps:remove(Sysname, Handlers0),
                    Data1 = Data0#data{handlers = Handlers1, monitors = Monitors1},
                    {keep_state, Data1};
                error ->
                    keep_state_and_data
            end
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
route_to_handler(Sysname, Operation, Data = #data{handlers = Handlers}) ->
    case maps:find(Sysname, Handlers) of
        {ok, HandlerPid} ->
            _ = catch erlang:send(HandlerPid, Operation, [noconnect]),
            keep_state_and_data;
        error ->
            start_and_route_to_handler(Sysname, Operation, Data)
    end.

%% @private
start_and_route_to_handler(
    Sysname, Operation, Data0 = #data{handler_sup = HandlerSup, handlers = Handlers0, monitors = Monitors0}
) ->
    case supervisor:start_child(HandlerSup, [Sysname]) of
        {ok, HandlerPid} ->
            _ = catch erlang:send(HandlerPid, Operation, [noconnect]),
            HandlerMon = erlang:monitor(process, HandlerPid),
            Handlers1 = maps:put(Sysname, HandlerPid, Handlers0),
            Monitors1 = maps:put(HandlerMon, {Sysname, HandlerPid}, Monitors0),
            Data1 = Data0#data{handlers = Handlers1, monitors = Monitors1},
            {keep_state, Data1}
    end.
