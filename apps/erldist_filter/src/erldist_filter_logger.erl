%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% Created :  10 May 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
-module(erldist_filter_logger).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(gen_statem).

%% OTP callbacks
-export([
    child_name/2,
    child_spec/3,
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
    recv/3,
    wait/3
]).

%% Records
-record(init_data, {
    server_name :: gen_statem:server_name(),
    handler :: handler(),
    handler_options :: handler_options(),
    worker_number :: worker_number()
}).
-record(data, {
    server_name :: gen_statem:server_name(),
    handler :: handler(),
    handler_state :: handler_state(),
    worker_number :: worker_number(),
    logger :: erldist_filter_nif:logger(),
    logger_select_handle = undefined :: undefined | erldist_filter_nif:logger_select_handle()
}).

%% Types
-type init_data() :: #init_data{}.
-type data() :: #data{}.

%% Macros
-define(is_handler(X), is_atom(X)).
-define(is_worker_number(X), (is_integer(X) andalso (X) >= 1)).

%% Types
-type batch_event() :: {erldist_filter_nif:logger_time(), erldist_filter_nif:logger_event()}.
-type gen_statem_event_content() :: dynamic().
-type handle_batch_result() :: handle_batch_result(handler_state()).
-type handle_batch_result(State) ::
    handle_events
    | {handle_events, [batch_event()]}
    | {handle_events, [batch_event()], State}
    | stop
    | {stop, stop_reason()}
    | {stop, stop_reason(), State}.
-type handle_event_result() :: handle_event_result(handler_state()).
-type handle_event_result(State) :: cont | {cont, State} | stop | {stop, stop_reason()} | {stop, stop_reason(), State}.
-type handle_info_result() :: handle_info_result(handler_state()).
-type handle_info_result(State) :: cont | {cont, State} | stop | {stop, stop_reason()} | {stop, stop_reason(), State}.
-type handler() :: module().
-type handler_options() :: term() | dynamic().
-type handler_state() :: term() | dynamic().
-type init_result() :: init_result(handler_state()).
-type init_result(State) :: {ok, State} | ignore | {stop, stop_reason()}.
-type stop_reason() :: normal | shutdown | {shutdown, term()} | term() | dynamic().
-type worker_number() :: pos_integer().

-export_type([
    batch_event/0,
    handle_batch_result/0,
    handle_batch_result/1,
    handle_event_result/0,
    handle_event_result/1,
    handle_info_result/0,
    handle_info_result/1,
    handler/0,
    handler_options/0,
    handler_state/0,
    init_result/0,
    init_result/1,
    stop_reason/0,
    worker_number/0
]).

%% Callbacks
-callback init(HandlerOptions :: handler_options(), WorkerNumber :: worker_number()) -> init_result().
-callback handle_batch(
    BatchSize :: non_neg_integer(),
    BatchDrop :: non_neg_integer(),
    BatchEvents :: [batch_event()],
    HandlerState :: handler_state()
) -> handle_batch_result().
-callback handle_control_event(
    Time :: erldist_filter_nif:logger_time(),
    Sysname :: node(),
    Control :: dynamic(),
    HandlerState :: handler_state()
) -> handle_event_result().
-callback handle_payload_event(
    Time :: erldist_filter_nif:logger_time(),
    Sysname :: node(),
    Control :: dynamic(),
    Payload :: dynamic(),
    HandlerState :: handler_state()
) -> handle_event_result().
-callback handle_info(Info :: term() | dynamic(), HandlerState :: handler_state()) -> handle_info_result().
-callback terminate(Reason :: stop_reason(), HandlerState :: handler_state()) -> Ignored :: term().

-optional_callbacks([
    handle_info/2,
    terminate/2
]).

%%%=============================================================================
%%% OTP callbacks
%%%=============================================================================

-spec child_name(Handler, WorkerNumber) -> atom() when Handler :: handler(), WorkerNumber :: worker_number().
child_name(Handler, WorkerNumber) when ?is_handler(Handler) andalso ?is_worker_number(WorkerNumber) ->
    erlang:binary_to_atom(
        <<"erldist_filter_logger_", (erlang:atom_to_binary(Handler, utf8))/bytes, $_,
            (erlang:integer_to_binary(WorkerNumber))/bytes>>,
        utf8
    ).

-spec child_spec(Handler, HandlerOptions, WorkerNumber) -> supervisor:child_spec() when
    Handler :: handler(), HandlerOptions :: handler_options(), WorkerNumber :: worker_number().
child_spec(Handler, HandlerOptions, WorkerNumber) when ?is_handler(Handler) andalso ?is_worker_number(WorkerNumber) ->
    ChildName = child_name(Handler, WorkerNumber),
    #{
        id => ChildName,
        start => {?MODULE, start_link, [{local, ChildName}, Handler, HandlerOptions, WorkerNumber]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [?MODULE]
    }.

-spec start_link(ServerName, Handler, HandlerOptions, WorkerNumber) -> gen_statem:start_ret() when
    ServerName :: gen_statem:server_name(),
    Handler :: handler(),
    HandlerOptions :: handler_options(),
    WorkerNumber :: worker_number().
start_link(ServerName, Handler, HandlerOptions, WorkerNumber) when
    ?is_handler(Handler) andalso ?is_worker_number(WorkerNumber)
->
    InitData = #init_data{
        server_name = ServerName,
        handler = Handler,
        handler_options = HandlerOptions,
        worker_number = WorkerNumber
    },
    gen_statem:start_link(ServerName, ?MODULE, InitData, []).

%%%=============================================================================
%%% gen_statem callbacks
%%%=============================================================================

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    [state_functions, state_enter].

-spec init(InitData) -> InitResult when
    InitData :: init_data(),
    State :: recv,
    Data :: data(),
    InitResult :: gen_statem:init_result(State, Data).
init(#init_data{
    server_name = ServerName,
    handler = Handler,
    handler_options = HandlerOptions,
    worker_number = WorkerNumber
}) ->
    case Handler:init(HandlerOptions, WorkerNumber) of
        {ok, HandlerState} ->
            Logger = erldist_filter_nif:logger_open(),
            Data = #data{
                server_name = ServerName,
                handler = Handler,
                handler_state = HandlerState,
                worker_number = WorkerNumber,
                logger = Logger,
                logger_select_handle = undefined
            },
            {ok, recv, Data};
        ignore ->
            ignore;
        {stop, Reason} ->
            {stop, Reason}
    end.

-spec terminate(Reason, State, Data) -> Ignored when
    Reason :: stop_reason(), State :: recv | wait, Data :: data(), Ignored :: term().
terminate(Reason, _State, _Data = #data{handler = Handler, handler_state = HandlerState}) ->
    ok =
        case erlang:function_exported(Handler, terminate, 2) of
            true ->
                _ = Handler:terminate(Reason, HandlerState),
                ok;
            false ->
                ok
        end,
    ok.

%%%=============================================================================
%%% gen_statem states
%%%=============================================================================

-spec recv
    (enter, State, Data) -> StateEnterResult when
        State :: recv | wait,
        Data :: data(),
        StateEnterResult :: gen_statem:state_enter_result(State, Data);
    (EventType, EventContent, Data) -> HandleEventResult when
        EventType :: gen_statem:event_type(),
        EventContent :: gen_statem_event_content(),
        State :: recv | wait,
        Data :: data(),
        HandleEventResult :: gen_statem:event_handler_result(State, Data).
recv(enter, State, _Data) when State =:= recv orelse State =:= wait ->
    Actions = [{state_timeout, 0, execute}],
    {keep_state_and_data, Actions};
recv(state_timeout, execute, _Data) ->
    Actions = [{next_event, internal, logger_receive}],
    {keep_state_and_data, Actions};
recv(internal, logger_receive, Data0 = #data{logger = Logger, logger_select_handle = undefined}) ->
    case erldist_filter_nif:logger_recv(Logger) of
        {BatchSize, BatchDrop, BatchEvents} when
            is_integer(BatchSize) andalso is_integer(BatchDrop) andalso is_list(BatchEvents)
        ->
            Actions = [{next_event, internal, {handler_batch, BatchSize, BatchDrop, BatchEvents}}],
            {keep_state_and_data, Actions};
        {select, LoggerSelectHandle} ->
            Data1 = Data0#data{logger_select_handle = LoggerSelectHandle},
            {next_state, wait, Data1};
        {error, Reason} ->
            {stop, Reason}
    end;
recv(
    internal,
    {handler_batch, BatchSize, BatchDrop, BatchEvents},
    Data0 = #data{handler = Handler, handler_state = HandlerState0}
) ->
    case Handler:handle_batch(BatchSize, BatchDrop, BatchEvents, HandlerState0) of
        handle_events ->
            Actions = [{next_event, internal, {handler_events, BatchEvents}}],
            {keep_state_and_data, Actions};
        {handle_events, NewBatchEvents} when is_list(NewBatchEvents) ->
            Actions = [{next_event, internal, {handler_events, NewBatchEvents}}],
            {keep_state_and_data, Actions};
        {handle_events, NewBatchEvents, HandlerState1} ->
            Data1 = Data0#data{handler_state = HandlerState1},
            Actions = [{next_event, internal, {handler_events, NewBatchEvents}}],
            {keep_state, Data1, Actions};
        stop ->
            stop;
        {stop, Reason} ->
            {stop, Reason};
        {stop, Reason, HandlerState1} ->
            Data1 = Data0#data{handler_state = HandlerState1},
            {stop, Reason, Data1}
    end;
recv(
    internal,
    {handler_events, BatchEvents0},
    Data0 = #data{handler = Handler, handler_state = HandlerState0}
) ->
    case BatchEvents0 of
        [{Time, {Sysname, Atoms, RawControl, RawPayload}} | BatchEvents1] ->
            Control = erts_debug:dist_ext_to_term(Atoms, RawControl),
            HandleEventResult =
                case RawPayload of
                    undefined ->
                        Handler:handle_control_event(Time, Sysname, Control, HandlerState0);
                    _ ->
                        Payload = erts_debug:dist_ext_to_term(Atoms, RawPayload),
                        Handler:handle_payload_event(Time, Sysname, Control, Payload, HandlerState0)
                end,
            case HandleEventResult of
                cont ->
                    Actions = [{next_event, internal, {handler_events, BatchEvents1}}],
                    {keep_state_and_data, Actions};
                {cont, HandlerState1} ->
                    Data1 = Data0#data{handler_state = HandlerState1},
                    Actions = [{next_event, internal, {handler_events, BatchEvents1}}],
                    {keep_state, Data1, Actions};
                stop ->
                    stop;
                {stop, Reason} ->
                    {stop, Reason};
                {stop, Reason, HandlerState1} ->
                    Data1 = Data0#data{handler_state = HandlerState1},
                    {stop, Reason, Data1}
            end;
        [] ->
            Actions = [{next_event, internal, logger_receive}],
            {keep_state_and_data, Actions}
    end;
recv(info, Info, Data) ->
    handler_info(Info, Data).

-spec wait
    (enter, State, Data) -> StateEnterResult when
        State :: wait,
        Data :: data(),
        StateEnterResult :: gen_statem:state_enter_result(State, Data);
    (EventType, EventContent, Data) -> HandleEventResult when
        EventType :: gen_statem:event_type(),
        EventContent :: gen_statem_event_content(),
        State :: recv | wait,
        Data :: data(),
        HandleEventResult :: gen_statem:event_handler_result(State, Data).
wait(enter, recv, _Data = #data{logger_select_handle = LoggerSelectHandle}) when LoggerSelectHandle =/= undefined ->
    keep_state_and_data;
wait(
    info,
    {'$logger', Logger, select, LoggerSelectHandle},
    Data0 = #data{logger = Logger, logger_select_handle = LoggerSelectHandle}
) ->
    Data1 = Data0#data{logger_select_handle = undefined},
    {next_state, recv, Data1};
wait(info, Info, Data) ->
    handler_info(Info, Data).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-spec handler_info(gen_statem_event_content(), data()) -> gen_statem:event_handler_result(State, Data) when
    State :: recv | wait,
    Data :: data().
handler_info(Info, Data0 = #data{handler = Handler, handler_state = HandlerState0}) ->
    case erlang:function_exported(Handler, handle_info, 2) of
        true ->
            case Handler:handle_info(Info, HandlerState0) of
                cont ->
                    keep_state_and_data;
                {cont, HandlerState1} ->
                    Data1 = Data0#data{handler_state = HandlerState1},
                    {keep_state, Data1};
                stop ->
                    stop;
                {stop, Reason} ->
                    {stop, Reason};
                {stop, Reason, HandlerState1} ->
                    Data1 = Data0#data{handler_state = HandlerState1},
                    {stop, Reason, Data1}
            end;
        false ->
            keep_state_and_data
    end.
