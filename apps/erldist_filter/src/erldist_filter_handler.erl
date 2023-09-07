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
-module(erldist_filter_handler).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("erldist_filter/include/udist.hrl").

-behaviour(gen_statem).

%% OTP callbacks
-export([
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
    handle/3
]).

%% Records
-record(init_data, {
    router_number :: erldist_filter_router:router_number(),
    sysname :: node()
}).
-record(data, {
    router_number :: erldist_filter_router:router_number(),
    sysname :: node(),
    last_seen = -1 :: integer()
}).

%% Internal Types
-type init_data() :: #init_data{}.
-type data() :: #data{}.
-type gen_statem_event_content() :: eqwalizer:dynamic().
-type stop_reason() :: normal | shutdown | {shutdown, term()} | term() | eqwalizer:dynamic().

%% Types
-type action() :: drop | keep.
-type control_without_payload() :: udist:dop_without_payload_t().
-type control_with_payload() :: udist:dop_with_payload_t().
-type decision() :: action() | {action(), control_without_payload()} | {action(), control_with_payload(), payload()}.
-type hint() :: drop | safe | unsafe.
-type payload() :: term().

% %% Callbacks
-callback classify(Hint :: hint(), Sysname :: node(), Control :: control_without_payload()) -> decision().
-callback classify(Hint :: hint(), Sysname :: node(), Control :: control_with_payload(), Payload :: payload()) ->
    decision().
-callback spawn_request_init(Sysname :: node(), Module :: module(), FunctionName :: atom(), Arguments :: [payload()]) ->
    term() | no_return().

%% Macros
-define(hint_drop_control(), handler_classify(Handler, drop, Sysname, Control)).
% -define(hint_safe_control(), handler_classify(Handler, safe, Sysname, Control)).
% -define(hint_unsafe_control(), handler_classify(Handler, unsafe, Sysname, Control)).
-define(hint_drop_payload(), handler_classify(Handler, drop, Sysname, Control, Payload)).
-define(hint_safe_payload(), handler_classify(Handler, safe, Sysname, Control, Payload)).
-define(hint_unsafe_payload(), handler_classify(Handler, unsafe, Sysname, Control, Payload)).
-define(is_router_number(X), (is_integer(X) andalso (X) >= 1)).

%%%=============================================================================
%%% OTP callbacks
%%%=============================================================================

-spec child_spec(RouterNumber) -> supervisor:child_spec() when
    RouterNumber :: erldist_filter_router:router_number().
child_spec(RouterNumber) when ?is_router_number(RouterNumber) ->
    #{
        id => undefined,
        start => {?MODULE, start_link, [RouterNumber]},
        restart => temporary,
        shutdown => brutal_kill,
        type => worker,
        modules => [?MODULE]
    }.

-spec start_link(RouterNumber, Sysname) -> gen_statem:start_ret() when
    RouterNumber :: erldist_filter_router:router_number(),
    Sysname :: node().
start_link(RouterNumber, Sysname) when ?is_router_number(RouterNumber) andalso is_atom(Sysname) ->
    InitData = #init_data{
        router_number = RouterNumber,
        sysname = Sysname
    },
    gen_statem:start_link(?MODULE, InitData, [{hibernate_after, 5000}]).

%%%=============================================================================
%%% gen_statem callbacks
%%%=============================================================================

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    [state_functions].

-spec init(InitData) -> InitResult when
    InitData :: init_data(),
    State :: handle,
    Data :: data(),
    InitResult :: gen_statem:init_result(State, Data).
init(#init_data{
    router_number = RouterNumber,
    sysname = Sysname
}) ->
    Data = #data{
        router_number = RouterNumber,
        sysname = Sysname
    },
    {ok, handle, Data}.

-spec terminate(Reason, State, Data) -> Ignored when
    Reason :: stop_reason(), State :: handle, Data :: data(), Ignored :: term().
terminate(_Reason, _State, _Data = #data{}) ->
    ok.

%%%=============================================================================
%%% gen_statem states
%%%=============================================================================

-spec handle(EventType, EventContent, Data) -> HandleEventResult when
    EventType :: gen_statem:event_type(),
    EventContent :: gen_statem_event_content(),
    State :: handle,
    Data :: data(),
    HandleEventResult :: gen_statem:event_handler_result(State, Data).
handle(info, {Sysname, Sort, Control0}, Data0 = #data{sysname = Sysname, last_seen = LastSeen}) when
    LastSeen < Sort
->
    Control = udist:cast_to_dop(Control0),
    {_Hint, Decision} = classify(erldist_filter:handler_get(), Sysname, Control),
    ok = execute(Decision),
    Data1 = Data0#data{last_seen = Sort},
    {keep_state, Data1};
handle(
    info, {Sysname, Sort, Control0, Payload}, Data0 = #data{sysname = Sysname, last_seen = LastSeen}
) when LastSeen < Sort ->
    Control = udist:cast_to_dop(Control0),
    {_Hint, Decision} = classify(erldist_filter:handler_get(), Sysname, Control, Payload),
    ok = execute(Decision),
    Data1 = Data0#data{last_seen = Sort},
    {keep_state, Data1};
handle(EventType = info, EventContent = {Sysname, _Sort, _Control}, Data0 = #data{sysname = Sysname}) ->
    % Sort has been reset, which means the dist connection for this Sysname has been reset.
    _ = catch erlang:garbage_collect(self()),
    Data1 = Data0#data{last_seen = -1},
    Actions = [{next_event, EventType, EventContent}],
    {keep_state, Data1, Actions};
handle(
    EventType = info, EventContent = {Sysname, _Sort, _Control, _Payload}, Data0 = #data{sysname = Sysname}
) ->
    % Sort has been reset, which means the dist connection for this Sysname has been reset.
    _ = catch erlang:garbage_collect(self()),
    Data1 = Data0#data{last_seen = -1},
    Actions = [{next_event, EventType, EventContent}],
    {keep_state, Data1, Actions}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
classify(Handler, Sysname, Control) ->
    case udist:get_dop_group(Control) of
        exit2 ->
            ?hint_drop_control();
        group_leader ->
            ?hint_drop_control()
        %% Unreachable, these Control operations are handled by `Handler:spawn_request_init/4'.
        % spawn_request ->
        %% Unreachable, these Control operations always have a Payload.
        % send_to_alias ->
        % send_to_name ->
        % send_to_pid ->
        %% Unreachable, these Control operations are only dropped and/or logged.
        % exit ->
        % link ->
        % unlink ->
        %% Unreachable, these Control operations are only allowed and/or logged.
        % monitor_related ->
        % spawn_reply ->
    end.

%% @private
classify(Handler, Sysname, Control, Payload) when
    is_atom(Handler) andalso is_atom(Sysname) andalso ?is_udist_dop_with_payload_t(Control)
->
    case udist:get_dop_group(Control) of
        exit2 ->
            ?hint_drop_payload();
        send_to_alias ->
            classify_send(Handler, Sysname, Control, Payload);
        send_to_name ->
            Name =
                case Control of
                    #udist_dop_reg_send{to_name = ToName} ->
                        ToName;
                    #udist_dop_reg_send_tt{to_name = ToName} ->
                        ToName
                end,
            case Name of
                net_kernel ->
                    classify_send_to_net_kernel(Handler, Sysname, Control, Payload);
                rex ->
                    classify_send_to_rex(Handler, Sysname, Control, Payload);
                _ ->
                    classify_send(Handler, Sysname, Control, Payload)
            end;
        send_to_pid ->
            classify_send(Handler, Sysname, Control, Payload)
        %% Unreachable, these Control operations are handled by `spawn_request_init/4'.
        % spawn_request ->
        %% Unreachable, these Control operations never have a Payload.
        % group_leader ->
        %% Unreachable, these Control operations are only dropped and/or logged.
        % exit ->
        % link ->
        % unlink ->
        %% Unreachable, these Control operations are only allowed and/or logged.
        % monitor_related ->
        % spawn_reply ->
    end.

%% @private
classify_send(Handler, Sysname, Control, Payload) ->
    case Payload of
        {system, _From, _Request} ->
            ?hint_drop_payload();
        {'EXIT', _Pid, _Reason} ->
            ?hint_drop_payload();
        {'$gen_cast', Request} ->
            case Request of
                {try_again_restart, _TryAgainId} ->
                    ?hint_drop_payload();
                _ ->
                    ?hint_unsafe_payload()
            end;
        {'$gen_call', _From, Request} ->
            case Request of
                {start_child, _ChildSpec} ->
                    ?hint_drop_payload();
                {terminate_child, _ChildId} ->
                    ?hint_drop_payload();
                {restart_child, _ChildId} ->
                    ?hint_drop_payload();
                {delete_child, _ChildId} ->
                    ?hint_drop_payload();
                _ ->
                    ?hint_unsafe_payload()
            end;
        {io_request, _From, _ReplyAs, _Request} ->
            ?hint_drop_payload();
        {io_reply, _ReplyAs, _Reply} ->
            ?hint_drop_payload();
        _ ->
            ?hint_unsafe_payload()
    end.

%% @private
classify_send_to_net_kernel(Handler, Sysname, Control, Payload) ->
    case Payload of
        {'$gen_call', _From, {is_auth, Sysname}} ->
            ?hint_safe_payload();
        _ ->
            ?hint_drop_payload()
    end.

%% @private
classify_send_to_rex(Handler, Sysname, Control, Payload) ->
    case Payload of
        {_From, features_request} ->
            ?hint_safe_payload();
        {features_reply, Sysname, Features} when is_list(Features) ->
            ?hint_safe_payload();
        _ ->
            ?hint_drop_payload()
    end.

%% @private
handler_classify(undefined, Hint, _Sysname, Control) ->
    case Hint of
        drop -> {Hint, {drop, Control}};
        safe -> {Hint, {keep, Control}};
        unsafe -> {Hint, {keep, Control}}
    end;
handler_classify(Handler, Hint, Sysname, Control) when is_atom(Handler) ->
    case Handler:classify(Hint, Sysname, Control) of
        keep ->
            {Hint, {keep, Control}};
        {keep, NewControl} when ?is_udist_dop_without_payload_t(NewControl) ->
            {Hint, {keep, NewControl}};
        drop ->
            {Hint, {drop, Control}}
    end.

%% @private
handler_classify(undefined, Hint, _Sysname, Control, Payload) ->
    case Hint of
        drop -> {Hint, {drop, Control, Payload}};
        safe -> {Hint, {keep, Control, Payload}};
        unsafe -> {Hint, {keep, Control, Payload}}
    end;
handler_classify(Handler, Hint, Sysname, Control, Payload) when is_atom(Handler) ->
    case Handler:classify(Hint, Sysname, Control, Payload) of
        keep ->
            {Hint, {keep, Control, Payload}};
        {keep, NewControl, NewPayload} when ?is_udist_dop_with_payload_t(NewControl) ->
            {Hint, {keep, NewControl, NewPayload}};
        drop ->
            {Hint, {drop, Control, Payload}}
    end.

%% @private
execute({drop, _Control}) ->
    ok;
execute({drop, _Control, _Payload}) ->
    ok;
execute({keep, Control}) ->
    case Control of
        #udist_dop_exit2{to_pid = ToPid, reason = Reason} when node(ToPid) =:= node() ->
            _ = catch erlang:exit(ToPid, Reason),
            ok;
        #udist_dop_exit2_tt{to_pid = ToPid, reason = Reason} when node(ToPid) =:= node() ->
            _ = catch erlang:exit(ToPid, Reason),
            ok;
        #udist_dop_group_leader{from_pid = FromPid, to_pid = ToPid} when node(ToPid) =:= node() ->
            _ = catch erlang:group_leader(FromPid, ToPid),
            ok;
        _ when ?is_udist_dop_without_payload_t(Control) ->
            % Invalid message: drop
            ok
    end;
execute({keep, Control, Payload}) ->
    case Control of
        #udist_dop_alias_send{alias = Alias} when node(Alias) =:= node() ->
            _ = catch erlang:send(Alias, Payload, [noconnect]),
            ok;
        #udist_dop_alias_send_tt{alias = Alias} when node(Alias) =:= node() ->
            _ = catch erlang:send(Alias, Payload, [noconnect]),
            ok;
        #udist_dop_payload_exit2{to_pid = ToPid} when node(ToPid) =:= node() ->
            Reason = Payload,
            _ = catch erlang:exit(ToPid, Reason),
            ok;
        #udist_dop_payload_exit2_tt{to_pid = ToPid} when node(ToPid) =:= node() ->
            Reason = Payload,
            _ = catch erlang:exit(ToPid, Reason),
            ok;
        #udist_dop_reg_send{to_name = ToName} ->
            _ = catch erlang:send(ToName, Payload, [noconnect]),
            ok;
        #udist_dop_reg_send_tt{to_name = ToName} ->
            _ = catch erlang:send(ToName, Payload, [noconnect]),
            ok;
        #udist_dop_send{to_pid = ToPid} when node(ToPid) =:= node() ->
            _ = catch erlang:send(ToPid, Payload, [noconnect]),
            ok;
        #udist_dop_send_tt{to_pid = ToPid} when node(ToPid) =:= node() ->
            _ = catch erlang:send(ToPid, Payload, [noconnect]),
            ok;
        #udist_dop_send_sender{to_pid = ToPid} when node(ToPid) =:= node() ->
            _ = catch erlang:send(ToPid, Payload, [noconnect]),
            ok;
        #udist_dop_send_sender_tt{to_pid = ToPid} when node(ToPid) =:= node() ->
            _ = catch erlang:send(ToPid, Payload, [noconnect]),
            ok;
        _ when ?is_udist_dop_with_payload_t(Control) ->
            % Invalid message: drop
            ok
    end.
