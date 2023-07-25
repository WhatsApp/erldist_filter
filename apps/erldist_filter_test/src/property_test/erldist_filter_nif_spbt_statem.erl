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
%%% Created :  27 Sep 2022 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(erldist_filter_nif_spbt_statem).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(nowarn_missing_spec).

-behaviour(proper_statem).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("erldist_filter/include/erldist_filter.hrl").

%% proper_statem callbacks
-export([
    initial_state/0,
    command/1,
    precondition/2,
    postcondition/3,
    next_state/3
]).
%% Type Generation API
-export([
    gen_channel_open_call/1,
    gen_connection_id/0,
    gen_creation/0,
    gen_distribution_flags/0,
    gen_fragment_size/0,
    gen_packet_size/0,
    gen_reg_name/0,
    % gen_reg_send/2,
    % gen_reg_send/6,
    gen_sysname/0
]).
%% Model API
-export([
    is_channel_sysname/2,
    unwrap_channel_resource/1,
    unwrap_channel_owner/1,
    unwrap_ok/1
]).

%% Macros.
-define(MODEL_CHANNEL, erldist_filter_nif_spbt_model_channel).
-define(SHIM, erldist_filter_nif_spbt_shim).
-define(is_symbol(X),
    (is_tuple(X) andalso
        ((tuple_size(X) =:= 2 andalso element(1, (X)) =:= var) orelse
            (tuple_size(X) =:= 4 andalso element(1, (X)) =:= call)))
).

%% Types
-type channel_resource() :: erldist_filter_nif:channel() | {var, term()} | {call, module(), atom(), [term()]}.
-type symbolic_call_or_var(T) ::
    {'call', module(), atom(), list(T | symbolic_call_or_var(T))} | proper_statem:symbolic_var().
-type symbolic_call_or_var() :: symbolic_call_or_var(term()).
-type t() :: #{
    '__type__' := ?MODULE,
    channels := #{channel_resource() => ?MODEL_CHANNEL:t()}
}.

-export_type([
    channel_resource/0,
    t/0
]).

%%%=============================================================================
%%% proper_statem callbacks
%%%=============================================================================

initial_state() ->
    #{
        '__type__' => ?MODULE,
        channels => maps:new()
    }.

-dialyzer({nowarn_function, command/1}).
-dialyzer({nowarn_function, command_fill_atom_cache/4}).
-dialyzer({nowarn_function, gen_channel_dop_with_payload/2}).
-dialyzer({nowarn_function, gen_channel_dop_without_payload/2}).
-dialyzer({nowarn_function, unwrap_ok/1}).

command(SymbolicState = #{channels := Channels}) when map_size(Channels) =:= 0 ->
    gen_channel_open_call(SymbolicState);
command(SymbolicState = #{channels := Channels}) ->
    ?LET(
        ChannelResource,
        oneof(maps:keys(Channels)),
        begin
            ChannelT = {channel, ChannelResource},
            ModelChannel = maps:get(ChannelResource, Channels),
            {ok, HeaderModes} = ?MODEL_CHANNEL:get_header_modes(ModelChannel),
            frequency(
                lists:append([
                    [
                        {1, gen_channel_open_call(SymbolicState)},
                        {1, {call, ?SHIM, channel_close, [ChannelT]}},
                        {100,
                            {call, ?SHIM, channel_dop_with_payload, [
                                ChannelT, gen_channel_dop_with_payload(SymbolicState, ChannelResource)
                            ]}},
                        {10,
                            {call, ?SHIM, channel_dop_without_payload, [
                                ChannelT, gen_channel_dop_without_payload(SymbolicState, ChannelResource)
                            ]}},
                        {10, {call, ?SHIM, channel_get_rx_atom_cache, [ChannelT]}}
                    ],
                    command_fill_atom_cache(SymbolicState, ModelChannel, HeaderModes, ChannelT)
                ])
            )
        end
    ).

%% @private
command_fill_atom_cache(_SymbolicState, ModelChannel0, HeaderModes, ChannelT) ->
    AtomCacheSupported =
        case HeaderModes of
            #{fragment := _} ->
                {ok, fragment};
            #{normal := _} ->
                {ok, normal};
            _ ->
                error
        end,
    case AtomCacheSupported of
        {ok, HeaderMode} ->
            case ?MODEL_CHANNEL:is_atom_cache_filled(ModelChannel0) of
                false ->
                    Spec0 = #{
                        header_mode => HeaderMode,
                        % use a very large (1GB) fragment size to prevent actual fragmentation
                        fragment_size => 1 * 1024 * 1024 * 1024
                    },
                    GenSpec = ?LAZY(begin
                        {ok, Spec1, _ModelChannel1} = ?MODEL_CHANNEL:maybe_fill_atom_cache(ModelChannel0, Spec0),
                        Spec1
                    end),
                    [
                        {10, {call, ?SHIM, channel_fill_atom_cache, [ChannelT, GenSpec]}}
                    ];
                true ->
                    []
            end;
        error ->
            []
    end.

% %% @private
% command(SymbolicState, ChannelResources) ->
%     ?LET(
%         ChannelResource,
%         oneof(ChannelResources),
%         begin
%             {ok, Modes} = ?MODEL:get_channel_modes(SymbolicState, ChannelResource),
%             GenMode = oneof([ModeKey || {ModeKey, ModeVal} <- maps:to_list(Modes), ModeVal =:= true]),
%             oneof([
%                 {call, ?SHIM, noop, [ChannelResource]},
%                 {call, ?SHIM, reg_send, [ChannelResource, ?LET(Mode, GenMode, gen_reg_send(SymbolicState, ChannelResource, Mode))]}
%             ])
%         end
%     ).

% %% @private
% fragment_commands(SymbolicState, ChannelResource, #{fragment := true}) ->
%     [
%         {call, ?SHIM, fragment_reg_send, [ChannelResource, gen_reg_send(SymbolicState, ChannelResource, fragment)]}
%     ];
% fragment_commands(_SymbolicState, _ChannelResource, _Modes) ->
%     [].

% %% @private
% normal_commands(SymbolicState, ChannelResource, #{normal := true}) ->
%     [
%         {call, ?SHIM, normal_reg_send, [ChannelResource, gen_reg_send(SymbolicState, ChannelResource, normal)]}
%     ];
% normal_commands(_SymbolicState, _ChannelResource, _Modes) ->
%     [].

% %% @private
% pass_through_commands(SymbolicState, ChannelResource, #{pass_through := true}) ->
%     [
%         {call, ?SHIM, pass_through_reg_send, [
%             ChannelResource, gen_reg_send(SymbolicState, ChannelResource, pass_through)
%         ]}
%     ];
% pass_through_commands(_SymbolicState, _ChannelResource, _Modes) ->
%     [].

precondition(_SymbolicState = #{channels := Channels}, SymbolicCall) ->
    case SymbolicCall of
        {call, _, channel_open, _} ->
            true;
        {call, _, Func, [{channel, ChannelResource} | Args]} ->
            case maps:find(ChannelResource, Channels) of
                {ok, ModelChannel} ->
                    ?MODEL_CHANNEL:precondition(ModelChannel, Func, Args);
                error ->
                    false
            end
    end.

% %% @private
% precondition(SymbolicState, SymbolicCall, _ChannelResources) ->
%     % Channels HAVE been opened, check at the model level
%     case SymbolicCall of
%         {call, _, Func, Args = [ChannelResource | _]} ->
%             ?MODEL:precondition(SymbolicCall, ChannelResource, Func, Args)
%     end.

postcondition(_State = #{channels := Channels}, SymbolicCall, Result) ->
    case SymbolicCall of
        {call, _, channel_open, _} when is_pid(Result) orelse is_reference(Result) ->
            true;
        {call, _, Func, [{channel, ChannelResource} | Args]} ->
            ModelChannel = maps:get(ChannelResource, Channels),
            ?MODEL_CHANNEL:postcondition(ModelChannel, Func, Args, Result)
    end.

% postcondition(State, SymbolicCall = {call, _, Func, Args}, Result) ->
%     case ?MODEL:categorize_call(State, Func, Args) of
%         {ok, dynamic} ->
%             {_NewState, _ExpectedResult} = ?MODEL:dynamic_call(State, Func, Args, Result),
%             true;
%         {ok, symbolic} ->
%             {_NewState, ExpectedResult} = ?MODEL:symbolic_call(State, Func, Args),
%             case Result =:= ExpectedResult of
%                 true ->
%                     true;
%                 false ->
%                     ct:log(
%                         error,
%                         ?MAX_IMPORTANCE,
%                         "Error: postcondition failure in ~w:~s/~w~nState = ~tp~nSymbolicCall = ~tp~nResult = ~tp~nExpectedResult = ~tp~n",
%                         [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, State, SymbolicCall, Result, ExpectedResult]
%                     ),
%                     false
%             end
%     end.

next_state(State0 = #{channels := Channels0}, Result, SymbolicCall) ->
    case SymbolicCall of
        {call, _, channel_open, [PacketSize, Sysname, Creation, ConnectionId, DistributionFlags]} ->
            % ChannelResource = unwrap_channel_resource(Result),
            % Owner = unwrap_channel_owner(Result),
            ChannelResource = Result,
            ModelChannel = ?MODEL_CHANNEL:open(
                ChannelResource, PacketSize, Sysname, Creation, ConnectionId, DistributionFlags
            ),
            Channels1 = maps:put(ChannelResource, ModelChannel, Channels0),
            State1 = State0#{channels := Channels1},
            State1;
        {call, _, channel_close, [{channel, ChannelResource}]} ->
            Channels1 = maps:remove(ChannelResource, Channels0),
            State1 = State0#{channels := Channels1},
            State1;
        {call, _, Func, [{channel, ChannelResource} | Args]} ->
            ModelChannel0 = maps:get(ChannelResource, Channels0),
            ModelChannel1 = ?MODEL_CHANNEL:next_state(ModelChannel0, Func, Args, Result),
            Channels1 = maps:put(ChannelResource, ModelChannel1, Channels0),
            State1 = State0#{channels := Channels1},
            State1
    end.

%%%=============================================================================
%%% Type Generation API Functions
%%%=============================================================================

gen_channel_open_call(SymbolicState) ->
    PacketSize = gen_packet_size(),
    Sysname = gen_sysname(SymbolicState),
    Creation = gen_creation(),
    ConnectionId = gen_connection_id(),
    DistributionFlags = gen_distribution_flags(),
    return({call, ?SHIM, channel_open, [PacketSize, Sysname, Creation, ConnectionId, DistributionFlags]}).

gen_connection_id() ->
    range(0, 16#ffffffff).

gen_creation() ->
    range(0, 16#ffffffff).

gen_distribution_flags() ->
    proper_vdist:vdist_distribution_flags().
% #{
%     'DFLAG_DIST_DEFAULT' := DFLAG_DIST_DEFAULT,
%     'DFLAG_DIST_HDR_ATOM_CACHE' := DFLAG_DIST_HDR_ATOM_CACHE,
%     'DFLAG_FRAGMENTS' := DFLAG_FRAGMENTS,
%     'DFLAG_DIST_MANDATORY' := DFLAG_DIST_MANDATORY
% } = erldist_filter_nif:distribution_flags(),
% frequency([
%     {1, DFLAG_DIST_MANDATORY},
%     {1, DFLAG_DIST_DEFAULT},
%     % No fragment header support
%     {1, DFLAG_DIST_DEFAULT band (bnot DFLAG_FRAGMENTS)},
%     % No normal header support
%     {1, DFLAG_DIST_DEFAULT band (bnot DFLAG_DIST_HDR_ATOM_CACHE)}
% ]).

gen_fragment_size() ->
    integer(1, 16#FF).

gen_packet_size() ->
    % NOTE: don't test PacketSize=0, PacketSize=1, or PacketSize=2 for now
    % oneof([0, 1, 2, 4, 8]).
    oneof([4, 8]).

gen_reg_name() ->
    ?SUCHTHAT(
        RegName,
        atom(),
        RegName =/= '' andalso RegName =/= nil andalso RegName =/= undefined
    ).

gen_channel_dop_with_payload(_SymbolicState = #{channels := Channels}, ChannelResource) ->
    ModelChannel0 = maps:get(ChannelResource, Channels),
    {ok, HeaderModes0} = ?MODEL_CHANNEL:get_header_modes(ModelChannel0),
    HeaderModes = maps:keys(HeaderModes0),
    ?LET(
        {ControlMessage, Payload, HeaderMode, FragmentSize},
        ?LET(
            ControlMessage,
            proper_vdist:vdist_any_dop_with_payload(),
            {
                ControlMessage,
                proper_vdist:vdist_payload(ControlMessage, #{large_binaries => true}),
                oneof(HeaderModes),
                gen_fragment_size()
            }
        ),
        begin
            Spec0 = #{
                header_mode => HeaderMode,
                fragment_size => FragmentSize,
                control_message => ControlMessage,
                payload => Payload
            },
            {ok, Spec1, _ModelChannel1} = ?MODEL_CHANNEL:channel_send_packets(ModelChannel0, Spec0),
            Spec1
        end
    ).

gen_channel_dop_without_payload(_SymbolicState = #{channels := Channels}, ChannelResource) ->
    ModelChannel0 = maps:get(ChannelResource, Channels),
    {ok, HeaderModes0} = ?MODEL_CHANNEL:get_header_modes(ModelChannel0),
    HeaderModes = maps:keys(HeaderModes0),
    ?LET(
        {ControlMessage, HeaderMode, FragmentSize},
        {proper_vdist:vdist_any_dop_without_payload(), oneof(HeaderModes), gen_fragment_size()},
        begin
            Spec0 = #{
                header_mode => HeaderMode,
                fragment_size => FragmentSize,
                control_message => ControlMessage
            },
            {ok, Spec1, _ModelChannel1} = ?MODEL_CHANNEL:channel_send_packets(ModelChannel0, Spec0),
            Spec1
        end
    ).

gen_sysname() ->
    ?LET(
        Name,
        ?SUCHTHAT(
            N,
            list(range($a, $z)),
            N =/= "" andalso N =/= "nil" andalso N =/= "undefined"
        ),
        list_to_atom(Name ++ "@127.0.0.1")
    ).

gen_sysname(SymbolicState) ->
    ?SUCHTHAT(
        Sysname,
        gen_sysname(),
        not is_channel_sysname(SymbolicState, Sysname)
    ).

%%%=============================================================================
%%% Model API Functions
%%%=============================================================================

-spec is_channel_sysname(ModelSystem, Sysname) -> boolean() when
    ModelSystem :: t(),
    Sysname :: erldist_filter_nif:sysname().
is_channel_sysname(#{channels := ModelChannels}, Sysname) ->
    Predicate = fun(ModelChannel) ->
        ?MODEL_CHANNEL:is_sysname(ModelChannel, Sysname)
    end,
    lists:any(Predicate, maps:values(ModelChannels)).

unwrap_channel_resource({Owner, ChannelResource}) when is_pid(Owner) andalso is_reference(ChannelResource) ->
    ChannelResource;
unwrap_channel_resource(Symbol) ->
    maybe_unwrap(Symbol, ?MODULE, ?FUNCTION_NAME).

unwrap_channel_owner({Owner, ChannelResource}) when is_pid(Owner) andalso is_reference(ChannelResource) ->
    Owner;
unwrap_channel_owner(Symbol) ->
    maybe_unwrap(Symbol, ?MODULE, ?FUNCTION_NAME).

maybe_unwrap(Var = {var, _}, Module, Function) ->
    {call, Module, Function, [Var]};
maybe_unwrap(Call = {call, Module, Function, [_Symbol]}, Module, Function) ->
    Call;
maybe_unwrap(OtherCall = {call, _, _, _}, Module, Function) ->
    {call, Module, Function, [OtherCall]}.

-spec unwrap_ok
    ({ok, Result}) -> Result when Result :: term();
    (AlreadyWrappedCall) -> AlreadyWrappedCall when
        AlreadyWrappedCall :: {call, ?MODULE, unwrap_ok, list(Result | symbolic_call_or_var(Result))}, Result :: term();
    (SymbolicCallOrVar) -> {call, ?MODULE, unwrap_ok, list(Result | SymbolicCallOrVar)} when
        SymbolicCallOrVar :: symbolic_call_or_var(Result), Result :: term().
unwrap_ok({ok, Result}) ->
    Result;
unwrap_ok(AlreadyWrappedCall = {call, ?MODULE, unwrap_ok, [_]}) ->
    AlreadyWrappedCall;
unwrap_ok(Call = {call, _, _, _}) ->
    {call, ?MODULE, unwrap_ok, [Call]};
unwrap_ok(Var = {var, _}) ->
    {call, ?MODULE, unwrap_ok, [Var]}.
