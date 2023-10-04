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
-compile(warn_missing_spec).

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
    gen_channel_dop_with_payload/2,
    gen_channel_dop_without_payload/2,
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
-define(is_connection_id(X), (is_integer(X) andalso X >= 0 andalso X =< 16#ffffffff)).
-define(is_creation(X), (is_integer(X) andalso X >= 0 andalso X =< 16#ffffffff)).
-define(is_distribution_flags(X), (is_integer(X) andalso X >= 0 andalso X =< 16#ffffffffffffffff)).
-define(is_packet_size(X), (X =:= 0 orelse X =:= 1 orelse X =:= 2 orelse X =:= 4 orelse X =:= 8)).
-define(is_symbol(X),
    (is_tuple(X) andalso
        ((tuple_size(X) =:= 2 andalso element(1, (X)) =:= var) orelse
            (tuple_size(X) =:= 4 andalso element(1, (X)) =:= call)))
).

%% Types
-type channel_resource() :: erldist_filter_nif:channel() | eqwalizer:dynamic().
-type t() :: #{
    '__type__' := ?MODULE,
    channels := #{channel_resource() => ?MODEL_CHANNEL:t()}
}.
-type symbolic_state() :: t().
-type dynamic_state() :: t().

-export_type([
    channel_resource/0,
    t/0,
    symbolic_state/0,
    dynamic_state/0
]).

%%%=============================================================================
%%% proper_statem callbacks
%%%=============================================================================

-spec initial_state() -> symbolic_state().
initial_state() ->
    #{
        '__type__' => ?MODULE,
        channels => maps:new()
    }.

-spec command(symbolic_state()) -> proper_types:type().
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

-spec precondition(SymbolicState, SymbolicCall) -> boolean() when
    SymbolicState :: dynamic_state(),
    SymbolicCall :: proper_statem:symbolic_call().
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

-spec postcondition(DynamicState, SymbolicCall, Result) -> boolean() when
    DynamicState :: dynamic_state(),
    SymbolicCall :: proper_statem:symbolic_call(),
    Result :: eqwalizer:dynamic().
postcondition(_State = #{channels := Channels}, SymbolicCall, Result) ->
    case SymbolicCall of
        {call, _, channel_open, _} when is_pid(Result) orelse is_reference(Result) ->
            true;
        {call, _, Func, [{channel, ChannelResource} | Args]} ->
            ModelChannel = maps:get(ChannelResource, Channels),
            ?MODEL_CHANNEL:postcondition(ModelChannel, Func, Args, Result)
    end.

-spec next_state(State, Result, SymbolicCall) -> State when
    State :: symbolic_state() | dynamic_state(),
    Result :: eqwalizer:dynamic(),
    SymbolicCall :: proper_statem:symbolic_call().
next_state(State0 = #{'__type__' := ?MODULE, channels := Channels0}, Result, SymbolicCall) ->
    case SymbolicCall of
        {call, _, channel_open, [PacketSize, Sysname, Creation, ConnectionId, DistributionFlags]} when
            ?is_packet_size(PacketSize) andalso is_atom(Sysname) andalso ?is_creation(Creation) andalso
                ?is_connection_id(ConnectionId) andalso ?is_distribution_flags(DistributionFlags)
        ->
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

-spec gen_channel_open_call(symbolic_state()) -> proper_types:type().
gen_channel_open_call(SymbolicState) ->
    PacketSize = gen_packet_size(),
    Sysname = gen_sysname(SymbolicState),
    Creation = gen_creation(),
    ConnectionId = gen_connection_id(),
    DistributionFlags = gen_distribution_flags(),
    return({call, ?SHIM, channel_open, [PacketSize, Sysname, Creation, ConnectionId, DistributionFlags]}).

-spec gen_connection_id() -> proper_types:type().
gen_connection_id() ->
    range(0, 16#ffffffff).

-spec gen_creation() -> proper_types:type().
gen_creation() ->
    range(0, 16#ffffffff).

-spec gen_distribution_flags() -> proper_types:type().
gen_distribution_flags() ->
    proper_vdist:vdist_distribution_flags().

-spec gen_fragment_size() -> proper_types:type().
gen_fragment_size() ->
    integer(1, 16#FF).

-spec gen_packet_size() -> proper_types:type().
gen_packet_size() ->
    % NOTE: don't test PacketSize=0, PacketSize=1, or PacketSize=2 for now
    % oneof([0, 1, 2, 4, 8]).
    oneof([4, 8]).

-spec gen_reg_name() -> proper_types:type().
gen_reg_name() ->
    ?SUCHTHAT(
        RegName,
        atom(),
        RegName =/= '' andalso RegName =/= nil andalso RegName =/= undefined
    ).

-spec gen_channel_dop_with_payload(symbolic_state(), channel_resource()) -> proper_types:type().
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

-spec gen_channel_dop_without_payload(symbolic_state(), channel_resource()) -> proper_types:type().
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

-spec gen_sysname() -> proper_types:type().
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

-spec gen_sysname(symbolic_state()) -> proper_types:type().
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

-spec unwrap_channel_resource({pid(), reference()} | eqwalizer:dynamic()) -> reference() | eqwalizer:dynamic().
unwrap_channel_resource({Owner, ChannelResource}) when is_pid(Owner) andalso is_reference(ChannelResource) ->
    ChannelResource;
unwrap_channel_resource(Symbol) ->
    maybe_unwrap(Symbol, ?MODULE, ?FUNCTION_NAME).

-spec unwrap_channel_owner({pid(), reference()} | eqwalizer:dynamic()) -> pid() | eqwalizer:dynamic().
unwrap_channel_owner({Owner, ChannelResource}) when is_pid(Owner) andalso is_reference(ChannelResource) ->
    Owner;
unwrap_channel_owner(Symbol) ->
    maybe_unwrap(Symbol, ?MODULE, ?FUNCTION_NAME).

%% @private
maybe_unwrap(Var = {var, _}, Module, Function) ->
    {call, Module, Function, [Var]};
maybe_unwrap(Call = {call, Module, Function, [_Symbol]}, Module, Function) ->
    Call;
maybe_unwrap(OtherCall = {call, _, _, _}, Module, Function) ->
    {call, Module, Function, [OtherCall]}.

-spec unwrap_ok({ok, Result} | eqwalizer:dynamic()) -> Result | eqwalizer:dynamic().
unwrap_ok({ok, Result}) ->
    Result;
unwrap_ok(AlreadyWrappedCall = {call, ?MODULE, unwrap_ok, [_]}) ->
    AlreadyWrappedCall;
unwrap_ok(Call = {call, _, _, _}) ->
    {call, ?MODULE, unwrap_ok, [Call]};
unwrap_ok(Var = {var, _}) ->
    {call, ?MODULE, unwrap_ok, [Var]}.
