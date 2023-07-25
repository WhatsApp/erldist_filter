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
%%% Created :  29 Sep 2022 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(erldist_filter_nif_spbt_model_channel).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(nowarn_missing_spec).

-include_lib("erldist_filter/include/erldist_filter.hrl").

%% StateM API
-export([
    % initial_state/0,
    % initial_state/1,
    precondition/3,
    postcondition/4,
    next_state/4
]).
%% Model API
-export([
    open/6,
    get_header_modes/1,
    is_sysname/2,
    channel_recv_packets/2,
    channel_send_packets/2,
    is_atom_cache_filled/1,
    maybe_fill_atom_cache/2,
    cast_actions_to_fragments/2,
    cast_actions_to_packets/2,
    cast_fragments_to_packets/2,
    cast_packets_to_actions/2,
    cast_packets_to_fragments/2
]).

%% Macros
-define(MODEL_SYSTEM, erldist_filter_nif_spbt_statem).

%% Types
-type dop_spec() :: #{
    header_mode := fragment | normal | pass_through,
    fragment_size := pos_integer(),
    control_message := vdist:control_message(),
    payload => vterm:t(),
    packets => [binary()]
}.
-type header_modes() :: #{
    fragment => [],
    normal => [],
    pass_through => []
}.
-type t() :: #{
    '__type__' := ?MODULE,
    channel := ?MODEL_SYSTEM:channel_resource(),
    entry := vdist_entry:t(),
    packet_size := erldist_filter_nif:packet_size(),
    sysname := erldist_filter_nif:sysname(),
    creation := erldist_filter_nif:creation(),
    connection_id := erldist_filter_nif:connection_id(),
    distribution_flags := erldist_filter_nif:distribution_flags()
}.

-export_type([
    dop_spec/0,
    header_modes/0,
    t/0
]).

%%%=============================================================================
%%% StateM API functions
%%%=============================================================================

% -spec initial_state() -> Model when
%     Model :: t().
% initial_state() ->
%     #{
%         '__type__' => ?MODULE,
%         channel => nil,
%         entry => nil,
%         packet_size => nil,
%         sysname => nil,
%         creation => nil,
%         connection_id => nil,
%         distribution_flags => nil
%     }.

% -spec initial_state([{atom(), term()}] | #{atom() => term()}) -> Model when
%     Model :: t().
% initial_state(List) when is_list(List) ->
%     initial_state(maps:from_list(List));
% initial_state(Map) when is_map(Map) ->
%     maps:fold(fun maps:update/3, initial_state(), Map).

precondition(Model = #{'__type__' := ?MODULE}, Func, Args) ->
    case {Func, Args} of
        {channel_close, []} ->
            true;
        {channel_dop_with_payload, [Spec]} ->
            case channel_send_packets(Model, maps:without([packets], Spec)) of
                {ok, Spec, _NewModel} ->
                    true;
                {ok, _BadSpec, _NewModel} ->
                    false
            end;
        {channel_dop_without_payload, [Spec]} ->
            case channel_send_packets(Model, maps:without([packets], Spec)) of
                {ok, Spec, _NewModel} ->
                    true;
                {ok, _BadSpec, _NewModel} ->
                    false
            end;
        {channel_fill_atom_cache, [Spec]} ->
            case maybe_fill_atom_cache(Model, maps:without([packets], Spec)) of
                {ok, Spec, _NewModel} ->
                    true;
                {ok, _BadSpec, _NewModel} ->
                    false;
                {error, atom_cache_already_filled} ->
                    false
            end;
        {channel_get_rx_atom_cache, []} ->
            true
    end.

postcondition(Model = #{'__type__' := ?MODULE, entry := Entry}, Func, Args, Result) ->
    case {Func, Args} of
        {channel_close, []} ->
            Result =:= ok;
        {channel_dop_with_payload, [_Spec = #{packets := Packets}]} ->
            {ok, #{actions := Actions, events := Events}, _NewModel} = channel_recv_packets(Model, Packets),
            % Actions = packets_to_actions(Model, Packets),
            ResultActions = maybe_drop_noop_actions(Model, Result),
            case ResultActions =:= Actions of
                true ->
                    true;
                false ->
                    ResultPackets = cast_actions_to_packets(Model, Result),
                    io:format(user, "ResultPackets =~n~p~n", [ResultPackets]),
                    io:format(user, "Packets =~n~p~n", [Packets]),
                    {ok, #{events := ResultEvents}, _} = channel_recv_packets(Model, ResultPackets),
                    io:format(user, "Events =~n~p~nResultEvents =~n~p~n~n", [Events, ResultEvents]),
                    false
            end;
        {channel_dop_without_payload, [_Spec = #{packets := Packets}]} ->
            {ok, #{actions := Actions}, _NewModel} = channel_recv_packets(Model, Packets),
            % Actions = packets_to_actions(Model, Packets),
            ResultActions = maybe_drop_noop_actions(Model, Result),
            ResultActions =:= Actions;
        {channel_fill_atom_cache, [_Spec = #{packets := Packets}]} ->
            {ok, #{actions := Actions}, _NewModel} = channel_recv_packets(Model, Packets),
            Result =:= Actions;
        {channel_get_rx_atom_cache, []} ->
            RxAtomCache =
                case Entry of
                    #vdist_entry{rx_atom_cache = undefined} ->
                        undefined;
                    #vdist_entry{rx_atom_cache = #vdist_atom_cache{entries = RxAtomCacheEntries}} ->
                        RxAtomCacheEntries
                end,
            Result =:= RxAtomCache
    end.

next_state(Model0 = #{'__type__' := ?MODULE}, Func, Args, _Result) ->
    case {Func, Args} of
        {channel_dop_with_payload, [Spec = #{packets := Packets}]} ->
            {ok, _Spec, Model1} = channel_send_packets(Model0, maps:without([packets], Spec)),
            {ok, _Info, Model2} = channel_recv_packets(Model1, Packets),
            Model2;
        {channel_dop_without_payload, [Spec = #{packets := Packets}]} ->
            {ok, _Spec, Model1} = channel_send_packets(Model0, maps:without([packets], Spec)),
            {ok, _Info, Model2} = channel_recv_packets(Model1, Packets),
            Model2;
        {channel_fill_atom_cache, [Spec = #{packets := Packets}]} ->
            {ok, _Spec, Model1} = maybe_fill_atom_cache(Model0, maps:without([packets], Spec)),
            {ok, _Info, Model2} = channel_recv_packets(Model1, Packets),
            Model2;
        {channel_get_rx_atom_cache, []} ->
            Model0
    end.

%%%=============================================================================
%%% Model API functions
%%%=============================================================================

-spec open(ChannelResource, PacketSize, Sysname, Creation, ConnectionId, DistributionFlags) -> Model when
    ChannelResource :: ?MODEL_SYSTEM:channel_resource(),
    PacketSize :: erldist_filter_nif:packet_size(),
    Sysname :: erldist_filter_nif:sysname(),
    Creation :: erldist_filter_nif:creation(),
    ConnectionId :: erldist_filter_nif:connection_id(),
    DistributionFlags :: erldist_filter_nif:distribution_flags(),
    Model :: t().
open(ChannelResource, PacketSize, Sysname, Creation, ConnectionId, DistributionFlags) ->
    Entry = vdist_entry:new(DistributionFlags),
    #{
        '__type__' => ?MODULE,
        channel => ChannelResource,
        entry => Entry,
        packet_size => PacketSize,
        sysname => Sysname,
        creation => Creation,
        connection_id => ConnectionId,
        distribution_flags => DistributionFlags
    }.

-spec get_header_modes(Model) -> {ok, HeaderModes} when
    Model :: t(),
    HeaderModes :: header_modes().
get_header_modes(#{'__type__' := ?MODULE, distribution_flags := DFlags}) ->
    #{
        'DFLAG_DIST_HDR_ATOM_CACHE' := DFLAG_DIST_HDR_ATOM_CACHE,
        'DFLAG_FRAGMENTS' := DFLAG_FRAGMENTS
    } = erldist_filter_nif:distribution_flags(),
    HM0 = #{},
    HM1 =
        case (DFlags band DFLAG_FRAGMENTS) =/= 0 of
            true -> HM0#{fragment => []};
            false -> HM0
        end,
    HM2 =
        case (DFlags band DFLAG_DIST_HDR_ATOM_CACHE) =/= 0 of
            true -> HM1#{normal => []};
            false -> HM1
        end,
    HM3 =
        case (DFlags band (DFLAG_DIST_HDR_ATOM_CACHE bor DFLAG_FRAGMENTS)) =:= 0 of
            true -> HM2#{pass_through => []};
            false -> HM2
        end,
    {ok, HM3}.

-spec is_sysname(Model, Sysname) -> boolean() when
    Model :: t(),
    Sysname :: erldist_filter_nif:sysname().
is_sysname(#{'__type__' := ?MODULE, sysname := Sysname}, Sysname) ->
    true;
is_sysname(#{'__type__' := ?MODULE}, _Sysname) ->
    false.

-spec channel_recv_packets(Model, Packets) -> {ok, Info, Model} when
    Model :: t(),
    Packets :: [binary()],
    Info :: #{actions := [Action], events := [{dop, ControlMessage, MaybePayload}]},
    Action :: erldist_filter_nif:action(),
    ControlMessage :: vdist:control_message(),
    MaybePayload :: undefined | vterm:t().
channel_recv_packets(Model = #{'__type__' := ?MODULE}, Packets) when is_list(Packets) ->
    channel_recv_packets(Model, Packets, [], []).

%% @private
channel_recv_packets(
    Model0 = #{'__type__' := ?MODULE, entry := Entry0, packet_size := 0}, [Packet | Packets], Actions, Events
) ->
    case vdist_entry:decode(Entry0, Packet) of
        {ok, ControlMessage, MaybePayload, Entry1, <<>>} ->
            Model1 = Model0#{entry := Entry1},
            channel_recv_packets(Model1, Packets, [{emit, Packet} | Actions], [
                {dop, ControlMessage, MaybePayload} | Events
            ]);
        {cont, Entry1} ->
            Model1 = Model0#{entry := Entry1},
            channel_recv_packets(Model1, Packets, [{emit, Packet} | Actions], Events)
    end;
channel_recv_packets(
    Model0 = #{'__type__' := ?MODULE, entry := Entry0, packet_size := PacketSize},
    [HeadPacket | Packets],
    Actions,
    Events
) ->
    <<PacketLength:PacketSize/unsigned-big-integer-unit:8, Packet:PacketLength/bytes>> = HeadPacket,
    case vdist_entry:decode(Entry0, Packet) of
        {ok, ControlMessage, MaybePayload, Entry1, <<>>} ->
            Model1 = Model0#{entry := Entry1},
            channel_recv_packets(Model1, Packets, [{emit, Packet} | Actions], [
                {dop, ControlMessage, MaybePayload} | Events
            ]);
        {cont, Entry1} ->
            Model1 = Model0#{entry := Entry1},
            channel_recv_packets(Model1, Packets, [{emit, Packet} | Actions], Events)
    end;
channel_recv_packets(Model = #{'__type__' := ?MODULE}, [], Actions, Events) ->
    Info = #{
        actions => lists:reverse(Actions),
        events => lists:reverse(Events)
    },
    {ok, Info, Model}.

-spec channel_send_packets(Model, Spec) -> {ok, Spec, Model} when Model :: t(), Spec :: dop_spec().
channel_send_packets(
    ModelChannel0 = #{entry := Entry0}, Spec = #{header_mode := HeaderMode, fragment_size := FragmentSize}
) ->
    Options = #{
        fragment_size => FragmentSize
    },
    case Spec of
        #{control_message := ControlMessage} when
            ?is_vdist_dop_without_payload_t(ControlMessage) andalso is_map_key(payload, Spec) =:= false
        ->
            case HeaderMode of
                fragment ->
                    {ok, Fragments, Entry1} = vdist_entry_encode:encode_with_fragment_header(
                        Entry0, ControlMessage, Options
                    ),
                    ModelChannel1 = ModelChannel0#{entry := Entry1},
                    Packets = cast_fragments_to_packets(ModelChannel1, Fragments),
                    {ok, Spec#{packets => Packets}, ModelChannel1};
                normal ->
                    {ok, Fragments, Entry1} = vdist_entry_encode:encode_with_normal_header(
                        Entry0, ControlMessage, Options
                    ),
                    ModelChannel1 = ModelChannel0#{entry := Entry1},
                    Packets = cast_fragments_to_packets(ModelChannel1, Fragments),
                    {ok, Spec#{packets => Packets}, ModelChannel1};
                pass_through ->
                    {ok, Fragments, Entry1} = vdist_entry_encode:encode_with_pass_through_header(
                        Entry0, ControlMessage, Options
                    ),
                    ModelChannel1 = ModelChannel0#{entry := Entry1},
                    Packets = cast_fragments_to_packets(ModelChannel1, Fragments),
                    {ok, Spec#{packets => Packets}, ModelChannel1}
            end;
        #{control_message := ControlMessage, payload := Payload} when
            ?is_vdist_dop_with_payload_t(ControlMessage) andalso ?is_vterm_t(Payload)
        ->
            case HeaderMode of
                fragment ->
                    {ok, Fragments, Entry1} = vdist_entry_encode:encode_with_fragment_header(
                        Entry0, ControlMessage, Payload, Options
                    ),
                    ModelChannel1 = ModelChannel0#{entry := Entry1},
                    Packets = cast_fragments_to_packets(ModelChannel1, Fragments),
                    {ok, Spec#{packets => Packets}, ModelChannel1};
                normal ->
                    {ok, Fragments, Entry1} = vdist_entry_encode:encode_with_normal_header(
                        Entry0, ControlMessage, Payload, Options
                    ),
                    ModelChannel1 = ModelChannel0#{entry := Entry1},
                    Packets = cast_fragments_to_packets(ModelChannel1, Fragments),
                    {ok, Spec#{packets => Packets}, ModelChannel1};
                pass_through ->
                    {ok, Fragments, Entry1} = vdist_entry_encode:encode_with_pass_through_header(
                        Entry0, ControlMessage, Payload, Options
                    ),
                    ModelChannel1 = ModelChannel0#{entry := Entry1},
                    Packets = cast_fragments_to_packets(ModelChannel1, Fragments),
                    {ok, Spec#{packets => Packets}, ModelChannel1}
            end
    end.

is_atom_cache_filled(#{'__type__' := ?MODULE, entry := Entry}) ->
    vdist_entry:is_tx_atom_cache_filled(Entry).

maybe_fill_atom_cache(
    Model0 = #{'__type__' := ?MODULE, entry := Entry0},
    Spec0 = #{header_mode := HeaderMode, fragment_size := FragmentSize}
) ->
    case vdist_entry:is_tx_atom_cache_filled(Entry0) of
        false ->
            Options = #{
                header_mode => HeaderMode,
                fragment_size => FragmentSize
            },
            {ControlMessage, Payload} = vdist_entry:reg_send_noop(),
            {ok, Fragments, Entry1} = vdist_entry:fill_tx_atom_cache(Entry0, Options),
            Model1 = Model0#{entry := Entry1},
            Packets = cast_fragments_to_packets(Model1, Fragments),
            Spec1 = Spec0#{
                control_message => ControlMessage,
                payload => Payload,
                packets => Packets
            },
            {ok, Spec1, Model1};
        true ->
            {error, atom_cache_already_filled}
    end.

-spec cast_actions_to_fragments(Model, Actions) -> Fragments when
    Model :: t(), Actions :: [Action], Action :: erldist_filter_nif:action(), Fragments :: [binary()].
cast_actions_to_fragments(#{'__type__' := ?MODULE}, Actions) when is_list(Actions) ->
    Fragments = [Fragment || {emit, Fragment} <- Actions],
    Fragments.

-spec cast_actions_to_packets(Model, Actions) -> Packets when
    Model :: t(), Actions :: [Action], Action :: erldist_filter_nif:action(), Packets :: [binary()].
cast_actions_to_packets(Model = #{'__type__' := ?MODULE}, Actions) when is_list(Actions) ->
    Fragments = cast_actions_to_fragments(Model, Actions),
    Packets = cast_fragments_to_packets(Model, Fragments),
    Packets.

-spec cast_fragments_to_packets(Model, Fragments) -> Packets when
    Model :: t(), Fragments :: [binary()], Packets :: [binary()].
cast_fragments_to_packets(#{'__type__' := ?MODULE, packet_size := PacketSize}, Fragments = [Fragment | _]) when
    is_binary(Fragment)
->
    do_encode_packets(PacketSize, Fragments);
cast_fragments_to_packets(#{'__type__' := ?MODULE}, []) ->
    [].

-spec cast_packets_to_actions(Model, Packets) -> [Action] when
    Model :: t(), Packets :: [binary()], Action :: erldist_filter_nif:action().
cast_packets_to_actions(Model = #{'__type__' := ?MODULE}, Packets) when is_list(Packets) ->
    Fragments = cast_packets_to_fragments(Model, Packets),
    Actions = [{emit, Fragment} || Fragment <- Fragments],
    Actions.

-spec cast_packets_to_fragments(Model, Packets) -> Fragments when
    Model :: t(), Packets :: [binary()], Fragments :: [binary()].
cast_packets_to_fragments(#{'__type__' := ?MODULE, packet_size := PacketSize}, Packets = [Packet | _]) when
    is_binary(Packet)
->
    do_decode_packets(PacketSize, Packets);
cast_packets_to_fragments(#{'__type__' := ?MODULE}, []) ->
    [].

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
do_decode_packets(PacketSize = 0, [Fragment | Fragments]) ->
    [Fragment | do_decode_packets(PacketSize, Fragments)];
do_decode_packets(PacketSize = 1, [<<PacketLength:8, Fragment:PacketLength/bytes>> | Packets]) ->
    [Fragment | do_decode_packets(PacketSize, Packets)];
do_decode_packets(PacketSize = 2, [<<PacketLength:16, Fragment:PacketLength/bytes>> | Packets]) ->
    [Fragment | do_decode_packets(PacketSize, Packets)];
do_decode_packets(PacketSize = 4, [<<PacketLength:32, Fragment:PacketLength/bytes>> | Packets]) ->
    [Fragment | do_decode_packets(PacketSize, Packets)];
do_decode_packets(PacketSize = 8, [<<PacketLength:64, Fragment:PacketLength/bytes>> | Packets]) ->
    [Fragment | do_decode_packets(PacketSize, Packets)];
do_decode_packets(_PacketSize, []) ->
    [].

%% @private
do_encode_packets(_PacketSize = 0, Packets = [_ | _]) ->
    Packets;
do_encode_packets(PacketSize = 1, [Fragment | Fragments]) when byte_size(Fragment) =< 16#FF ->
    [<<(byte_size(Fragment)):8, Fragment/bytes>> | do_encode_packets(PacketSize, Fragments)];
do_encode_packets(PacketSize = 2, [Fragment | Fragments]) when byte_size(Fragment) =< 16#FFFF ->
    [<<(byte_size(Fragment)):16, Fragment/bytes>> | do_encode_packets(PacketSize, Fragments)];
do_encode_packets(PacketSize = 4, [Fragment | Fragments]) when byte_size(Fragment) =< 16#FFFFFFFF ->
    [<<(byte_size(Fragment)):32, Fragment/bytes>> | do_encode_packets(PacketSize, Fragments)];
do_encode_packets(PacketSize = 8, [Fragment | Fragments]) when byte_size(Fragment) =< 16#FFFFFFFFFFFFFFFF ->
    [<<(byte_size(Fragment)):64, Fragment/bytes>> | do_encode_packets(PacketSize, Fragments)];
do_encode_packets(_PacketSize, []) ->
    [].

%% @private
maybe_drop_noop_actions(_Model = #{'__type__' := ?MODULE}, Actions = [{emit, _}, {emit, _} | _]) ->
    lists:filter(fun should_keep_noop_action/1, Actions);
maybe_drop_noop_actions(_Model = #{'__type__' := ?MODULE}, OtherResult) ->
    OtherResult.

%% @private
should_keep_noop_action({emit, Fragment}) ->
    case vdist_header_decode:decode_header(Fragment) of
        {ok, #vdist_fragment_header{fragment_id = 1}, EncControlMessage} ->
            {ok, ControlMessageVTerm, EncPayload} = vterm_decode:internal_binary_to_vterm(EncControlMessage),
            AtomEmpty = #vterm_atom_utf8_ext{len = 0, name = <<>>},
            AtomUndefined = #vterm_atom_utf8_ext{len = 9, name = <<"undefined">>},
            case vdist_dop:control_message_vterm_to_dop(ControlMessageVTerm) of
                #vdist_dop_reg_send{
                    from_pid = #vterm_new_pid_ext{node = AtomEmpty, id = 0, serial = 0, creation = 0},
                    unused = AtomEmpty,
                    to_name = AtomUndefined
                } ->
                    case vterm_decode:internal_binary_to_vterm(EncPayload) of
                        {ok, #vterm_nil_ext{}, <<>>} ->
                            false;
                        _ ->
                            true
                    end;
                _ ->
                    true
            end;
        _ ->
            true
    end.

% %% @private
% maybe_drop_first_action([{emit, _FirstAction} | Actions = [{emit, _SecondAction}, {emit, _ThirdAction} | _]]) ->
%     Actions;
% maybe_drop_first_action(Result) ->
%     Result.
