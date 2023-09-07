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
%%% Created :  09 Jun 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(vedf_channel).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include("erldist_filter.hrl").
-include("erldist_filter_erts_dist.hrl").
-include("erldist_filter_erts_external.hrl").
-include("udist.hrl").

%% API
-export([
    new/2,
    new/3,
    recv/2,
    send_encode/3,
    send_encode/4,
    cast_fragments_to_packets/2
]).

%% Types
-type config() :: #{
    compact_fragments => boolean(),
    deep_packet_inspection => boolean(),
    logging => boolean(),
    redirect_dist_operations => boolean(),
    sysname => undefined | erldist_filter_nif:sysname(),
    untrusted => boolean()
}.
-type t() :: #vedf_channel{}.

-export_type([
    config/0,
    t/0
]).

%% Macros
-define(is_packet_size(X),
    (is_integer(X) andalso (X =:= 0 orelse X =:= 1 orelse X =:= 2 orelse X =:= 4 orelse X =:= 8))
).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(PacketSize, DistributionFlags) -> T when
    PacketSize :: 0 | 1 | 2 | 4 | 8, DistributionFlags :: vterm:u64(), T :: t().
new(PacketSize, DistributionFlags) when ?is_packet_size(PacketSize) andalso is_integer(DistributionFlags) ->
    new(PacketSize, DistributionFlags, #{}).

-spec new(PacketSize, DistributionFlags, Config) -> T when
    PacketSize :: 0 | 1 | 2 | 4 | 8, DistributionFlags :: vterm:u64(), Config :: config(), T :: t().
new(PacketSize, DistributionFlags, Config) when
    ?is_packet_size(PacketSize) andalso is_integer(DistributionFlags) andalso is_map(Config)
->
    CompactFragments = maps:get(compact_fragments, Config, false),
    DeepPacketInspection = maps:get(deep_packet_inspection, Config, false),
    Logging = maps:get(logging, Config, false),
    RedirectDistOperations = maps:get(redirect_dist_operations, Config, false),
    Sysname = maps:get(sysname, Config, undefined),
    Untrusted = maps:get(untrusted, Config, false),
    Channel0 =
        #vedf_channel{
            packet_size = PacketSize,
            dflags = DistributionFlags,
            rx_atom_cache = undefined,
            rx_logger_time = 0,
            rx_router_name = erldist_filter_nif:router_name(Sysname),
            rx_sort = 0,
            compact_fragments = CompactFragments,
            deep_packet_inspection = DeepPacketInspection,
            logging = Logging,
            redirect_dist_operations = RedirectDistOperations,
            sysname = Sysname,
            untrusted = Untrusted
        },
    Channel1 =
        case
            (DistributionFlags band ?DFLAG_DIST_HDR_ATOM_CACHE) =/= 0 orelse
                (DistributionFlags band ?DFLAG_FRAGMENTS) =/= 0
        of
            true ->
                RxAtomCache = vdist_atom_cache:new(),
                Channel0#vedf_channel{
                    rx_atom_cache = RxAtomCache
                };
            false ->
                Channel0
        end,
    Channel1.

-spec recv(OldChannel, Packets) -> {ok, Actions, NewChannel} when
    OldChannel :: t(),
    Packets :: [binary()],
    Actions :: [Action],
    Action :: erldist_filter_nif:action(),
    NewChannel :: t().
recv(Channel = #vedf_channel{dflags = DFlags, rx_atom_cache = RxAtomCache}, Packets) when is_list(Packets) ->
    Entry0 = vdist_entry:new(DFlags),
    Entry1 = Entry0#vdist_entry{rx_atom_cache = RxAtomCache},
    do_recv(Channel, Entry1, Packets, []).

do_recv(
    Channel0 = #vedf_channel{packet_size = PacketSize, rx_sequences = Sequences0, compact_fragments = CompactFragments},
    Entry0,
    [Packet | Packets],
    Actions0
) ->
    Fragment =
        case Packet of
            _ when PacketSize =:= 0 ->
                Packet;
            <<PacketLen:PacketSize/unsigned-big-integer-unit:8, PacketFragment:PacketLen/bytes>> ->
                PacketFragment
        end,
    NextStep =
        case vdist_header_decode:decode_header(Fragment) of
            {ok, FragHeader = #vdist_fragment_header{sequence_id = SeqId, fragment_id = FragId}, _EncControlMessage} ->
                {append_sequence, SeqId, FragId, FragHeader};
            {ok, FragCont = #vdist_fragment_cont{sequence_id = SeqId, fragment_id = FragId}, _EncControlMessage} ->
                {append_sequence, SeqId, FragId, FragCont};
            {ok, #vdist_normal_header{}, _EncControlMessage} ->
                decode_fragment;
            {ok, #vdist_pass_through_header{}, _EncControlMessage} ->
                decode_fragment
        end,
    case NextStep of
        decode_fragment ->
            case vdist_entry:decode(Entry0, Fragment) of
                {ok, ControlMessage, MaybePayload, Entry1, <<>>} ->
                    {Channel1, MoreActions} = deep_packet_inspection(
                        Channel0, Entry1, ControlMessage, MaybePayload, Fragment
                    ),
                    Actions1 = MoreActions ++ Actions0,
                    do_recv(Channel1, Entry1, Packets, Actions1)
            end;
        {append_sequence, SequenceId, _FragmentId, Header = #vdist_fragment_cont{}} ->
            {ok, External0 = #vdist_external{}} = maps:find(SequenceId, Sequences0),
            case vdist_external:append_next_fragment(External0, Header, Fragment) of
                {cont, External1} ->
                    Sequences1 = maps:put(SequenceId, External1, Sequences0),
                    Channel1 = Channel0#vedf_channel{rx_sequences = Sequences1},
                    do_recv(Channel1, Entry0, Packets, Actions0);
                {ok, FragmentsQueue0, AtomTable = #vdist_atom_translation_table{}} ->
                    {{value, FragmentHeader0}, FragmentsQueue1} = queue:out(FragmentsQueue0),
                    {ok, FragmentHeader1, MaybeRollback} = maybe_rewrite_fragment_header(
                        Entry0#vdist_entry.rx_atom_cache, AtomTable, FragmentHeader0
                    ),
                    FragmentsQueue2 = queue:in_r(FragmentHeader1, FragmentsQueue1),
                    Sequences1 = maps:remove(SequenceId, Sequences0),
                    Channel1 = Channel0#vedf_channel{rx_sequences = Sequences1},
                    {ok, Channel2, Entry1, ExternalActions} = do_recv_sequence(
                        Channel1, Entry0, FragmentsQueue2, []
                    ),
                    {Channel3, MoreActions} =
                        case CompactFragments of
                            true ->
                                CF = compact_fragments(FragmentsQueue2, <<>>),
                                {ok, ControlMessage, MaybePayload, Entry1, <<>>} = vdist_entry:decode(Entry0, CF),
                                deep_packet_inspection(Channel2, Entry1, ControlMessage, MaybePayload, CF);
                            false ->
                                {Channel2, ExternalActions}
                        end,
                    Actions1 = MoreActions ++ Actions0,
                    {ok, Actions2, Entry2} = maybe_emit_atom_cache_rollback(Entry1, MaybeRollback, Actions1),
                    do_recv(Channel3, Entry2, Packets, Actions2)
            end;
        {append_sequence, _SequenceId, _FragmentId = 1, #vdist_fragment_header{}} ->
            case vdist_entry:decode(Entry0, Fragment) of
                {ok, ControlMessage, MaybePayload, Entry1, <<>>} ->
                    {Channel1, MoreActions} = deep_packet_inspection(
                        Channel0, Entry1, ControlMessage, MaybePayload, Fragment
                    ),
                    Actions1 = MoreActions ++ Actions0,
                    do_recv(Channel1, Entry1, Packets, Actions1)
            end;
        {append_sequence, SequenceId, _FragmentId, Header = #vdist_fragment_header{}} ->
            #vdist_fragment_header{sequence_id = SequenceId, fragment_id = FragmentCount} = Header,
            {ok, _, AtomTable} = vdist_fragment_header:update_atom_cache(
                Header, Entry0#vdist_entry.rx_atom_cache
            ),
            {ok, Actions1, Entry1} = maybe_emit_atom_cache_commit(Entry0, Header, Actions0),
            External0 = vdist_external:new(AtomTable, SequenceId, FragmentCount),
            FragContHeader = vdist_fragment_cont:new(SequenceId, Header#vdist_fragment_header.fragment_id),
            {cont, External1} = vdist_external:append_next_fragment(External0, FragContHeader, Fragment),
            Sequences1 = maps:put(SequenceId, External1, Sequences0),
            Channel1 = Channel0#vedf_channel{rx_sequences = Sequences1},
            do_recv(Channel1, Entry1, Packets, Actions1)
    end;
do_recv(Channel0 = #vedf_channel{}, _Entry = #vdist_entry{rx_atom_cache = RxAtomCache}, _Packets = [], Actions0) ->
    Channel1 = Channel0#vedf_channel{rx_atom_cache = RxAtomCache},
    Actions1 = lists:reverse(Actions0),
    {ok, Actions1, Channel1}.

-spec send_encode(Channel, ControlMessage, Options) -> {ok, Packets, Channel} when
    Channel :: t(),
    ControlMessage :: vdist:dop_without_payload_t(),
    Options :: vdist_entry_encode:options(),
    Packets :: [binary()].
send_encode(Channel = #vedf_channel{dflags = DFlags, rx_atom_cache = RxAtomCache}, ControlMessage, Options) when
    ?is_vdist_dop_without_payload_t(ControlMessage) andalso is_map(Options)
->
    Entry0 = vdist_entry:new(DFlags),
    Entry1 = Entry0#vdist_entry{tx_atom_cache = RxAtomCache},
    case vdist_entry_encode:encode(Entry1, ControlMessage, Options) of
        {ok, Fragments, _Entry2 = #vdist_entry{}} ->
            Packets = cast_fragments_to_packets(Channel, Fragments),
            {ok, Packets, Channel}
    end.

-spec send_encode(Channel, ControlMessage, Payload, Options) -> {ok, Packets, Channel} when
    Channel :: t(),
    ControlMessage :: vdist:dop_with_payload_t(),
    Payload :: vterm:t(),
    Options :: vdist_entry_encode:options(),
    Packets :: [binary()].
send_encode(
    Channel = #vedf_channel{dflags = DFlags, rx_atom_cache = RxAtomCache}, ControlMessage, Payload, Options
) when
    ?is_vdist_dop_with_payload_t(ControlMessage) andalso
        ?is_vterm_t(Payload) andalso is_map(Options)
->
    Entry0 = vdist_entry:new(DFlags),
    Entry1 = Entry0#vdist_entry{tx_atom_cache = RxAtomCache},
    case vdist_entry_encode:encode(Entry1, ControlMessage, Payload, Options) of
        {ok, Fragments, _Entry2 = #vdist_entry{}} ->
            Packets = cast_fragments_to_packets(Channel, Fragments),
            {ok, Packets, Channel}
    end.

-spec cast_fragments_to_packets(Channel, Fragments) -> Packets when
    Channel :: t(), Fragments :: [binary()], Packets :: [binary()].
cast_fragments_to_packets(#vedf_channel{packet_size = PacketSize}, Fragments = [Fragment | _]) when
    is_binary(Fragment)
->
    do_encode_packets(PacketSize, Fragments);
cast_fragments_to_packets(#vedf_channel{}, []) ->
    [].

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
action_log_event_atoms(Cache, [#vdist_new_atom_cache_ref_entry{atom_text = AtomText} | Entries], Atoms) ->
    Atom = erlang:binary_to_atom(AtomText, unicode),
    action_log_event_atoms(Cache, Entries, [Atom | Atoms]);
action_log_event_atoms(Cache, [#vdist_old_atom_cache_ref_entry{atom_cache_index = AtomCacheIndex} | Entries], Atoms) ->
    case vdist_atom_cache:find(Cache, AtomCacheIndex) of
        {ok, {_, Atom}} ->
            action_log_event_atoms(Cache, Entries, [Atom | Atoms])
    end;
action_log_event_atoms(_Cache, [], Atoms) ->
    erlang:list_to_tuple(lists:reverse(Atoms)).

%% @private
compact_fragments(Q0, Acc) ->
    case queue:out(Q0) of
        {{value, Fragment}, Q1} ->
            case vdist_header_decode:decode_header(Fragment) of
                {ok, FragHeader = #vdist_fragment_header{}, EncRest} when Acc =:= <<>> ->
                    % Rewrite first fragment_id to be 1.
                    EncHeader = vdist_header_encode:encode_header(FragHeader#vdist_fragment_header{fragment_id = 1}),
                    compact_fragments(Q1, <<EncHeader/bytes, EncRest/bytes>>);
                {ok, _FragCont = #vdist_fragment_cont{}, EncRest} when Acc =/= <<>> ->
                    compact_fragments(Q1, <<Acc/bytes, EncRest/bytes>>);
                {ok, #vdist_normal_header{}, _EncRest} when Acc =:= <<>> ->
                    compact_fragments(Q1, Fragment);
                {ok, #vdist_pass_through_header{}, _EncRest} ->
                    compact_fragments(Q1, Fragment)
            end;
        {empty, Q0} when Acc =/= <<>> ->
            Acc
    end.

%% @private
deep_packet_inspection(
    Channel = #vedf_channel{deep_packet_inspection = true}, Entry, ControlMessage, MaybePayload, CompactFragment
) when is_binary(CompactFragment) ->
    Control = udist:cast_to_dop(vdist:simplify(ControlMessage)),
    case Control of
        _ when ?is_udist_dop_exit_t(Control) ->
            dpi_classify_exit(Channel, Entry, Control, MaybePayload, CompactFragment);
        _ when ?is_udist_dop_exit2_t(Control) ->
            dpi_classify_exit2(Channel, Entry, Control, MaybePayload, CompactFragment);
        _ when ?is_udist_dop_group_leader_t(Control) ->
            dpi_classify_group_leader(Channel, Entry, Control, MaybePayload, CompactFragment);
        _ when ?is_udist_dop_link_t(Control) ->
            dpi_classify_link(Channel, Entry, Control, MaybePayload, CompactFragment);
        _ when ?is_udist_dop_monitor_related_t(Control) ->
            dpi_classify_monitor_related(Channel, Entry, Control, MaybePayload, CompactFragment);
        _ when ?is_udist_dop_send_to_alias_t(Control) ->
            dpi_classify_send_to_alias(Channel, Entry, Control, MaybePayload, CompactFragment);
        _ when ?is_udist_dop_send_to_name_t(Control) ->
            dpi_classify_send_to_name(Channel, Entry, Control, MaybePayload, CompactFragment);
        _ when ?is_udist_dop_send_to_pid_t(Control) ->
            dpi_classify_send_to_pid(Channel, Entry, Control, MaybePayload, CompactFragment);
        _ when ?is_udist_dop_spawn_reply_t(Control) ->
            dpi_classify_spawn_reply(Channel, Entry, Control, MaybePayload, CompactFragment);
        _ when ?is_udist_dop_spawn_request_t(Control) ->
            dpi_classify_spawn_request(Channel, Entry, Control, MaybePayload, CompactFragment);
        _ when ?is_udist_dop_unlink_t(Control) ->
            dpi_classify_unlink(Channel, Entry, Control, MaybePayload, CompactFragment)
    end;
deep_packet_inspection(
    Channel = #vedf_channel{deep_packet_inspection = false}, _Entry, _ControlMessage, _MaybePayload, CompactFragment
) when
    is_binary(CompactFragment)
->
    {Channel, [{emit, CompactFragment}]}.

%% @private
dpi_classify_exit(Channel, Entry, Control, MaybePayload, CompactFragment) ->
    % LOG, DROP
    dpi_maybe_log_event(Channel, Entry, Control, MaybePayload, CompactFragment, fun dpi_drop/5).

%% @private
dpi_classify_exit2(Channel, Entry, Control, MaybePayload, CompactFragment) ->
    % LOG, DROP or REDIRECT
    dpi_maybe_log_event(Channel, Entry, Control, MaybePayload, CompactFragment, fun dpi_drop_or_redirect/5).

%% @private
dpi_classify_group_leader(Channel, Entry, Control, MaybePayload, CompactFragment) ->
    % LOG, DROP or REDIRECT
    dpi_maybe_log_event(Channel, Entry, Control, MaybePayload, CompactFragment, fun dpi_drop_or_redirect/5).

%% @private
dpi_classify_link(Channel, Entry, Control, MaybePayload, CompactFragment) ->
    % LOG, DROP
    dpi_maybe_log_event(Channel, Entry, Control, MaybePayload, CompactFragment, fun dpi_drop/5).

%% @private
dpi_classify_monitor_related(Channel, Entry, Control, MaybePayload, CompactFragment) ->
    % EMIT
    dpi_emit(Channel, Entry, Control, MaybePayload, CompactFragment).

%% @private
dpi_classify_send_to_alias(Channel, Entry, Control, MaybePayload, CompactFragment) ->
    dpi_classify_send(Channel, Entry, Control, MaybePayload, CompactFragment).

%% @private
dpi_classify_send_to_name(Channel, Entry, Control, MaybePayload, CompactFragment) ->
    case Control of
        #udist_dop_reg_send{to_name = Name} ->
            case Name of
                net_kernel ->
                    dpi_classify_send_to_net_kernel(Channel, Entry, Control, MaybePayload, CompactFragment);
                rex ->
                    dpi_classify_send_to_rex(Channel, Entry, Control, MaybePayload, CompactFragment);
                _ ->
                    dpi_classify_send(Channel, Entry, Control, MaybePayload, CompactFragment)
            end;
        #udist_dop_reg_send_tt{to_name = Name} ->
            case Name of
                net_kernel ->
                    dpi_classify_send_to_net_kernel(Channel, Entry, Control, MaybePayload, CompactFragment);
                rex ->
                    dpi_classify_send_to_rex(Channel, Entry, Control, MaybePayload, CompactFragment);
                _ ->
                    dpi_classify_send(Channel, Entry, Control, MaybePayload, CompactFragment)
            end
    end.

%% @private
dpi_classify_send_to_pid(Channel, Entry, Control, MaybePayload, CompactFragment) ->
    dpi_classify_send(Channel, Entry, Control, MaybePayload, CompactFragment).

%% @private
dpi_classify_spawn_reply(Channel, Entry, Control, MaybePayload, CompactFragment) ->
    % EMIT
    dpi_emit(Channel, Entry, Control, MaybePayload, CompactFragment).

%% @private
dpi_classify_spawn_request(Channel, Entry, Control, MaybePayload, CompactFragment) ->
    % LOG, REDIRECT
    dpi_maybe_log_event(Channel, Entry, Control, MaybePayload, CompactFragment, fun dpi_redirect_spawn_request/5).

%% @private
dpi_classify_unlink(Channel, Entry, Control, MaybePayload, CompactFragment) ->
    % EMIT
    dpi_emit(Channel, Entry, Control, MaybePayload, CompactFragment).

%% @private
dpi_classify_send(Channel, Entry, Control, Payload0, CompactFragment) when ?is_vterm_t(Payload0) ->
    %% Logs, emits, drops, and/or redirects any messages matching the following:
    %%
    %%     {system, From, Request}
    %%     {'EXIT', Pid, Reason}
    %%     {'$gen_cast', {try_again_restart, TryAgainId}}
    %%     {'$gen_call', From, {start_child, ChildSpec}}
    %%     {'$gen_call', From, {terminate_child, ChildId}}
    %%     {'$gen_call', From, {restart_child, ChildId}}
    %%     {'$gen_call', From, {delete_child, ChildId}}
    %%     {io_request, From, ReplyAs, Request}
    %%     {io_reply, ReplyAs, Reply}
    %%
    %% Otherwise, emit the message.
    Payload = vterm:simplify(Payload0),
    case Payload of
        {system, _From, _Request} ->
            % LOG, DROP or REDIRECT
            dpi_maybe_log_event(Channel, Entry, Control, Payload0, CompactFragment, fun dpi_drop_or_redirect/5);
        {'EXIT', _Pid, _Reason} ->
            % LOG, DROP or REDIRECT
            dpi_maybe_log_event(Channel, Entry, Control, Payload0, CompactFragment, fun dpi_drop_or_redirect/5);
        {'$gen_cast', {try_again_restart, _TryAgainId}} ->
            % LOG, DROP or REDIRECT
            dpi_maybe_log_event(Channel, Entry, Control, Payload0, CompactFragment, fun dpi_drop_or_redirect/5);
        {'$gen_call', _From, Request} ->
            Hint =
                case Request of
                    {start_child, _ChildSpec} -> drop;
                    {terminate_child, _ChildId} -> drop;
                    {restart_child, _ChildId} -> drop;
                    {delete_child, _ChildId} -> drop;
                    _ -> unsafe
                end,
            case Hint of
                drop ->
                    % LOG, DROP or REDIRECT
                    dpi_maybe_log_event(Channel, Entry, Control, Payload0, CompactFragment, fun dpi_drop_or_redirect/5);
                unsafe ->
                    % EMIT
                    dpi_emit(Channel, Entry, Control, Payload0, CompactFragment)
            end;
        {io_request, _From, _ReplyAs, _Request} ->
            % LOG, DROP or REDIRECT
            dpi_maybe_log_event(Channel, Entry, Control, Payload0, CompactFragment, fun dpi_drop_or_redirect/5);
        {io_reply, _ReplyAs, _Reply} ->
            % LOG, DROP or REDIRECT
            dpi_maybe_log_event(Channel, Entry, Control, Payload0, CompactFragment, fun dpi_drop_or_redirect/5);
        _ ->
            % EMIT
            dpi_emit(Channel, Entry, Control, Payload0, CompactFragment)
    end.

%% @private
dpi_classify_send_to_net_kernel(Channel, Entry, Control, Payload0, CompactFragment) when ?is_vterm_t(Payload0) ->
    %% REG_SEND: net_kernel
    %%
    %% Only allow messages matching the following:
    %%
    %%     {'$gen_call', From, {is_auth, Node}}
    %%
    %% Otherwise, redirect the message (drop it).
    Payload = vterm:simplify(Payload0),
    case Payload of
        {'$gen_call', _From, {is_auth, _Node}} ->
            % EMIT
            dpi_emit(Channel, Entry, Control, Payload0, CompactFragment);
        _ ->
            % LOG, DROP or REDIRECT
            dpi_maybe_log_event(Channel, Entry, Control, Payload0, CompactFragment, fun dpi_drop_or_redirect/5)
    end.

%% @private
dpi_classify_send_to_rex(Channel, Entry, Control, Payload0, CompactFragment) when ?is_vterm_t(Payload0) ->
    %% REG_SEND: rex
    %%
    %% Only allow messages matching the following:
    %%
    %%     {From, features_request}
    %%     {features_reply, node(), [erpc]}
    %%
    %% Otherwise, redirect the message (drop it).
    Payload = vterm:simplify(Payload0),
    case Payload of
        {_From, features_request} ->
            % EMIT
            dpi_emit(Channel, Entry, Control, Payload0, CompactFragment);
        {features_reply, _Node, _Reply} ->
            % EMIT
            dpi_emit(Channel, Entry, Control, Payload0, CompactFragment);
        _ ->
            % LOG, DROP or REDIRECT
            dpi_maybe_log_event(Channel, Entry, Control, Payload0, CompactFragment, fun dpi_drop_or_redirect/5)
    end.

%% @private
dpi_drop(Channel, _Entry, _Control, _MaybePayload, CompactFragment) ->
    Node = vterm_small_atom_utf8_ext:new(0, <<>>),
    Unused = vterm_small_atom_utf8_ext:new(0, <<>>),
    ToName = vterm_small_atom_utf8_ext:new(9, <<"undefined">>),
    FromPid = vterm_new_pid_ext:new(Node, 0, 0, 0),
    RegSendDOP = vdist_dop_reg_send:new(FromPid, Unused, ToName),
    ControlVTerm = vdist_dop:dop_to_control_message_vterm(RegSendDOP),
    PayloadVTerm = vterm_nil_ext:new(),
    case vdist_header_decode:decode_header(CompactFragment) of
        {ok, #vdist_fragment_header{}, EncRest} ->
            EncHeadersLength = byte_size(CompactFragment) - byte_size(EncRest),
            EncHeaders = binary:part(CompactFragment, 0, EncHeadersLength),
            EncControl = vterm_encode:internal_vterm_to_binary(ControlVTerm, #{}),
            EncPayload = vterm_encode:internal_vterm_to_binary(PayloadVTerm, #{}),
            EncExternal = <<EncHeaders/bytes, EncControl/bytes, EncPayload/bytes>>,
            {Channel, [{emit, EncExternal}]};
        {ok, #vdist_normal_header{}, EncRest} ->
            EncHeadersLength = byte_size(CompactFragment) - byte_size(EncRest),
            EncHeaders = binary:part(CompactFragment, 0, EncHeadersLength),
            EncControl = vterm_encode:internal_vterm_to_binary(ControlVTerm, #{}),
            EncPayload = vterm_encode:internal_vterm_to_binary(PayloadVTerm, #{}),
            EncExternal = <<EncHeaders/bytes, EncControl/bytes, EncPayload/bytes>>,
            {Channel, [{emit, EncExternal}]};
        {ok, #vdist_pass_through_header{}, _EncRest} ->
            {Channel, []}
    end.

%% @private
dpi_drop_or_redirect(
    Channel = #vedf_channel{redirect_dist_operations = true}, Entry, Control, MaybePayload, CompactFragment
) ->
    dpi_redirect_dop(Channel, Entry, Control, MaybePayload, CompactFragment);
dpi_drop_or_redirect(Channel, Entry, Control, MaybePayload, CompactFragment) ->
    dpi_drop(Channel, Entry, Control, MaybePayload, CompactFragment).

%% @private
dpi_emit(Channel, _Entry, _Control, _MaybePayload, CompactFragment) ->
    {Channel, [{emit, CompactFragment}]}.

%% @private
dpi_maybe_log_event(
    Channel0 = #vedf_channel{
        rx_logger_time = LoggerTime0, sysname = Sysname, rx_atom_cache = RxAtomCache, logging = true
    },
    Entry,
    ControlMessage,
    MaybePayload,
    CompactFragment,
    NextState
) when is_function(NextState, 5) ->
    FragmentCount = 1,
    {Atoms, Control0, Payload0} =
        case vdist_header_decode:decode_header(CompactFragment) of
            {ok, #vdist_fragment_header{fragment_id = 1, atom_cache_ref_entries = Entries}, EncRest} ->
                {ok, _ControlVTerm, EncPayload} = vterm_decode:internal_binary_to_vterm(EncRest),
                EncControlLength = byte_size(EncRest) - byte_size(EncPayload),
                EncControl = binary:part(EncRest, 0, EncControlLength),
                {
                    action_log_event_atoms(RxAtomCache, Entries, []),
                    <<?VERSION_MAGIC:8, EncControl/bytes>>,
                    <<?VERSION_MAGIC:8, EncPayload/bytes>>
                };
            {ok, #vdist_normal_header{atom_cache_ref_entries = Entries}, EncRest} ->
                {ok, _ControlVTerm, EncPayload} = vterm_decode:internal_binary_to_vterm(EncRest),
                EncControlLength = byte_size(EncRest) - byte_size(EncPayload),
                EncControl = binary:part(EncRest, 0, EncControlLength),
                {
                    action_log_event_atoms(RxAtomCache, Entries, []),
                    <<?VERSION_MAGIC:8, EncControl/bytes>>,
                    <<?VERSION_MAGIC:8, EncPayload/bytes>>
                };
            {ok, #vdist_pass_through_header{}, EncRest} ->
                {ok, _ControlVTerm, EncPayload} = vterm_decode:external_binary_to_vterm(EncRest),
                EncControlLength = byte_size(EncRest) - byte_size(EncPayload),
                EncControl = binary:part(EncRest, 0, EncControlLength),
                {{}, EncControl, EncPayload}
        end,
    Control =
        case Control0 of
            <<?VERSION_MAGIC:8>> ->
                undefined;
            _ ->
                Control0
        end,
    Payload =
        case Payload0 of
            <<>> ->
                undefined;
            <<?VERSION_MAGIC:8>> ->
                undefined;
            _ ->
                Payload0
        end,
    Action = {log, FragmentCount, {LoggerTime0, {Sysname, Atoms, Control, Payload}}},
    LoggerTime1 = LoggerTime0 + 1,
    Channel1 = Channel0#vedf_channel{rx_logger_time = LoggerTime1},
    {Channel2, Actions0} = NextState(Channel1, Entry, ControlMessage, MaybePayload, CompactFragment),
    Actions1 = Actions0 ++ [Action],
    {Channel2, Actions1};
dpi_maybe_log_event(Channel, Entry, ControlMessage, MaybePayload, CompactFragment, NextState) when
    is_function(NextState, 5)
->
    NextState(Channel, Entry, ControlMessage, MaybePayload, CompactFragment).

%% @private
dpi_redirect_dop(
    Channel0 = #vedf_channel{rx_router_name = RxRouterName, rx_sort = RxSort},
    _Entry,
    _Control,
    _MaybePayload,
    CompactFragment
) ->
    EmptyNode = vterm_small_atom_utf8_ext:new(0, <<>>),
    FromPid = vterm_new_pid_ext:new(EmptyNode, 0, 0, 0),
    Unused = vterm_small_atom_utf8_ext:new(0, <<>>),
    case vdist_header_decode:decode_header(CompactFragment) of
        {ok, Header, EncRest0} ->
            IsPassThrough =
                case Header of
                    _ when is_record(Header, vdist_fragment_header) orelse is_record(Header, vdist_normal_header) ->
                        false;
                    #vdist_pass_through_header{} ->
                        true
                end,
            EncHeadersLength = byte_size(CompactFragment) - byte_size(EncRest0),
            EncHeaders = binary:part(CompactFragment, 0, EncHeadersLength),
            {NestedControlVTerm, MaybeNestedPayloadVTerm} =
                case IsPassThrough of
                    false ->
                        case vterm_decode:internal_binary_to_vterm(EncRest0) of
                            {ok, ControlVTerm0, <<>>} ->
                                {ControlVTerm0, nothing};
                            {ok, ControlVTerm0, EncRest1} ->
                                {ok, PayloadVTerm0, <<>>} = vterm_decode:internal_binary_to_vterm(EncRest1),
                                {ControlVTerm0, {just, PayloadVTerm0}}
                        end;
                    true ->
                        case vterm_decode:external_binary_to_vterm(EncRest0) of
                            {ok, ControlVTerm0, <<>>} ->
                                {ControlVTerm0, nothing};
                            {ok, ControlVTerm0, EncRest1} ->
                                {ok, PayloadVTerm0, <<>>} = vterm_decode:external_binary_to_vterm(EncRest1),
                                {ControlVTerm0, {just, PayloadVTerm0}}
                        end
                end,
            EncToName = dpi_resolve_atom(Channel0, Header, RxRouterName),
            {ok, ToName, <<>>} = vterm_decode:internal_binary_to_vterm_atom(EncToName),
            ControlDOP =
                case dpi_resolve_token(NestedControlVTerm) of
                    {ok, TokenVTerm} ->
                        vdist_dop_reg_send_tt:new(FromPid, Unused, ToName, TokenVTerm);
                    error ->
                        vdist_dop_reg_send:new(FromPid, Unused, ToName)
                end,
            ControlVTerm = vdist_dop:dop_to_control_message_vterm(ControlDOP),
            EncNode = dpi_resolve_node(Channel0, Header),
            {ok, Node, <<>>} = vterm_decode:internal_binary_to_vterm_atom(EncNode),
            EncSort = dpi_resolve_u64(RxSort),
            {ok, Sort, <<>>} = vterm_decode:internal_binary_to_vterm(EncSort),
            PayloadVTerm =
                case MaybeNestedPayloadVTerm of
                    nothing ->
                        vterm_small_tuple_ext:new(3, [Node, Sort, NestedControlVTerm]);
                    {just, NestedPayloadVTerm} ->
                        vterm_small_tuple_ext:new(4, [Node, Sort, NestedControlVTerm, NestedPayloadVTerm])
                end,
            {EncControl, EncPayload} =
                case IsPassThrough of
                    false ->
                        EncControl0 = vterm_encode:internal_vterm_to_binary(ControlVTerm, #{
                            allow_atom_cache_refs => true
                        }),
                        EncPayload0 = vterm_encode:internal_vterm_to_binary(PayloadVTerm, #{
                            allow_atom_cache_refs => true
                        }),
                        {EncControl0, EncPayload0};
                    true ->
                        EncControl0 = vterm_encode:external_vterm_to_binary(ControlVTerm, #{
                            allow_atom_cache_refs => false
                        }),
                        EncPayload0 = vterm_encode:external_vterm_to_binary(PayloadVTerm, #{
                            allow_atom_cache_refs => false
                        }),
                        {EncControl0, EncPayload0}
                end,
            EncExternal = <<EncHeaders/bytes, EncControl/bytes, EncPayload/bytes>>,
            Channel1 = Channel0#vedf_channel{
                rx_sort = RxSort + 1
            },
            {Channel1, [{emit, EncExternal}]}
    end.

%% @private
dpi_redirect_spawn_request(Channel, _Entry, _Control, _MaybePayload, CompactFragment) ->
    StaticMFA = vterm_small_tuple_ext:new(3, [
        vterm_small_atom_utf8_ext:new(3, <<"edf">>),
        vterm_small_atom_utf8_ext:new(3, <<"req">>),
        vterm_small_integer_ext:new(3)
    ]),
    case vdist_header_decode:decode_header(CompactFragment) of
        {ok, Header, EncRest0} when
            is_record(Header, vdist_fragment_header) orelse is_record(Header, vdist_normal_header)
        ->
            EncNode = dpi_resolve_node(Channel, Header),
            EncHeadersLength = byte_size(CompactFragment) - byte_size(EncRest0),
            EncHeaders = binary:part(CompactFragment, 0, EncHeadersLength),
            {ok, ControlVTerm0, EncRest1} = vterm_decode:internal_binary_to_vterm(EncRest0),
            {ControlVTerm1, MFAVTerm} =
                case ControlVTerm0 of
                    {TupleRecord, Arity, [E0, E1, E2, E3, MFA0 | ERest]} when
                        (TupleRecord =:= vterm_small_tuple_ext orelse TupleRecord =:= vterm_large_tuple_ext) andalso
                            (Arity =:= 6 orelse Arity =:= 7)
                    ->
                        {{TupleRecord, Arity, [E0, E1, E2, E3, StaticMFA | ERest]}, MFA0}
                end,
            EncMFA = vterm_encode:internal_vterm_to_binary(MFAVTerm, #{allow_atom_cache_refs => true}),
            EncControl = vterm_encode:internal_vterm_to_binary(ControlVTerm1, #{allow_atom_cache_refs => true}),
            EncPayload = <<?LIST_EXT:8, 3:32, EncNode/bytes, EncMFA/bytes, EncRest1/bytes, ?NIL_EXT:8>>,
            EncExternal = <<EncHeaders/bytes, EncControl/bytes, EncPayload/bytes>>,
            {Channel, [{emit, EncExternal}]};
        {ok, Header = #vdist_pass_through_header{}, EncRest0} ->
            EncNode = dpi_resolve_node(Channel, Header),
            EncHeadersLength = byte_size(CompactFragment) - byte_size(EncRest0),
            EncHeaders = binary:part(CompactFragment, 0, EncHeadersLength),
            {ok, ControlVTerm0, <<?VERSION_MAGIC:8, EncRest1/bytes>>} = vterm_decode:external_binary_to_vterm(EncRest0),
            {ControlVTerm1, MFAVTerm} =
                case ControlVTerm0 of
                    {TupleRecord, Arity, [E0, E1, E2, E3, MFA0 | ERest]} when
                        (TupleRecord =:= vterm_small_tuple_ext orelse TupleRecord =:= vterm_large_tuple_ext) andalso
                            (Arity =:= 6 orelse Arity =:= 7)
                    ->
                        {{TupleRecord, Arity, [E0, E1, E2, E3, StaticMFA | ERest]}, MFA0}
                end,
            EncMFA = vterm_encode:internal_vterm_to_binary(MFAVTerm, #{allow_atom_cache_refs => false}),
            EncControl = vterm_encode:external_vterm_to_binary(ControlVTerm1, #{allow_atom_cache_refs => false}),
            EncPayload =
                <<?VERSION_MAGIC:8, ?LIST_EXT:8, 3:32, EncNode/bytes, EncMFA/bytes, EncRest1/bytes, ?NIL_EXT:8>>,
            EncExternal = <<EncHeaders/bytes, EncControl/bytes, EncPayload/bytes>>,
            {Channel, [{emit, EncExternal}]}
    end.

%% @private
dpi_resolve_atom(#vedf_channel{rx_atom_cache = AtomCache}, Header, Atom) when
    is_record(Header, vdist_fragment_header) orelse is_record(Header, vdist_normal_header)
->
    HeaderType = element(1, Header),
    {ok, _AtomCache, AtomTable} = HeaderType:update_atom_cache(Header, AtomCache),
    case vdist_atom_translation_table:rfind(AtomTable, Atom) of
        {ok, InternalIndex} ->
            AtomCacheRef = vterm_atom_cache_ref_resolved:new(InternalIndex, Atom),
            vterm_encode:internal_vterm_to_binary(AtomCacheRef, #{allow_atom_cache_refs => true});
        {error, not_found} ->
            AtomVTerm = vterm:expand(Atom),
            vterm_encode:internal_vterm_to_binary(AtomVTerm, #{allow_atom_cache_refs => false})
    end;
dpi_resolve_atom(#vedf_channel{}, #vdist_pass_through_header{}, Atom) ->
    AtomVTerm = vterm:expand(Atom),
    vterm_encode:internal_vterm_to_binary(AtomVTerm, #{allow_atom_cache_refs => false}).

%% @private
dpi_resolve_node(Channel = #vedf_channel{sysname = Sysname}, Header) ->
    dpi_resolve_atom(Channel, Header, Sysname).

%% @private
dpi_resolve_token(ControlVTerm) ->
    ControlDOP = vdist_dop:control_message_vterm_to_dop(ControlVTerm),
    case ControlDOP of
        #vdist_dop_exit_tt{trace_token = Token} ->
            {ok, Token};
        #vdist_dop_payload_exit_tt{trace_token = Token} ->
            {ok, Token};
        #vdist_dop_exit2_tt{trace_token = Token} ->
            {ok, Token};
        #vdist_dop_payload_exit2_tt{trace_token = Token} ->
            {ok, Token};
        #vdist_dop_alias_send_tt{token = Token} ->
            {ok, Token};
        #vdist_dop_reg_send_tt{trace_token = Token} ->
            {ok, Token};
        #vdist_dop_send_tt{trace_token = Token} ->
            {ok, Token};
        #vdist_dop_send_sender_tt{trace_token = Token} ->
            {ok, Token};
        #vdist_dop_spawn_reply_tt{token = Token} ->
            {ok, Token};
        #vdist_dop_spawn_request_tt{token = Token} ->
            {ok, Token};
        _ ->
            error
    end.

%% @private
dpi_resolve_u64(Val) when ?is_u64(Val) ->
    case Val of
        _ when Val < 256 ->
            SmallIntegerExt = vterm_small_integer_ext:new(Val),
            vterm_encode:internal_vterm_to_binary(SmallIntegerExt, #{});
        _ when Val < 2147483648 ->
            IntegerExt = vterm_integer_ext:new(Val),
            vterm_encode:internal_vterm_to_binary(IntegerExt, #{});
        _ ->
            vterm_small_big_ext:new(8, 0, <<Val:64/unsigned-big-integer-unit:1>>)
    end.

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
do_recv_sequence(Channel0, Entry0, FragmentsQueue0, Actions0) ->
    case queue:out(FragmentsQueue0) of
        {{value, Fragment}, FragmentsQueue1} ->
            Actions1 = [{emit, Fragment} | Actions0],
            case vdist_entry:decode(Entry0, Fragment) of
                {ok, _ControlMessage, _MaybePayload, Entry1, <<>>} ->
                    do_recv_sequence(Channel0, Entry1, FragmentsQueue1, Actions1);
                {cont, Entry1} ->
                    do_recv_sequence(Channel0, Entry1, FragmentsQueue1, Actions1)
            end;
        {empty, FragmentsQueue0} ->
            {ok, Channel0, Entry0, Actions0}
    end.

%% @private
emit_atom_cache_commit(Entry0 = #vdist_entry{}, Header0 = #vdist_fragment_header{}, Actions) ->
    {ControlMessage, Payload} = vdist_entry:reg_send_noop(),
    Header1 = Header0#vdist_fragment_header{fragment_id = 1},
    EncHeader = vdist_header_encode:encode_header(Header1),
    ControlMessageVTerm0 = vdist_dop:dop_to_control_message_vterm(ControlMessage),
    EncControlMessage = vterm_encode:internal_vterm_to_binary(ControlMessageVTerm0, #{allow_atom_cache_refs => false}),
    EncPayload = vterm_encode:internal_vterm_to_binary(Payload, #{allow_atom_cache_refs => false}),
    Encoded = <<EncHeader/bytes, EncControlMessage/bytes, EncPayload/bytes>>,
    case vdist_entry:decode(Entry0, Encoded) of
        {ok, _ControlMessage, _MaybePayload, Entry1, <<>>} ->
            {ok, [{emit, Encoded} | Actions], Entry1}
    end.

%% @private
maybe_emit_atom_cache_commit(
    Entry = #vdist_entry{}, Header = #vdist_fragment_header{atom_cache_ref_entries = AtomCacheRefEntries}, Actions
) ->
    HasAtomCacheWrites = lists:any(
        fun
            (#vdist_new_atom_cache_ref_entry{}) -> true;
            (_) -> false
        end,
        AtomCacheRefEntries
    ),
    case HasAtomCacheWrites of
        true ->
            emit_atom_cache_commit(Entry, Header, Actions);
        false ->
            {ok, Actions, Entry}
    end.

%% @private
maybe_emit_atom_cache_rollback(Entry = #vdist_entry{}, undefined, Actions) ->
    {ok, Actions, Entry};
maybe_emit_atom_cache_rollback(Entry = #vdist_entry{}, Header = #vdist_fragment_header{}, Actions) ->
    emit_atom_cache_rollback(Entry, Header, Actions).

emit_atom_cache_rollback(Entry0 = #vdist_entry{}, Header0 = #vdist_fragment_header{}, Actions) ->
    {ControlMessage, Payload} = vdist_entry:reg_send_noop(),
    Header1 = Header0#vdist_fragment_header{fragment_id = 1},
    EncHeader = vdist_header_encode:encode_header(Header1),
    ControlMessageVTerm0 = vdist_dop:dop_to_control_message_vterm(ControlMessage),
    EncControlMessage = vterm_encode:internal_vterm_to_binary(ControlMessageVTerm0, #{allow_atom_cache_refs => false}),
    EncPayload = vterm_encode:internal_vterm_to_binary(Payload, #{allow_atom_cache_refs => false}),
    Encoded = <<EncHeader/bytes, EncControlMessage/bytes, EncPayload/bytes>>,
    case vdist_entry:decode(Entry0, Encoded) of
        {ok, _ControlMessage, _MaybePayload, Entry1, <<>>} ->
            {ok, [{emit, Encoded} | Actions], Entry1}
    end.

%% @private
maybe_rewrite_fragment_header(
    #vdist_atom_cache{entries = ACEntries}, #vdist_atom_translation_table{entries = ATEntries}, FragmentHeader0
) ->
    case find_atom_cache_conflicts(ACEntries, maps:to_list(ATEntries), []) of
        [] ->
            % io:format("CASE 1: No atom cache conflicts.~n", []),
            {ok, FragmentHeader0, undefined};
        Conflicts = [_ | _] ->
            case vdist_header_decode:decode_header(FragmentHeader0) of
                {ok,
                    Header0 = #vdist_fragment_header{
                        sequence_id = SequenceId, atom_cache_ref_entries = AtomCacheRefEntries0, long_atoms = LongAtoms
                    },
                    FragmentHeaderTail} ->
                    case rewrite_atom_cache_entries(AtomCacheRefEntries0, Conflicts, 0, [], LongAtoms, [], false) of
                        {ok, AtomCacheRefEntries0, _RewriteLongAtoms, RollbackAtomCacheRefEntries, RollbackLongAtoms} ->
                            % No need to rewrite fragment header, but we will need a rollback.
                            % io:format("CASE 2: No need to rewrite fragment header, but we will need a rollback.~n", []),
                            % io:format("Rollback = ~p~n~n", [RollbackAtomCacheRefEntries]),
                            Rollback = vdist_fragment_header:new(
                                SequenceId,
                                1,
                                length(RollbackAtomCacheRefEntries),
                                RollbackAtomCacheRefEntries,
                                RollbackLongAtoms
                            ),
                            {ok, FragmentHeader0, Rollback};
                        {ok, AtomCacheRefEntries1, RewriteLongAtoms, RollbackAtomCacheRefEntries, RollbackLongAtoms} ->
                            % Rewrite fragment header and rollback.
                            % io:format("CASE 3: Rewrite fragment header and rollback.~n", []),
                            % io:format("DIFFERENCE~n~nAtomCacheRefEntries0 = ~p~n~nAtomCacheRefEntries1 = ~p~n~n", [AtomCacheRefEntries0, AtomCacheRefEntries1]),
                            Header1 = Header0#vdist_fragment_header{
                                atom_cache_ref_entries = AtomCacheRefEntries1, long_atoms = RewriteLongAtoms
                            },
                            FragmentHeader1 = vdist_header_encode:encode_header(Header1),
                            Rollback = vdist_fragment_header:new(
                                SequenceId,
                                1,
                                length(RollbackAtomCacheRefEntries),
                                RollbackAtomCacheRefEntries,
                                RollbackLongAtoms
                            ),
                            {ok, <<FragmentHeader1/bytes, FragmentHeaderTail/bytes>>, Rollback}
                    end
            end
    end.

rewrite_atom_cache_entries(
    [#vdist_old_atom_cache_ref_entry{atom_cache_index = CacheIndex} | AtomCacheRefEntries],
    [{Index, {CacheIndex, ConflictAtom, Atom}} | Conflicts],
    Index,
    Rewrite,
    RewriteLongAtoms,
    Rollback,
    RollbackLongAtoms
) ->
    AtomText = erlang:atom_to_binary(Atom),
    NewAtomCacheRefEntry = vdist_new_atom_cache_ref_entry:new(CacheIndex, AtomText),
    RollbackAtomText = erlang:atom_to_binary(ConflictAtom),
    RollbackNewAtomCacheRefEntry = vdist_new_atom_cache_ref_entry:new(CacheIndex, RollbackAtomText),
    rewrite_atom_cache_entries(
        AtomCacheRefEntries,
        Conflicts,
        Index + 1,
        [NewAtomCacheRefEntry | Rewrite],
        case byte_size(AtomText) > 255 of
            true -> true;
            false -> RewriteLongAtoms
        end,
        [RollbackNewAtomCacheRefEntry | Rollback],
        case byte_size(RollbackAtomText) > 255 of
            true -> true;
            false -> RollbackLongAtoms
        end
    );
rewrite_atom_cache_entries(
    [#vdist_new_atom_cache_ref_entry{atom_cache_index = CacheIndex, atom_text = AtomText} | AtomCacheRefEntries],
    [{Index, {CacheIndex, ConflictAtom, Atom}} | Conflicts],
    Index,
    Rewrite,
    RewriteLongAtoms,
    Rollback,
    RollbackLongAtoms
) ->
    AtomText = erlang:atom_to_binary(Atom),
    NewAtomCacheRefEntry = vdist_new_atom_cache_ref_entry:new(CacheIndex, AtomText),
    RollbackAtomText = erlang:atom_to_binary(ConflictAtom),
    RollbackNewAtomCacheRefEntry = vdist_new_atom_cache_ref_entry:new(CacheIndex, RollbackAtomText),
    rewrite_atom_cache_entries(
        AtomCacheRefEntries,
        Conflicts,
        Index + 1,
        [NewAtomCacheRefEntry | Rewrite],
        case byte_size(AtomText) > 255 of
            true -> true;
            false -> RewriteLongAtoms
        end,
        [RollbackNewAtomCacheRefEntry | Rollback],
        case byte_size(RollbackAtomText) > 255 of
            true -> true;
            false -> RollbackLongAtoms
        end
    );
rewrite_atom_cache_entries(
    [#vdist_new_atom_cache_ref_entry{atom_cache_index = CacheIndex} | AtomCacheRefEntries],
    Conflicts,
    Index,
    Rewrite,
    RewriteLongAtoms,
    Rollback,
    RollbackLongAtoms
) ->
    OldAtomCacheRefEntry = vdist_old_atom_cache_ref_entry:new(CacheIndex),
    rewrite_atom_cache_entries(
        AtomCacheRefEntries,
        Conflicts,
        Index + 1,
        [OldAtomCacheRefEntry | Rewrite],
        RewriteLongAtoms,
        Rollback,
        RollbackLongAtoms
    );
rewrite_atom_cache_entries(
    [OldAtomCacheRefEntry = #vdist_old_atom_cache_ref_entry{} | AtomCacheRefEntries],
    Conflicts,
    Index,
    Rewrite,
    RewriteLongAtoms,
    Rollback,
    RollbackLongAtoms
) ->
    rewrite_atom_cache_entries(
        AtomCacheRefEntries,
        Conflicts,
        Index + 1,
        [OldAtomCacheRefEntry | Rewrite],
        RewriteLongAtoms,
        Rollback,
        RollbackLongAtoms
    );
rewrite_atom_cache_entries(
    _AtomCacheRefEntries = [], _Conflicts = [], _Index, Rewrite, RewriteLongAtoms, Rollback, RollbackLongAtoms
) ->
    {ok, lists:reverse(Rewrite), RewriteLongAtoms, lists:reverse(Rollback), RollbackLongAtoms}.

%% @private
find_atom_cache_conflicts(_ACEntries, [], Conflicts) ->
    lists:sort(Conflicts);
find_atom_cache_conflicts(ACEntries, [{Index, {CacheIndex, Atom}} | ATEntries], Conflicts) ->
    case orddict:find(CacheIndex, ACEntries) of
        {ok, Atom} ->
            find_atom_cache_conflicts(ACEntries, ATEntries, Conflicts);
        {ok, ConflictAtom} ->
            find_atom_cache_conflicts(ACEntries, ATEntries, [{Index, {CacheIndex, ConflictAtom, Atom}} | Conflicts])
    end.
