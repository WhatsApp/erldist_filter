%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% Created :  27 Mar 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
-module(vdist_entry_encode).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("erldist_filter/include/erldist_filter.hrl").
-include_lib("erldist_filter/include/erldist_filter_erts_dist.hrl").

%% API
-export([
    encode/3,
    encode_with_fragment_header/3,
    encode_with_normal_header/3,
    encode_with_pass_through_header/3,
    encode/4,
    encode_with_fragment_header/4,
    encode_with_normal_header/4,
    encode_with_pass_through_header/4
]).

%% Types
-type options() :: #{
    fragment_size => pos_integer(),
    header_mode => fragment | normal | pass_through
}.

-export_type([
    options/0
]).

%% Macros
% -define(DEFAULT_FRAGMENT_SIZE, 16#F).
% -define(DEFAULT_FRAGMENT_SIZE, 16#FF).
-define(DEFAULT_FRAGMENT_SIZE, 16#FFFF).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec encode(Entry, ControlMessage, Options) -> {ok, Fragments, Entry} when
    Entry :: vdist_entry:t(),
    ControlMessage :: vdist:dop_without_payload_t(),
    Options :: options(),
    Fragments :: [binary()].
encode(Entry = #vdist_entry{}, ControlMessage, Options = #{header_mode := HeaderMode}) ->
    case HeaderMode of
        fragment ->
            encode_with_fragment_header(Entry, ControlMessage, Options);
        normal ->
            encode_with_normal_header(Entry, ControlMessage, Options);
        pass_through ->
            encode_with_pass_through_header(Entry, ControlMessage, Options)
    end;
encode(Entry = #vdist_entry{dflags = DFlags}, ControlMessage, Options) when
    (DFlags band ?DFLAG_FRAGMENTS) =/= 0 andalso ?is_vdist_dop_without_payload_t(ControlMessage) andalso is_map(Options)
->
    encode_with_fragment_header(Entry, ControlMessage, Options);
encode(Entry = #vdist_entry{dflags = DFlags}, ControlMessage, Options) when
    (DFlags band ?DFLAG_DIST_HDR_ATOM_CACHE) =/= 0 andalso ?is_vdist_dop_without_payload_t(ControlMessage) andalso
        is_map(Options)
->
    encode_with_normal_header(Entry, ControlMessage, Options);
encode(Entry = #vdist_entry{tx_atom_cache = undefined}, ControlMessage, Options) when
    ?is_vdist_dop_without_payload_t(ControlMessage) andalso is_map(Options)
->
    encode_with_pass_through_header(Entry, ControlMessage, Options).

-spec encode_with_fragment_header(Entry, ControlMessage, Options) -> {ok, Fragments, Entry} when
    Entry :: vdist_entry:t(),
    ControlMessage :: vdist:dop_without_payload_t(),
    Options :: options(),
    Fragments :: [binary()].
encode_with_fragment_header(
    Entry0 = #vdist_entry{dflags = DFlags, tx_atom_cache = AtomCache0}, ControlMessage, Options
) when
    (DFlags band ?DFLAG_FRAGMENTS) =/= 0 andalso ?is_vdist_dop_without_payload_t(ControlMessage) andalso is_map(Options) andalso
        AtomCache0 =/= undefined
->
    SequenceId = vdist_dop:dop_sequence_id(ControlMessage),
    CacheMap0 = vdist_atom_cache_map:new(AtomCache0),
    ControlMessageVTerm0 = vdist_dop:dop_to_control_message_vterm(ControlMessage),
    {ControlMessageVTerm1, CacheMap1} = vterm:xform(ControlMessageVTerm0, CacheMap0, fun xform_cache_atoms/2),
    EncControlMessage = vterm_encode:internal_vterm_to_binary(ControlMessageVTerm1, #{allow_atom_cache_refs => true}),
    FragmentId = 1,
    Header = vdist_fragment_header:new_from_atom_cache_map(SequenceId, FragmentId, CacheMap1),
    EncHeader = vdist_header_encode:encode_header(Header),
    AtomCache1 = vdist_atom_cache_map:get_atom_cache(CacheMap1),
    Entry1 = Entry0#vdist_entry{tx_atom_cache = AtomCache1},
    Encoded = <<EncHeader/bytes, EncControlMessage/bytes>>,
    {ok, [Encoded], Entry1}.

-spec encode_with_normal_header(Entry, ControlMessage, Options) -> {ok, Fragments, Entry} when
    Entry :: vdist_entry:t(),
    ControlMessage :: vdist:dop_without_payload_t(),
    Options :: options(),
    Fragments :: [binary()].
encode_with_normal_header(
    Entry0 = #vdist_entry{dflags = DFlags, tx_atom_cache = AtomCache0}, ControlMessage, Options
) when
    (DFlags band ?DFLAG_DIST_HDR_ATOM_CACHE) =/= 0 andalso ?is_vdist_dop_without_payload_t(ControlMessage) andalso
        is_map(Options) andalso AtomCache0 =/= undefined
->
    CacheMap0 = vdist_atom_cache_map:new(AtomCache0),
    ControlMessageVTerm0 = vdist_dop:dop_to_control_message_vterm(ControlMessage),
    {ControlMessageVTerm1, CacheMap1} = vterm:xform(ControlMessageVTerm0, CacheMap0, fun xform_cache_atoms/2),
    Header = vdist_normal_header:new_from_atom_cache_map(CacheMap1),
    EncHeader = vdist_header_encode:encode_header(Header),
    EncControlMessage = vterm_encode:internal_vterm_to_binary(ControlMessageVTerm1, #{allow_atom_cache_refs => true}),
    AtomCache1 = vdist_atom_cache_map:get_atom_cache(CacheMap1),
    Entry1 = Entry0#vdist_entry{tx_atom_cache = AtomCache1},
    Encoded = <<EncHeader/bytes, EncControlMessage/bytes>>,
    {ok, [Encoded], Entry1}.

-spec encode_with_pass_through_header(Entry, ControlMessage, Options) -> {ok, Fragments, Entry} when
    Entry :: vdist_entry:t(),
    ControlMessage :: vdist:dop_without_payload_t(),
    Options :: options(),
    Fragments :: [binary()].
encode_with_pass_through_header(
    Entry = #vdist_entry{dflags = DFlags, tx_atom_cache = undefined}, ControlMessage, Options
) when
    (DFlags band (?DFLAG_DIST_HDR_ATOM_CACHE bor ?DFLAG_FRAGMENTS)) =:= 0 andalso
        ?is_vdist_dop_without_payload_t(ControlMessage) andalso is_map(Options)
->
    Header = vdist_pass_through_header:new(),
    ControlMessageVTerm = vdist_dop:dop_to_control_message_vterm(ControlMessage),
    EncHeader = vdist_header_encode:encode_header(Header),
    EncControlMessage = vterm_encode:external_vterm_to_binary(ControlMessageVTerm, #{}),
    Encoded = <<EncHeader/bytes, EncControlMessage/bytes>>,
    {ok, [Encoded], Entry}.

-spec encode(Entry, ControlMessage, Payload, Options) -> {ok, Fragments, Entry} when
    Entry :: vdist_entry:t(),
    ControlMessage :: vdist:dop_with_payload_t(),
    Payload :: vterm:t(),
    Options :: options(),
    Fragments :: [binary()].
encode(Entry = #vdist_entry{}, ControlMessage, Payload, Options = #{header_mode := HeaderMode}) ->
    case HeaderMode of
        fragment ->
            encode_with_fragment_header(Entry, ControlMessage, Payload, Options);
        normal ->
            encode_with_normal_header(Entry, ControlMessage, Payload, Options);
        pass_through ->
            encode_with_pass_through_header(Entry, ControlMessage, Payload, Options)
    end;
encode(Entry = #vdist_entry{dflags = DFlags}, ControlMessage, Payload, Options) when
    (DFlags band ?DFLAG_FRAGMENTS) =/= 0 andalso ?is_vdist_dop_with_payload_t(ControlMessage) andalso
        ?is_vterm_t(Payload) andalso is_map(Options)
->
    encode_with_fragment_header(Entry, ControlMessage, Payload, Options);
encode(Entry = #vdist_entry{dflags = DFlags}, ControlMessage, Payload, Options) when
    (DFlags band ?DFLAG_DIST_HDR_ATOM_CACHE) =/= 0 andalso ?is_vdist_dop_with_payload_t(ControlMessage) andalso
        ?is_vterm_t(Payload) andalso is_map(Options)
->
    encode_with_normal_header(Entry, ControlMessage, Payload, Options);
encode(Entry = #vdist_entry{dflags = DFlags, tx_atom_cache = undefined}, ControlMessage, Payload, Options) when
    (DFlags band (?DFLAG_DIST_HDR_ATOM_CACHE bor ?DFLAG_FRAGMENTS)) =:= 0 andalso
        ?is_vdist_dop_with_payload_t(ControlMessage) andalso ?is_vterm_t(Payload) andalso is_map(Options)
->
    encode_with_pass_through_header(Entry, ControlMessage, Payload, Options).

-spec encode_with_fragment_header(Entry, ControlMessage, Payload, Options) -> {ok, Fragments, Entry} when
    Entry :: vdist_entry:t(),
    ControlMessage :: vdist:dop_with_payload_t(),
    Payload :: vterm:t(),
    Options :: options(),
    Fragments :: [binary()].
encode_with_fragment_header(
    Entry0 = #vdist_entry{dflags = DFlags, tx_atom_cache = AtomCache0}, ControlMessage, Payload, Options
) when
    (DFlags band ?DFLAG_FRAGMENTS) =/= 0 andalso ?is_vdist_dop_with_payload_t(ControlMessage) andalso
        ?is_vterm_t(Payload) andalso is_map(Options) andalso AtomCache0 =/= undefined
->
    FragmentSize = maps:get(fragment_size, Options, ?DEFAULT_FRAGMENT_SIZE),
    SequenceId = vdist_dop:dop_sequence_id(ControlMessage),
    CacheMap0 = vdist_atom_cache_map:new(AtomCache0),
    ControlMessageVTerm0 = vdist_dop:dop_to_control_message_vterm(ControlMessage),
    {ControlMessageVTerm1, CacheMap1} = vterm:xform(ControlMessageVTerm0, CacheMap0, fun xform_cache_atoms/2),
    {PayloadVTerm0, CacheMap2} = vterm:xform(Payload, CacheMap1, fun xform_cache_atoms/2),
    EncControlMessage = vterm_encode:internal_vterm_to_binary(ControlMessageVTerm1, #{allow_atom_cache_refs => true}),
    EncPayload = vterm_encode:internal_vterm_to_binary(PayloadVTerm0, #{allow_atom_cache_refs => true}),
    ControlMessageLen = byte_size(EncControlMessage),
    PayloadLen = byte_size(EncPayload),
    TotalLen = ControlMessageLen + PayloadLen,
    case TotalLen of
        _ when TotalLen =< FragmentSize ->
            FragmentId = 1,
            Header = vdist_fragment_header:new_from_atom_cache_map(SequenceId, FragmentId, CacheMap2),
            EncHeader = vdist_header_encode:encode_header(Header),
            AtomCache1 = vdist_atom_cache_map:get_atom_cache(CacheMap2),
            Entry1 = Entry0#vdist_entry{tx_atom_cache = AtomCache1},
            Encoded = <<EncHeader/bytes, EncControlMessage/bytes, EncPayload/bytes>>,
            {ok, [Encoded], Entry1};
        _ when ControlMessageLen < FragmentSize ->
            MessageHeadLen = FragmentSize - ControlMessageLen,
            <<EncPayloadHead:MessageHeadLen/bytes, EncPayloadTail/bytes>> = EncPayload,
            FirstFragment = <<EncControlMessage/bytes, EncPayloadHead/bytes>>,
            FirstFragmentId =
                1 + (byte_size(EncPayloadTail) div FragmentSize) +
                    (case byte_size(EncPayloadTail) rem FragmentSize of
                        0 -> 0;
                        _ -> 1
                    end),
            FirstHeader = vdist_fragment_header:new_from_atom_cache_map(SequenceId, FirstFragmentId, CacheMap2),
            EncFirstHeader = vdist_header_encode:encode_header(FirstHeader),
            EncFirstFragment = <<EncFirstHeader/bytes, FirstFragment/bytes>>,
            AtomCache1 = vdist_atom_cache_map:get_atom_cache(CacheMap2),
            Entry1 = Entry0#vdist_entry{tx_atom_cache = AtomCache1},
            do_encode_fragments(Entry1, FragmentSize, SequenceId, FirstFragmentId - 1, EncPayloadTail, [
                EncFirstFragment
            ]);
        _ ->
            FirstFragment = <<EncControlMessage/bytes>>,
            FirstFragmentId =
                1 + (PayloadLen div FragmentSize) +
                    (case PayloadLen rem FragmentSize of
                        0 -> 0;
                        _ -> 1
                    end),
            FirstHeader = vdist_fragment_header:new_from_atom_cache_map(SequenceId, FirstFragmentId, CacheMap2),
            EncFirstHeader = vdist_header_encode:encode_header(FirstHeader),
            EncFirstFragment = <<EncFirstHeader/bytes, FirstFragment/bytes>>,
            AtomCache1 = vdist_atom_cache_map:get_atom_cache(CacheMap2),
            Entry1 = Entry0#vdist_entry{tx_atom_cache = AtomCache1},
            do_encode_fragments(Entry1, FragmentSize, SequenceId, FirstFragmentId - 1, EncPayload, [EncFirstFragment])
    end.

-spec encode_with_normal_header(Entry, ControlMessage, Payload, Options) -> {ok, Fragments, Entry} when
    Entry :: vdist_entry:t(),
    ControlMessage :: vdist:dop_with_payload_t(),
    Payload :: vterm:t(),
    Options :: options(),
    Fragments :: [binary()].
encode_with_normal_header(
    Entry0 = #vdist_entry{dflags = DFlags, tx_atom_cache = AtomCache0}, ControlMessage, Payload, Options
) when
    (DFlags band ?DFLAG_DIST_HDR_ATOM_CACHE) =/= 0 andalso ?is_vdist_dop_with_payload_t(ControlMessage) andalso
        ?is_vterm_t(Payload) andalso is_map(Options) andalso AtomCache0 =/= undefined
->
    CacheMap0 = vdist_atom_cache_map:new(AtomCache0),
    ControlMessageVTerm0 = vdist_dop:dop_to_control_message_vterm(ControlMessage),
    {ControlMessageVTerm1, CacheMap1} = vterm:xform(ControlMessageVTerm0, CacheMap0, fun xform_cache_atoms/2),
    {PayloadVTerm0, CacheMap2} = vterm:xform(Payload, CacheMap1, fun xform_cache_atoms/2),
    Header = vdist_normal_header:new_from_atom_cache_map(CacheMap2),
    EncHeader = vdist_header_encode:encode_header(Header),
    EncControlMessage = vterm_encode:internal_vterm_to_binary(ControlMessageVTerm1, #{allow_atom_cache_refs => true}),
    EncPayload = vterm_encode:internal_vterm_to_binary(PayloadVTerm0, #{allow_atom_cache_refs => true}),
    AtomCache1 = vdist_atom_cache_map:get_atom_cache(CacheMap2),
    Entry1 = Entry0#vdist_entry{tx_atom_cache = AtomCache1},
    Encoded = <<EncHeader/bytes, EncControlMessage/bytes, EncPayload/bytes>>,
    {ok, [Encoded], Entry1}.

-spec encode_with_pass_through_header(Entry, ControlMessage, Payload, Options) -> {ok, Fragments, Entry} when
    Entry :: vdist_entry:t(),
    ControlMessage :: vdist:dop_with_payload_t(),
    Payload :: vterm:t(),
    Options :: options(),
    Fragments :: [binary()].
encode_with_pass_through_header(
    Entry = #vdist_entry{dflags = DFlags, tx_atom_cache = undefined}, ControlMessage, Payload, Options
) when
    (DFlags band (?DFLAG_DIST_HDR_ATOM_CACHE bor ?DFLAG_FRAGMENTS)) =:= 0 andalso
        ?is_vdist_dop_with_payload_t(ControlMessage) andalso ?is_vterm_t(Payload) andalso is_map(Options)
->
    Header = vdist_pass_through_header:new(),
    ControlMessageVTerm = vdist_dop:dop_to_control_message_vterm(ControlMessage),
    EncHeader = vdist_header_encode:encode_header(Header),
    EncControlMessage = vterm_encode:external_vterm_to_binary(ControlMessageVTerm, #{}),
    EncPayload = vterm_encode:external_vterm_to_binary(Payload, #{}),
    Encoded = <<EncHeader/bytes, EncControlMessage/bytes, EncPayload/bytes>>,
    {ok, [Encoded], Entry}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-spec do_encode_fragments(Entry, FragmentSize, SequenceId, FragmentId, EncPayload, Fragments) ->
    {ok, Fragments, Entry}
when
    Entry :: vdist_entry:t(),
    FragmentSize :: pos_integer(),
    SequenceId :: vdist:sequence_id(),
    FragmentId :: non_neg_integer(),
    EncPayload :: binary(),
    Fragments :: [binary()].
do_encode_fragments(Entry, _FragmentSize, SequenceId, FragmentId = 1, EncPayload, Fragments) ->
    NextHeader = vdist_fragment_cont:new(SequenceId, FragmentId),
    EncNextHeader = vdist_header_encode:encode_header(NextHeader),
    NextFragment = <<EncNextHeader/bytes, EncPayload/bytes>>,
    {ok, lists:reverse([NextFragment | Fragments]), Entry};
do_encode_fragments(Entry, FragmentSize, SequenceId, FragmentId, EncPayload, Fragments) when
    byte_size(EncPayload) > FragmentSize
->
    NextHeader = vdist_fragment_cont:new(SequenceId, FragmentId),
    EncNextHeader = vdist_header_encode:encode_header(NextHeader),
    <<EncPayloadHead:FragmentSize/bytes, EncPayloadTail/bytes>> = EncPayload,
    NextFragment = <<EncNextHeader/bytes, EncPayloadHead/bytes>>,
    do_encode_fragments(Entry, FragmentSize, SequenceId, FragmentId - 1, EncPayloadTail, [NextFragment | Fragments]).

-spec xform_cache_atoms(VTerm, CacheMap) -> VTermCont when
    VTerm :: vterm:t(),
    CacheMap :: vdist_atom_cache_map:t(),
    VTermCont :: cont | {cont, VTerm, vdist_atom_cache_map:t()}.
xform_cache_atoms(AtomVTerm, CacheMap0) when ?is_vterm_atom_t(AtomVTerm) ->
    Atom = vterm:simplify(AtomVTerm),
    case vdist_atom_cache_map:find_or_insert(CacheMap0, Atom) of
        {ok, InternalIndex, CacheMap1} ->
            AtomCacheRef = vterm_atom_cache_ref_resolved:new(InternalIndex, Atom),
            {cont, AtomCacheRef, CacheMap1};
        {error, _Reason} ->
            cont
    end;
xform_cache_atoms(_VTerm, _CacheMap) ->
    cont.
