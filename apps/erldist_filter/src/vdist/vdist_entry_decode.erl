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
-module(vdist_entry_decode).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("erldist_filter/include/erldist_filter.hrl").
-include_lib("erldist_filter/include/erldist_filter_erts_dist.hrl").

%% API
-export([
    decode/2
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec decode(OldEntry, Input) -> {ok, ControlMessage, MaybePayload, NewEntry, Rest} | {cont, NewEntry} when
    OldEntry :: vdist_entry:t(),
    Input :: binary(),
    ControlMessage :: vdist:dop_t(),
    MaybePayload :: vterm:t() | undefined,
    NewEntry :: vdist_entry:t(),
    Rest :: binary().
decode(Entry = #vdist_entry{}, Input) when is_binary(Input) ->
    case vdist_header_decode:decode_header(Input) of
        {ok, Header = #vdist_fragment_header{}, EncControlMessage} ->
            do_decode_fragment_header(Entry, Header, EncControlMessage);
        {ok, Header = #vdist_fragment_cont{}, EncControlMessage} ->
            do_decode_fragment_cont(Entry, Header, EncControlMessage);
        {ok, Header = #vdist_normal_header{}, EncControlMessage} ->
            do_decode_normal_header(Entry, Header, EncControlMessage);
        {ok, Header = #vdist_pass_through_header{}, EncControlMessage} ->
            do_decode_pass_through_header(Entry, Header, EncControlMessage)
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-spec do_decode_fragment_header(
    OldEntry,
    Header,
    EncControlMessage
) -> {ok, ControlMessage, MaybePayload, NewEntry, Rest} | {cont, NewEntry} when
    OldEntry :: vdist_entry:t(),
    Header :: vdist_fragment_header:t(),
    EncControlMessage :: binary(),
    ControlMessage :: vdist:dop_t(),
    MaybePayload :: vterm:t() | undefined,
    NewEntry :: vdist_entry:t(),
    Rest :: binary().
do_decode_fragment_header(
    Entry0 = #vdist_entry{dflags = DFlags, rx_sequences = Sequences0, rx_atom_cache = AtomCache0},
    Header = #vdist_fragment_header{sequence_id = SequenceId, fragment_id = FragmentCount},
    Fragment
) when
    (DFlags band ?DFLAG_FRAGMENTS) =/= 0 andalso is_binary(Fragment)
->
    {ok, AtomCache1, AtomTable} = vdist_fragment_header:update_atom_cache(Header, AtomCache0),
    External = vdist_external:new(AtomTable, SequenceId, FragmentCount),
    case maps:is_key(SequenceId, Sequences0) of
        false ->
            FragmentId = FragmentCount,
            FragContHeader = vdist_fragment_cont:new(SequenceId, FragmentId),
            Sequences1 = Sequences0#{SequenceId => External},
            Entry1 = Entry0#vdist_entry{rx_sequences = Sequences1, rx_atom_cache = AtomCache1},
            do_decode_fragment_cont(Entry1, FragContHeader, Fragment)
    end.

-spec do_decode_fragment_cont(
    OldEntry,
    Header,
    EncControlMessage
) -> {ok, ControlMessage, MaybePayload, NewEntry, Rest} | {cont, NewEntry} when
    OldEntry :: vdist_entry:t(),
    Header :: vdist_fragment_cont:t(),
    EncControlMessage :: binary(),
    ControlMessage :: vdist:dop_t(),
    MaybePayload :: vterm:t() | undefined,
    NewEntry :: vdist_entry:t(),
    Rest :: binary().
do_decode_fragment_cont(
    Entry0 = #vdist_entry{dflags = DFlags, rx_sequences = Sequences0},
    Header = #vdist_fragment_cont{sequence_id = SequenceId},
    Fragment
) when (DFlags band ?DFLAG_FRAGMENTS) =/= 0 andalso is_binary(Fragment) ->
    case maps:find(SequenceId, Sequences0) of
        {ok, External0 = #vdist_external{}} ->
            case vdist_external:decode_next_fragment(External0, Header, Fragment) of
                {cont, External1} ->
                    Sequences1 = Sequences0#{SequenceId => External1},
                    Entry1 = Entry0#vdist_entry{rx_sequences = Sequences1},
                    {cont, Entry1};
                {ok, EncControlMessage, AtomTable = #vdist_atom_translation_table{}} ->
                    {ok, ControlMessageVTerm0, EncPayload} = vterm_decode:internal_binary_to_vterm(EncControlMessage),
                    {ControlMessageVTerm1, AtomTable} = vterm:xform(
                        ControlMessageVTerm0, AtomTable, fun xform_lookup_atoms/2
                    ),
                    ControlMessage = vdist_dop:control_message_vterm_to_dop(ControlMessageVTerm1),
                    Sequences1 = maps:remove(SequenceId, Sequences0),
                    Entry1 = Entry0#vdist_entry{rx_sequences = Sequences1},
                    case vdist_dop:dop_has_payload(ControlMessage) of
                        false ->
                            {ok, ControlMessage, undefined, Entry1, EncPayload};
                        true ->
                            {ok, PayloadVTerm0, Rest} = vterm_decode:internal_binary_to_vterm(EncPayload),
                            {Payload, AtomTable} = vterm:xform(PayloadVTerm0, AtomTable, fun xform_lookup_atoms/2),
                            {ok, ControlMessage, Payload, Entry1, Rest}
                    end
            end
    end.

-spec do_decode_normal_header(
    OldEntry,
    Header,
    EncControlMessage
) -> {ok, ControlMessage, MaybePayload, NewEntry, Rest} | {cont, NewEntry} when
    OldEntry :: vdist_entry:t(),
    Header :: vdist_normal_header:t(),
    EncControlMessage :: binary(),
    ControlMessage :: vdist:dop_t(),
    MaybePayload :: vterm:t() | undefined,
    NewEntry :: vdist_entry:t(),
    Rest :: binary().
do_decode_normal_header(
    Entry0 = #vdist_entry{dflags = DFlags, rx_atom_cache = AtomCache0}, Header, EncControlMessage
) when
    (DFlags band ?DFLAG_DIST_HDR_ATOM_CACHE) =/= 0, AtomCache0 =/= undefined
->
    {ok, AtomCache1, AtomTable} = vdist_normal_header:update_atom_cache(Header, AtomCache0),
    {ok, ControlMessageVTerm0, EncPayload} = vterm_decode:internal_binary_to_vterm(EncControlMessage),
    {ControlMessageVTerm1, AtomTable} = vterm:xform(ControlMessageVTerm0, AtomTable, fun xform_lookup_atoms/2),
    ControlMessage = vdist_dop:control_message_vterm_to_dop(ControlMessageVTerm1),
    Entry1 = Entry0#vdist_entry{rx_atom_cache = AtomCache1},
    case vdist_dop:dop_has_payload(ControlMessage) of
        false ->
            {ok, ControlMessage, undefined, Entry1, EncPayload};
        true ->
            {ok, PayloadVTerm0, Rest} = vterm_decode:internal_binary_to_vterm(EncPayload),
            {Payload, AtomTable} = vterm:xform(PayloadVTerm0, AtomTable, fun xform_lookup_atoms/2),
            {ok, ControlMessage, Payload, Entry1, Rest}
    end.

-spec do_decode_pass_through_header(
    OldEntry,
    Header,
    EncControlMessage
) -> {ok, ControlMessage, MaybePayload, NewEntry, Rest} | {cont, NewEntry} when
    OldEntry :: vdist_entry:t(),
    Header :: vdist_pass_through_header:t(),
    EncControlMessage :: binary(),
    ControlMessage :: vdist:dop_t(),
    MaybePayload :: vterm:t() | undefined,
    NewEntry :: vdist_entry:t(),
    Rest :: binary().
do_decode_pass_through_header(Entry = #vdist_entry{rx_atom_cache = undefined}, _Header, EncControlMessage) ->
    {ok, ControlMessageVTerm, EncPayload} = vterm_decode:external_binary_to_vterm(EncControlMessage),
    ControlMessage = vdist_dop:control_message_vterm_to_dop(ControlMessageVTerm),
    case vdist_dop:dop_has_payload(ControlMessage) of
        false ->
            {ok, ControlMessage, undefined, Entry, EncPayload};
        true ->
            {ok, Payload, Rest} = vterm_decode:external_binary_to_vterm(EncPayload),
            {ok, ControlMessage, Payload, Entry, Rest}
    end.

-spec xform_lookup_atoms(
    VTerm :: vterm:t(),
    AtomTable :: vdist_atom_translation_table:t()
) -> {cont, vterm_atom_cache_ref_resolved:t(), vdist_atom_translation_table:t()} | cont.
xform_lookup_atoms(#vterm_atom_cache_ref{index = InternalIndex}, AtomTable) ->
    case vdist_atom_translation_table:find(AtomTable, InternalIndex) of
        {ok, Atom} ->
            AtomCacheRef = vterm_atom_cache_ref_resolved:new(InternalIndex, Atom),
            {cont, AtomCacheRef, AtomTable};
        {error, not_found} ->
            % NOTE: maybe error here?
            cont
    end;
xform_lookup_atoms(_VTerm, _AtomTable) ->
    cont.
