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
-module(vdist_header_decode).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("erldist_filter/include/erldist_filter.hrl").
-include_lib("erldist_filter/include/erldist_filter_erts_external.hrl").

%% API
-export([
    decode_header/1
]).

-type header() ::
    #vdist_normal_header{} | #vdist_fragment_header{} | #vdist_fragment_cont{} | #vdist_pass_through_header{}.

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec decode_header(bitstring()) -> {ok, header(), bitstring()}.
decode_header(EncodedHeader) ->
    case EncodedHeader of
        <<?VERSION_MAGIC:8, ?DIST_HEADER:8, NumberOfAtomCacheRefs:8, Rest/bits>> when NumberOfAtomCacheRefs =:= 0 ->
            {ok, vdist_normal_header:new(NumberOfAtomCacheRefs, [], false), Rest};
        <<?VERSION_MAGIC:8, ?DIST_HEADER:8, NumberOfAtomCacheRefs:8, RawEncodedFlags/bits>> ->
            FlagBytes = ?ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTES(NumberOfAtomCacheRefs),
            ByteIx = ?ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTE_IX(NumberOfAtomCacheRefs),
            BitIx = ?ERTS_DIST_HDR_ATOM_CACHE_FLAG_BIT_IX(NumberOfAtomCacheRefs),
            <<EncodedFlags:FlagBytes/bytes, EncodedReferences/bytes>> = RawEncodedFlags,
            LongAtoms =
                case binary:at(EncodedFlags, ByteIx) band (?ERTS_DIST_HDR_LONG_ATOMS_FLG bsl BitIx) of
                    0 -> false;
                    _ -> true
                end,
            {ok, AtomCacheRefEntries, Rest} = decode_atom_cache_ref_entries(
                NumberOfAtomCacheRefs, LongAtoms, EncodedFlags, EncodedReferences, []
            ),
            {ok, vdist_normal_header:new(NumberOfAtomCacheRefs, AtomCacheRefEntries, LongAtoms), Rest};
        <<?VERSION_MAGIC:8, ?DIST_FRAG_HEADER:8, SequenceId:64, FragmentId:64, NumberOfAtomCacheRefs:8, Rest/bits>> when
            NumberOfAtomCacheRefs =:= 0
        ->
            {ok, vdist_fragment_header:new(SequenceId, FragmentId, NumberOfAtomCacheRefs, [], false), Rest};
        <<?VERSION_MAGIC:8, ?DIST_FRAG_HEADER:8, SequenceId:64, FragmentId:64, NumberOfAtomCacheRefs:8,
            RawEncodedFlags/bits>> ->
            FlagBytes = ?ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTES(NumberOfAtomCacheRefs),
            ByteIx = ?ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTE_IX(NumberOfAtomCacheRefs),
            BitIx = ?ERTS_DIST_HDR_ATOM_CACHE_FLAG_BIT_IX(NumberOfAtomCacheRefs),
            <<EncodedFlags:FlagBytes/bytes, EncodedReferences/bytes>> = RawEncodedFlags,
            LongAtoms =
                case binary:at(EncodedFlags, ByteIx) band (?ERTS_DIST_HDR_LONG_ATOMS_FLG bsl BitIx) of
                    0 -> false;
                    _ -> true
                end,
            {ok, AtomCacheRefEntries, Rest} = decode_atom_cache_ref_entries(
                NumberOfAtomCacheRefs, LongAtoms, EncodedFlags, EncodedReferences, []
            ),
            {ok,
                vdist_fragment_header:new(
                    SequenceId, FragmentId, NumberOfAtomCacheRefs, AtomCacheRefEntries, LongAtoms
                ),
                Rest};
        <<?VERSION_MAGIC:8, ?DIST_FRAG_CONT:8, SequenceId:64, FragmentId:64, Rest/bits>> ->
            {ok, vdist_fragment_cont:new(SequenceId, FragmentId), Rest};
        <<?PASS_THROUGH:8, Rest/bits>> ->
            {ok, vdist_pass_through_header:new(), Rest}
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-spec decode_atom_cache_ref_entry(
    NewCacheEntryFlag :: 0 | 1,
    SegmentIndex :: non_neg_integer(),
    LOngAtoms :: boolean(),
    EncodedReferences :: bitstring()
) -> {ok, vdist:atom_cache_ref_entry(), bitstring()} | {error, term()}.
decode_atom_cache_ref_entry(
    _NewCacheEntryFlag = 1,
    SegmentIndex,
    _LongAtoms = false,
    <<InternalSegmentIndex:8, Length:8, AtomText:Length/bytes, Rest/bits>>
) ->
    AtomCacheIndex = ((SegmentIndex band 7) bsl 8) + InternalSegmentIndex,
    {ok, vdist_new_atom_cache_ref_entry:new(AtomCacheIndex, AtomText), Rest};
decode_atom_cache_ref_entry(
    _NewCacheEntryFlag = 1,
    SegmentIndex,
    _LongAtoms = true,
    <<InternalSegmentIndex:8, Length:16, AtomText:Length/bytes, Rest/bits>>
) ->
    AtomCacheIndex = ((SegmentIndex band 7) bsl 8) + InternalSegmentIndex,
    {ok, vdist_new_atom_cache_ref_entry:new(AtomCacheIndex, AtomText), Rest};
decode_atom_cache_ref_entry(_NewCacheEntryFlag = 0, SegmentIndex, _LongAtoms, <<InternalSegmentIndex:8, Rest/bits>>) ->
    AtomCacheIndex = ((SegmentIndex band 7) bsl 8) + InternalSegmentIndex,
    {ok, vdist_old_atom_cache_ref_entry:new(AtomCacheIndex), Rest}.

-spec decode_atom_cache_ref_entries(
    NumberOfAtomCacheRefs :: integer(),
    LongAtoms :: boolean(),
    EncodedFlags :: bitstring(),
    EncodedReferences :: bitstring(),
    [vdist:atom_cache_ref_entry()]
) -> {ok, [vdist:atom_cache_ref_entry()], bitstring()} | {error, term()}.
decode_atom_cache_ref_entries(
    _NumberOfAtomCacheRefs = 1,
    LongAtoms,
    <<_CurrentlyUnused:3, RawLongAtoms:1, NewCacheEntryFlagA:1, SegmentIndexA:3>>,
    EncodedReferences0,
    Entries
) when (LongAtoms =:= true andalso RawLongAtoms =:= 1) orelse (LongAtoms =:= false andalso RawLongAtoms =:= 0) ->
    {ok, EntryA, Rest} = decode_atom_cache_ref_entry(NewCacheEntryFlagA, SegmentIndexA, LongAtoms, EncodedReferences0),
    {ok, lists:reverse([EntryA | Entries]), Rest};
decode_atom_cache_ref_entries(
    _NumberOfAtomCacheRefs = 2,
    LongAtoms,
    <<NewCacheEntryFlagB:1, SegmentIndexB:3, NewCacheEntryFlagA:1, SegmentIndexA:3, _CurrentlyUnused:7,
        RawLongAtoms:1>>,
    EncodedReferences0,
    Entries
) when (LongAtoms =:= true andalso RawLongAtoms =:= 1) orelse (LongAtoms =:= false andalso RawLongAtoms =:= 0) ->
    {ok, EntryA, EncodedReferences1} = decode_atom_cache_ref_entry(
        NewCacheEntryFlagA, SegmentIndexA, LongAtoms, EncodedReferences0
    ),
    {ok, EntryB, Rest} = decode_atom_cache_ref_entry(NewCacheEntryFlagB, SegmentIndexB, LongAtoms, EncodedReferences1),
    {ok, lists:reverse([EntryB, EntryA | Entries]), Rest};
decode_atom_cache_ref_entries(
    NumberOfAtomCacheRefs,
    LongAtoms,
    <<NewCacheEntryFlagB:1, SegmentIndexB:3, NewCacheEntryFlagA:1, SegmentIndexA:3, EncodedFlags/bits>>,
    EncodedReferences0,
    Entries
) when NumberOfAtomCacheRefs > 2 ->
    {ok, EntryA, EncodedReferences1} = decode_atom_cache_ref_entry(
        NewCacheEntryFlagA, SegmentIndexA, LongAtoms, EncodedReferences0
    ),
    {ok, EntryB, EncodedReferences2} = decode_atom_cache_ref_entry(
        NewCacheEntryFlagB, SegmentIndexB, LongAtoms, EncodedReferences1
    ),
    decode_atom_cache_ref_entries(NumberOfAtomCacheRefs - 2, LongAtoms, EncodedFlags, EncodedReferences2, [
        EntryB, EntryA | Entries
    ]).
