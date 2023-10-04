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
%%% Created :  27 Mar 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(vdist_normal_header).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(vdist_header_encode).

-include("erldist_filter.hrl").
% -include("erldist_filter_erts_dist.hrl").
-include("erldist_filter_erts_external.hrl").

%% API
-export([
    new/3,
    new_from_atom_cache_map/1,
    encode_header/1,
    encode_header_flag/1,
    encode_header_flags/3,
    encode_header_long_atoms/1,
    encode_header_reference/2,
    encode_header_references/3,
    update_atom_cache/2
]).

%% Types
-type t() :: #vdist_normal_header{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(NumberOfAtomCacheRefs, AtomCacheRefEntries, LongAtoms) -> T when
    NumberOfAtomCacheRefs :: vterm:u8(),
    AtomCacheRefEntries :: [vdist:atom_cache_ref_entry()],
    LongAtoms :: boolean(),
    T :: t().
new(NumberOfAtomCacheRefs, AtomCacheRefEntries, LongAtoms) when
    ?is_u8(NumberOfAtomCacheRefs) andalso is_list(AtomCacheRefEntries) andalso
        length(AtomCacheRefEntries) =:= NumberOfAtomCacheRefs andalso is_boolean(LongAtoms)
->
    #vdist_normal_header{
        number_of_atom_cache_refs = NumberOfAtomCacheRefs,
        atom_cache_ref_entries = AtomCacheRefEntries,
        long_atoms = LongAtoms
    }.

-spec new_from_atom_cache_map(CacheMap) -> T when CacheMap :: vdist_atom_cache_map:t(), T :: t().
new_from_atom_cache_map(CacheMap = #vdist_atom_cache_map{}) ->
    NumberOfAtomCacheRefs = vdist_atom_cache_map:get_number_of_atom_cache_refs(CacheMap),
    AtomCacheRefEntries = vdist_atom_cache_map:get_atom_cache_ref_entries(CacheMap),
    LongAtoms = vdist_atom_cache_map:get_long_atoms(CacheMap),
    new(NumberOfAtomCacheRefs, AtomCacheRefEntries, LongAtoms).

-spec encode_header(T) -> binary() when T :: t().
encode_header(#vdist_normal_header{number_of_atom_cache_refs = 0, atom_cache_ref_entries = [], long_atoms = _LongAtoms}) ->
    <<?VERSION_MAGIC:8, ?DIST_HEADER:8, 0:8>>;
encode_header(#vdist_normal_header{
    number_of_atom_cache_refs = NumberOfAtomCacheRefs,
    atom_cache_ref_entries = AtomCacheRefEntries,
    long_atoms = LongAtoms
}) ->
    Flags = encode_header_flags(AtomCacheRefEntries, LongAtoms, <<>>),
    References = encode_header_references(AtomCacheRefEntries, LongAtoms, <<>>),
    <<?VERSION_MAGIC:8, ?DIST_HEADER:8, NumberOfAtomCacheRefs:8, Flags/bits, References/bits>>.

-spec encode_header_flag(Entry) -> bitstring() when Entry :: vdist:atom_cache_ref_entry().
encode_header_flag(Entry = #vdist_new_atom_cache_ref_entry{}) ->
    vdist_new_atom_cache_ref_entry:encode_header_flag(Entry);
encode_header_flag(Entry = #vdist_old_atom_cache_ref_entry{}) ->
    vdist_old_atom_cache_ref_entry:encode_header_flag(Entry).

-spec encode_header_flags(Entries, LongAtoms, PrevFlags) -> NextFlags when
    Entries :: [vdist:atom_cache_ref_entry()],
    LongAtoms :: boolean(),
    PrevFlags :: bitstring(),
    NextFlags :: bitstring().
encode_header_flags([], LongAtoms, <<>>) ->
    <<0:4, 0:3, (encode_header_long_atoms(LongAtoms))/bits>>;
encode_header_flags([], LongAtoms, PrevFlag = <<_:4>>) ->
    <<0:3, (encode_header_long_atoms(LongAtoms))/bits, PrevFlag/bits>>;
encode_header_flags([Entry | Entries], LongAtoms, <<>>) ->
    PrevFlag = encode_header_flag(Entry),
    encode_header_flags(Entries, LongAtoms, PrevFlag);
encode_header_flags([Entry | Entries], LongAtoms, PrevFlag = <<_:4>>) ->
    NextFlag = encode_header_flag(Entry),
    Flags = encode_header_flags(Entries, LongAtoms, <<>>),
    <<NextFlag/bits, PrevFlag/bits, Flags/bits>>.

-spec encode_header_long_atoms(LongAtoms) -> bitstring() when LongAtoms :: boolean().
encode_header_long_atoms(false) -> <<0:1>>;
encode_header_long_atoms(true) -> <<1:1>>.

-spec encode_header_reference(Entry, LongAtoms) -> bitstring() when
    Entry :: vdist:atom_cache_ref_entry(), LongAtoms :: boolean().
encode_header_reference(Entry = #vdist_new_atom_cache_ref_entry{}, LongAtoms) ->
    vdist_new_atom_cache_ref_entry:encode_header_reference(Entry, LongAtoms);
encode_header_reference(Entry = #vdist_old_atom_cache_ref_entry{}, LongAtoms) ->
    vdist_old_atom_cache_ref_entry:encode_header_reference(Entry, LongAtoms).

-spec encode_header_references(Entries, LongAtoms, PrevReferences) -> NextReferences when
    Entries :: [vdist:atom_cache_ref_entry()],
    LongAtoms :: boolean(),
    PrevReferences :: bitstring(),
    NextReferences :: bitstring().
encode_header_references([], _LongAtoms, Acc) ->
    Acc;
encode_header_references([Entry | Entries], LongAtoms, Acc) ->
    Reference = encode_header_reference(Entry, LongAtoms),
    encode_header_references(Entries, LongAtoms, <<Acc/bits, Reference/bits>>).

-spec update_atom_cache(Header, Cache) -> {ok, Cache, Table} | {error, not_found} when
    Header :: t(), Cache :: vdist_atom_cache:t(), Table :: vdist_atom_translation_table:t().
update_atom_cache(#vdist_normal_header{number_of_atom_cache_refs = 0}, Cache = #vdist_atom_cache{}) ->
    Table = vdist_atom_translation_table:new(),
    {ok, Cache, Table};
update_atom_cache(#vdist_normal_header{atom_cache_ref_entries = Entries}, Cache = #vdist_atom_cache{}) ->
    update_atom_cache(Entries, Cache, vdist_atom_translation_table:new()).

-spec update_atom_cache(Entries, Cache, Table) -> {ok, Cache, Table} | {error, not_found} when
    Entries :: [vdist:atom_cache_ref_entry()],
    Cache :: vdist_atom_cache:t(),
    Table :: vdist_atom_translation_table:t().
update_atom_cache([#vdist_old_atom_cache_ref_entry{atom_cache_index = CacheIndex} | Entries], Cache, Table0) ->
    case vdist_atom_cache:find(Cache, CacheIndex) of
        {ok, {CacheIndex, Atom}} ->
            {ok, Table1} = vdist_atom_translation_table:store(Table0, CacheIndex, Atom),
            update_atom_cache(Entries, Cache, Table1);
        {error, Reason} ->
            {error, Reason}
    end;
update_atom_cache(
    [#vdist_new_atom_cache_ref_entry{atom_cache_index = CacheIndex, atom_text = AtomText} | Entries], Cache0, Table0
) ->
    Atom = erlang:binary_to_atom(AtomText, utf8),
    case vdist_atom_cache:insert(Cache0, CacheIndex, Atom) of
        {ok, {CacheIndex, Atom}, Cache1} ->
            {ok, Table1} = vdist_atom_translation_table:store(Table0, CacheIndex, Atom),
            update_atom_cache(Entries, Cache1, Table1)
    end;
update_atom_cache([], Cache, Table) ->
    {ok, Cache, Table}.
