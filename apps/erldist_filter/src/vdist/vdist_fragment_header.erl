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
-module(vdist_fragment_header).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(vdist_header_encode).

-include("erldist_filter.hrl").
% -include("erldist_filter_erts_dist.hrl").
-include("erldist_filter_erts_external.hrl").

%% API
-export([
    new/5,
    new_from_atom_cache_map/3,
    encode_header/1,
    update_atom_cache/2
]).

%% Types
-type t() :: #vdist_fragment_header{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(SequenceId, FragmentId, NumberOfAtomCacheRefs, AtomCacheRefEntries, LongAtoms) -> T when
    SequenceId :: vdist:sequence_id(),
    FragmentId :: vdist:fragment_id(),
    NumberOfAtomCacheRefs :: vterm:u8(),
    AtomCacheRefEntries :: [vdist:atom_cache_ref_entry()],
    LongAtoms :: boolean(),
    T :: t().
new(SequenceId, FragmentId, NumberOfAtomCacheRefs, AtomCacheRefEntries, LongAtoms) when
    ?is_u64(SequenceId) andalso ?is_u64(FragmentId) andalso ?is_u8(NumberOfAtomCacheRefs) andalso
        is_list(AtomCacheRefEntries) andalso length(AtomCacheRefEntries) =:= NumberOfAtomCacheRefs andalso
        is_boolean(LongAtoms)
->
    #vdist_fragment_header{
        sequence_id = SequenceId,
        fragment_id = FragmentId,
        number_of_atom_cache_refs = NumberOfAtomCacheRefs,
        atom_cache_ref_entries = AtomCacheRefEntries,
        long_atoms = LongAtoms
    }.

-spec new_from_atom_cache_map(SequenceId, FragmentId, CacheMap) -> T when
    SequenceId :: vdist:sequence_id(),
    FragmentId :: vdist:fragment_id(),
    CacheMap :: vdist_atom_cache_map:t(),
    T :: t().
new_from_atom_cache_map(SequenceId, FragmentId, CacheMap = #vdist_atom_cache_map{}) when
    ?is_u64(SequenceId) andalso ?is_u64(FragmentId)
->
    NumberOfAtomCacheRefs = vdist_atom_cache_map:get_number_of_atom_cache_refs(CacheMap),
    AtomCacheRefEntries = vdist_atom_cache_map:get_atom_cache_ref_entries(CacheMap),
    LongAtoms = vdist_atom_cache_map:get_long_atoms(CacheMap),
    % io:format("new_from_atom_cache_map -> ~p~n", [AtomCacheRefEntries]),
    new(SequenceId, FragmentId, NumberOfAtomCacheRefs, AtomCacheRefEntries, LongAtoms).

-spec encode_header(T) -> binary() when T :: t().
encode_header(#vdist_fragment_header{
    sequence_id = SequenceId,
    fragment_id = FragmentId,
    number_of_atom_cache_refs = 0,
    atom_cache_ref_entries = [],
    long_atoms = _LongAtoms
}) ->
    <<?VERSION_MAGIC:8, ?DIST_FRAG_HEADER:8, SequenceId:64, FragmentId:64, 0:8>>;
encode_header(#vdist_fragment_header{
    sequence_id = SequenceId,
    fragment_id = FragmentId,
    number_of_atom_cache_refs = NumberOfAtomCacheRefs,
    atom_cache_ref_entries = AtomCacheRefEntries,
    long_atoms = LongAtoms
}) ->
    Flags = vdist_normal_header:encode_header_flags(AtomCacheRefEntries, LongAtoms, <<>>),
    References = vdist_normal_header:encode_header_references(AtomCacheRefEntries, LongAtoms, <<>>),
    <<?VERSION_MAGIC:8, ?DIST_FRAG_HEADER:8, SequenceId:64, FragmentId:64, NumberOfAtomCacheRefs:8, Flags/bits,
        References/bits>>.

-spec update_atom_cache(T, Cache) -> {ok, Cache, Table} when
    T :: t(), Cache :: vdist_atom_cache:t() | undefined, Table :: vdist_atom_translation_table:t().
update_atom_cache(#vdist_fragment_header{number_of_atom_cache_refs = 0}, Cache = #vdist_atom_cache{}) ->
    Table = vdist_atom_translation_table:new(),
    {ok, Cache, Table};
update_atom_cache(#vdist_fragment_header{atom_cache_ref_entries = Entries}, Cache = #vdist_atom_cache{}) ->
    update_atom_cache(Entries, Cache, vdist_atom_translation_table:new()).

-spec update_atom_cache(Entries, Cache, Table) -> {ok, Cache, Table} when
    Entries :: [vdist_old_atom_cache_ref_entry:t()],
    Cache :: vdist_atom_cache:t(),
    Table :: vdist_atom_translation_table:t().
update_atom_cache([#vdist_old_atom_cache_ref_entry{atom_cache_index = CacheIndex} | Entries], Cache, Table0) ->
    % io:format("CacheIndex = ~p, Cache = ~p~n", [CacheIndex, Cache]),
    case vdist_atom_cache:find(Cache, CacheIndex) of
        {ok, {CacheIndex, Atom}} ->
            {ok, Table1} = vdist_atom_translation_table:store(Table0, CacheIndex, Atom),
            update_atom_cache(Entries, Cache, Table1)
        % {error, Reason} ->
        %     {error, Reason}
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
