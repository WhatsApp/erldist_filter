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
-module(vdist_atom_cache_map).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("erldist_filter/include/erldist_filter.hrl").
-include_lib("erldist_filter/include/erldist_filter_erts_external.hrl").

%% API
-export([
    new/1,
    find_or_insert/2,
    get_atom_cache/1,
    get_atom_cache_ref_entries/1,
    get_long_atoms/1,
    get_number_of_atom_cache_refs/1
]).

%% Types
% -type index() :: 0..?ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES.
-type index() :: 0..255.
-type t() :: #vdist_atom_cache_map{}.

-export_type([
    index/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Cache) -> CacheMap when Cache :: vdist_atom_cache:t(), CacheMap :: t().
new(Cache = #vdist_atom_cache{}) ->
    #vdist_atom_cache_map{cache = Cache}.

-spec find_or_insert(CacheMap, Atom) -> {ok, InternalIndex, CacheMap} | {error, Reason} when
    CacheMap :: t(),
    Atom :: atom(),
    InternalIndex :: index(),
    Reason ::
        {already_present, {InternalIndex, CacheIndex, OtherAtom}}
        | {corrupted_atom_cache_ref_entry, {InternalIndex, AtomCacheRefEntry}}
        | max_internal_atom_cache_entries
        | max_atom_cache_size,
    CacheIndex :: vdist_atom_cache:index(),
    OtherAtom :: atom(),
    AtomCacheRefEntry :: vdist:atom_cache_ref_entry().
find_or_insert(#vdist_atom_cache_map{entries = Entries}, Atom) when
    is_atom(Atom) andalso map_size(Entries) >= ?ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES
->
    {error, max_internal_atom_cache_entries};
find_or_insert(
    CacheMap0 = #vdist_atom_cache_map{long_atoms = LongAtoms0, cache = Cache0, entries = Entries0}, Atom
) when is_atom(Atom) ->
    CacheIndex = vdist_atom_cache:atom_cache_index(Atom),
    case maps:find(CacheIndex, Entries0) of
        {ok, {InternalIndex, Atom, _AtomCacheRefEntry}} ->
            {ok, InternalIndex, CacheMap0};
        {ok, {InternalIndex, OtherAtom, _AtomCacheRefEntry}} ->
            OtherEntry = {InternalIndex, CacheIndex, OtherAtom},
            {error, {already_present, OtherEntry}};
        error ->
            InternalIndex = map_size(Entries0),
            case vdist_atom_cache:find_by_index(Cache0, CacheIndex) of
                {ok, {CacheIndex, Atom}} ->
                    AtomCacheRefEntry = vdist_old_atom_cache_ref_entry:new(CacheIndex),
                    Entries1 = Entries0#{CacheIndex => {InternalIndex, Atom, AtomCacheRefEntry}},
                    CacheMap1 = CacheMap0#vdist_atom_cache_map{entries = Entries1},
                    {ok, InternalIndex, CacheMap1};
                _Other ->
                    % io:format("ATOM CACHE MAP ERROR for ~p: ~p~nSTRUCTURE = ~p~n", [Atom, _Other, CacheMap0]),
                    % Force overwrite any existing cache entries.
                    {ok, {CacheIndex, Atom}, Cache1} = vdist_atom_cache:insert(Cache0, CacheIndex, Atom),
                    AtomText = erlang:atom_to_binary(Atom, utf8),
                    LongAtoms1 =
                        case byte_size(AtomText) of
                            Len when Len > 255 ->
                                true;
                            _ ->
                                LongAtoms0
                        end,
                    AtomCacheRefEntry = vdist_new_atom_cache_ref_entry:new(CacheIndex, AtomText),
                    Entries1 = Entries0#{CacheIndex => {InternalIndex, Atom, AtomCacheRefEntry}},
                    CacheMap1 = CacheMap0#vdist_atom_cache_map{
                        long_atoms = LongAtoms1, cache = Cache1, entries = Entries1
                    },
                    {ok, InternalIndex, CacheMap1}
            end
    end.
% case vdist_atom_cache:find_or_insert(Cache0, Atom) of
%     {ok, {CacheIndex, Atom}, Cache0} ->
%         case maps:find(CacheIndex, Entries0) of
%             {ok, {InternalIndex, _AtomCacheRefEntry}} ->
%                 {ok, InternalIndex, CacheMap0};
%             error ->

%         end;
%     {ok, {CacheIndex, Atom}, Cache1} ->
%         case maps:find(CacheIndex, Entries0) of
%             {ok, {InternalIndex, AtomCacheRefEntry}} ->
%                 {error, {corrupted_atom_cache_ref_entry, {InternalIndex, AtomCacheRefEntry}}};
%             error ->
%                 InternalIndex = map_size(Entries0),
%                 AtomText = erlang:atom_to_binary(Atom, utf8),
%                 LongAtoms1 =
%                     case byte_size(AtomText) of
%                         Len when Len > 255 ->
%                             true;
%                         _ ->
%                             LongAtoms0
%                     end,
%                 AtomCacheRefEntry = vdist_new_atom_cache_ref_entry:new(CacheIndex, AtomText),
%                 Entries1 = maps:put(CacheIndex, {InternalIndex, AtomCacheRefEntry}, Entries0),
%                 CacheMap1 = CacheMap0#vdist_atom_cache_map{
%                     long_atoms = LongAtoms1, cache = Cache1, entries = Entries1
%                 },
%                 {ok, InternalIndex, CacheMap1}
%         end;
%     {error, Reason = {already_present, _OtherEntry}} ->
%         {error, Reason};
%     {error, Reason = max_atom_cache_size} ->
%         {error, Reason}
% end.

-spec get_atom_cache(CacheMap) -> Cache when CacheMap :: t(), Cache :: vdist_atom_cache:t().
get_atom_cache(#vdist_atom_cache_map{cache = Cache}) ->
    Cache.

-spec get_atom_cache_ref_entries(CacheMap) -> AtomCacheRefEntries when
    CacheMap :: t(), AtomCacheRefEntries :: [vdist:atom_cache_ref_entry()].
get_atom_cache_ref_entries(#vdist_atom_cache_map{entries = Entries}) ->
    [AtomCacheRefEntry || {_InternalIndex, _Atom, AtomCacheRefEntry} <- lists:sort(maps:values(Entries))].

-spec get_long_atoms(CacheMap) -> boolean() when CacheMap :: t().
get_long_atoms(#vdist_atom_cache_map{long_atoms = LongAtoms}) ->
    LongAtoms.

-spec get_number_of_atom_cache_refs(CacheMap) -> non_neg_integer() when CacheMap :: t().
get_number_of_atom_cache_refs(#vdist_atom_cache_map{entries = Entries}) ->
    map_size(Entries).
