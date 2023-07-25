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
-module(vdist_atom_translation_table).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include("erldist_filter.hrl").
-include("erldist_filter_erts_external.hrl").

%% API
-export([
    new/0,
    find/2,
    rfind/2,
    store/3
]).

%% Types
% -type index() :: 0..?ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES.
-type index() :: 0..255.
-type t() :: #vdist_atom_translation_table{}.

-export_type([
    index/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new() -> Table when Table :: t().
new() ->
    #vdist_atom_translation_table{}.

-spec find(Table, Index) -> {ok, Atom} | {error, Reason} when
    Table :: t(),
    Index :: index(),
    Atom :: atom(),
    Reason :: not_found.
find(#vdist_atom_translation_table{entries = Entries}, Index) when
    is_integer(Index) andalso Index >= 0 andalso Index =< ?ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES
->
    case maps:find(Index, Entries) of
        {ok, {_CacheIndex, Atom}} ->
            {ok, Atom};
        error ->
            {error, not_found}
    end.

-spec rfind(Table, Atom) -> {ok, Index} | {error, Reason} when
    Table :: t(),
    Index :: index(),
    Atom :: atom(),
    Reason :: not_found.
rfind(#vdist_atom_translation_table{entries = Entries}, Atom) when is_atom(Atom) ->
    do_rfind(maps:iterator(Entries), Atom).

-spec store(Table, CacheIndex, Atom) -> {ok, Table} | {error, Reason} when
    Table :: t(), CacheIndex :: vdist_atom_cache:index(), Atom :: atom(), Reason :: max_internal_atom_cache_entries.
store(#vdist_atom_translation_table{entries = Entries}, CacheIndex, Atom) when
    is_integer(CacheIndex) andalso is_atom(Atom) andalso map_size(Entries) > ?ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES
->
    {error, max_internal_atom_cache_entries};
store(Table0 = #vdist_atom_translation_table{entries = Entries0}, CacheIndex, Atom) when
    is_integer(CacheIndex) andalso is_atom(Atom)
->
    Index = map_size(Entries0),
    Entries1 = maps:put(Index, {CacheIndex, Atom}, Entries0),
    Table1 = Table0#vdist_atom_translation_table{entries = Entries1},
    {ok, Table1}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
do_rfind(Iterator, Atom) ->
    case maps:next(Iterator) of
        {InternalIndex, {_CacheIndex, Atom}, _NextIterator} ->
            {ok, InternalIndex};
        {_InternalIndex, {_CacheIndex, _Atom}, NextIterator} ->
            do_rfind(NextIterator, Atom);
        none ->
            {error, not_found}
    end.
