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
-module(vdist_atom_cache).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include("erldist_filter.hrl").
-include("erldist_filter_erts_external.hrl").

%% API
-export([
    new/0,
    atom_cache_index/1,
    find/2,
    find_or_insert/2,
    insert/3,
    diff/2,
    fill/0,
    fill/1,
    fill/2,
    is_filled/1
]).

%% Types
% -type index() :: 0..(?ERTS_ATOM_CACHE_SIZE - 1).
-type index() :: 0..2047.
-type t() :: #vdist_atom_cache{}.

-export_type([
    index/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new() -> Cache when Cache :: t().
new() ->
    #vdist_atom_cache{}.

-spec atom_cache_index(Atom) -> Index when Atom :: atom(), Index :: index().
atom_cache_index(Atom) when is_atom(Atom) ->
    erldist_filter_nif:atom2cix(Atom).
% B = erldist_filter_nif:internal_hash(Atom),
% M = ?ERTS_USE_ATOM_CACHE_SIZE,
% % mod(B, M)
% (B rem M + M) rem M.

-spec find
    (Cache, Atom) -> {ok, {Index, Atom}} | {error, Reason} when
        Cache :: t(),
        Atom :: atom(),
        Index :: index(),
        Reason :: not_found | {already_present, {Index, OtherAtom}},
        OtherAtom :: atom();
    (Cache, Index) -> {ok, {Index, Atom}} | {error, Reason} when
        Cache :: t(), Atom :: atom(), Index :: index(), Reason :: not_found.
find(#vdist_atom_cache{entries = Entries}, Atom) when is_atom(Atom) ->
    Index = atom_cache_index(Atom),
    case orddict:find(Index, Entries) of
        {ok, Atom} ->
            {ok, {Index, Atom}};
        {ok, OtherAtom} ->
            {error, {already_present, {Index, OtherAtom}}};
        error ->
            {error, not_found}
    end;
find(#vdist_atom_cache{entries = Entries}, Index) when
    is_integer(Index) andalso Index >= 0 andalso Index < ?ERTS_ATOM_CACHE_SIZE
->
    case orddict:find(Index, Entries) of
        {ok, Atom} ->
            {ok, {Index, Atom}};
        error ->
            {error, not_found}
    end.

-spec find_or_insert(Cache, Atom) -> {ok, {Index, Atom}, Cache} | {error, Reason} when
    Cache :: t(),
    Atom :: atom(),
    Index :: index(),
    Reason :: {already_present, {Index, OtherAtom}} | max_atom_cache_size,
    OtherAtom :: atom().
find_or_insert(#vdist_atom_cache{entries = Entries}, Atom) when
    is_atom(Atom) andalso length(Entries) >= ?ERTS_ATOM_CACHE_SIZE
->
    {error, max_atom_cache_size};
find_or_insert(Cache0 = #vdist_atom_cache{entries = Entries0}, Atom) when is_atom(Atom) ->
    case find(Cache0, Atom) of
        {ok, Entry} ->
            {ok, Entry, Cache0};
        {error, not_found} ->
            Index = atom_cache_index(Atom),
            Entries1 = orddict:store(Index, Atom, Entries0),
            Cache1 = Cache0#vdist_atom_cache{entries = Entries1},
            {ok, {Index, Atom}, Cache1};
        {error, Reason = {already_present, _OtherEntry}} ->
            % Index = atom_cache_index(Atom),
            % insert(Cache0, Index, Atom)
            {error, Reason}
    end.

-spec insert(t(), non_neg_integer(), atom()) -> {ok, {non_neg_integer(), atom()}, t()}.
insert(Cache0 = #vdist_atom_cache{entries = Entries0}, Index, Atom) when
    is_integer(Index) andalso Index >= 0 andalso Index < ?ERTS_ATOM_CACHE_SIZE andalso is_atom(Atom)
->
    Entries1 = orddict:store(Index, Atom, Entries0),
    Cache1 = Cache0#vdist_atom_cache{entries = Entries1},
    {ok, {Index, Atom}, Cache1}.

-spec diff(t(), t()) -> #{del := ordsets:ordset(atom()), add := ordsets:ordset(atom())}.
diff(_CacheA = #vdist_atom_cache{entries = EntriesA}, _CacheB = #vdist_atom_cache{entries = EntriesB}) ->
    SetA = ordsets:from_list([Atom || {_Index, Atom} <- orddict:to_list(EntriesA)]),
    SetB = ordsets:from_list([Atom || {_Index, Atom} <- orddict:to_list(EntriesB)]),
    Common = ordsets:intersection(SetA, SetB),
    Del = ordsets:subtract(SetA, Common),
    Add = ordsets:subtract(SetB, Common),
    #{del => Del, add => Add}.

-spec fill() -> t().
fill() ->
    fill(new()).

-spec fill(t()) -> t().
fill(Cache = #vdist_atom_cache{}) ->
    fill(Cache, 0).

-spec fill(t(), non_neg_integer()) -> t().
fill(Cache = #vdist_atom_cache{entries = Entries}, _I) when length(Entries) >= ?ERTS_USE_ATOM_CACHE_SIZE ->
    Cache;
fill(Cache0, I) ->
    Atom = erlang:binary_to_atom(<<"atom", (erlang:integer_to_binary(I))/bytes>>),
    case find(Cache0, Atom) of
        {error, not_found} ->
            Index = atom_cache_index(Atom),
            {ok, _, Cache1} = insert(Cache0, Index, Atom),
            fill(Cache1, I + 1);
        _ ->
            fill(Cache0, I + 1)
    end.

-spec is_filled(t()) -> boolean().
is_filled(#vdist_atom_cache{entries = Entries}) ->
    length(Entries) >= ?ERTS_USE_ATOM_CACHE_SIZE.
