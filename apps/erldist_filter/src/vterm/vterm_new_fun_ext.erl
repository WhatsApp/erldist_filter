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
-module(vterm_new_fun_ext).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(vterm_encode).
-behaviour(vterm_simplify).

-include("erldist_filter.hrl").
-include("erldist_filter_erts_external.hrl").

%% API
-export([
    new/10,
    new_with_derived_size/9,
    repair_derived_size/1,
    internal_vterm_to_binary/2,
    simplify/1
]).

%% Types
-type t() :: #vterm_new_fun_ext{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Size, Arity, Uniq, Index, NumFree, Module, OldIndex, OldUniq, Pid, FreeVars) -> T when
    Size :: vterm:u32(),
    Arity :: vterm:u8(),
    Uniq :: <<_:128>>,
    Index :: vterm:u32(),
    NumFree :: vterm:u32(),
    Module :: vterm:atom_t(),
    OldIndex :: vterm:fixed_integer_t(),
    OldUniq :: vterm:fixed_integer_t(),
    Pid :: vterm:pid_t(),
    FreeVars :: list(vterm:t()),
    T :: t().
new(Size, Arity, Uniq, Index, NumFree, Module, OldIndex, OldUniq, Pid, FreeVars) when
    ?is_u32(Size) andalso
        ?is_u8(Arity) andalso
        (is_binary(Uniq) andalso byte_size(Uniq) =:= 16) andalso
        ?is_u32(Index) andalso
        ?is_u32(NumFree) andalso
        ?is_vterm_atom_t(Module) andalso
        ?is_vterm_fixed_integer_t(OldIndex) andalso
        ?is_vterm_fixed_integer_t(OldUniq) andalso
        ?is_vterm_pid_t(Pid) andalso
        (is_list(FreeVars) andalso length(FreeVars) =:= NumFree)
->
    #vterm_new_fun_ext{
        size = Size,
        arity = Arity,
        uniq = Uniq,
        index = Index,
        num_free = NumFree,
        module = Module,
        old_index = OldIndex,
        old_uniq = OldUniq,
        pid = Pid,
        free_vars = FreeVars
    }.

-spec new_with_derived_size(Arity, Uniq, Index, NumFree, Module, OldIndex, OldUniq, Pid, FreeVars) -> T when
    Arity :: vterm:u8(),
    Uniq :: <<_:128>>,
    Index :: vterm:u32(),
    NumFree :: vterm:u32(),
    Module :: vterm:atom_t(),
    OldIndex :: vterm:fixed_integer_t(),
    OldUniq :: vterm:fixed_integer_t(),
    Pid :: vterm:pid_t(),
    FreeVars :: list(vterm:t()),
    T :: t().
new_with_derived_size(Arity, Uniq, Index, NumFree, Module, OldIndex, OldUniq, Pid, FreeVars) when
    ?is_u8(Arity) andalso
        (is_binary(Uniq) andalso byte_size(Uniq) =:= 16) andalso
        ?is_u32(Index) andalso
        ?is_u32(NumFree) andalso
        ?is_vterm_atom_t(Module) andalso
        ?is_vterm_fixed_integer_t(OldIndex) andalso
        ?is_vterm_fixed_integer_t(OldUniq) andalso
        ?is_vterm_pid_t(Pid) andalso
        (is_list(FreeVars) andalso length(FreeVars) =:= NumFree)
->
    T0 = new(0, Arity, Uniq, Index, NumFree, Module, OldIndex, OldUniq, Pid, FreeVars),
    <<?NEW_FUN_EXT:8, Size:32, _/bits>> = internal_vterm_to_binary(T0, #{allow_atom_cache_refs => true}),
    T1 = T0#vterm_new_fun_ext{size = Size},
    T1.

-spec repair_derived_size(T) -> T when T :: t().
repair_derived_size(T0 = #vterm_new_fun_ext{}) ->
    T1 = T0#vterm_new_fun_ext{size = 0},
    <<?NEW_FUN_EXT:8, Size:32, _/bits>> = internal_vterm_to_binary(T1, #{allow_atom_cache_refs => true}),
    T2 = T1#vterm_new_fun_ext{size = Size},
    T2.

-spec internal_vterm_to_binary(T, Opts) -> binary() when T :: t(), Opts :: term().
internal_vterm_to_binary(
    #vterm_new_fun_ext{
        size = Size,
        arity = Arity,
        uniq = Uniq,
        index = Index,
        num_free = NumFree,
        module = Module0,
        old_index = OldIndex0,
        old_uniq = OldUniq0,
        pid = Pid0,
        free_vars = FreeVars
    },
    Opts
) ->
    Module = vterm_encode:internal_vterm_to_binary(Module0, Opts),
    OldIndex = vterm_encode:internal_vterm_to_binary(OldIndex0, Opts),
    OldUniq = vterm_encode:internal_vterm_to_binary(OldUniq0, Opts),
    Pid = vterm_encode:internal_vterm_to_binary(Pid0, Opts),
    EncodedFreeVars = vterm_encode:internal_vterm_elements_to_binary(FreeVars, Opts),
    Body =
        <<Arity:8, Uniq:16/bytes, Index:32, NumFree:32, Module/bytes, OldIndex/bytes, OldUniq/bytes, Pid/bytes,
            EncodedFreeVars/bytes>>,
    case byte_size(Body) + 4 of
        Size ->
            <<?NEW_FUN_EXT:8, Size:32, Body/binary>>;
        NewSize ->
            %% It's possible that we're re-encoding with atom cache refs already resolved, so the size will have changed.
            %% If we detect that it's different, let's just use the new size of the body.
            <<?NEW_FUN_EXT:8, NewSize:32, Body/binary>>
    end.

-spec simplify(T) -> term() when T :: t().
simplify(VTerm = #vterm_new_fun_ext{}) ->
    {ok, Term, <<>>} = vterm_decode:external_binary_to_term(vterm_encode:external_vterm_to_binary(VTerm, #{})),
    Term.
