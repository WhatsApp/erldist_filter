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
-module(vterm_atom_cache_ref).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(vterm_encode).

-include("erldist_filter.hrl").
-include("erldist_filter_erts_external.hrl").

%% API
-export([
    new/1,
    internal_vterm_to_binary/2
]).

%% Types
-type t() :: #vterm_atom_cache_ref{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Index) -> T when Index :: vterm:u8(), T :: t().
new(Index) when ?is_u8(Index) ->
    #vterm_atom_cache_ref{index = Index}.

-spec internal_vterm_to_binary(T, Opts) -> binary() when T :: t(), Opts :: term().
internal_vterm_to_binary(#vterm_atom_cache_ref{index = Index}, #{allow_atom_cache_refs := true}) ->
    <<?ATOM_CACHE_REF, Index:8>>.
