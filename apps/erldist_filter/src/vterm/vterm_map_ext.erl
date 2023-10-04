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
-module(vterm_map_ext).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(vterm_encode).
-behaviour(vterm_simplify).

-include("erldist_filter.hrl").
-include("erldist_filter_erts_external.hrl").

%% API
-export([
    new/2,
    internal_vterm_to_binary/2,
    simplify/1
]).

%% Types
-type t() :: #vterm_map_ext{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Arity, Pairs) -> T when Arity :: vterm:u32(), Pairs :: list({vterm:t(), vterm:t()}), T :: t().
new(Arity, Pairs) when ?is_u32(Arity) andalso is_list(Pairs) andalso Arity =:= length(Pairs) ->
    #vterm_map_ext{arity = Arity, pairs = Pairs}.

-spec internal_vterm_to_binary(T, Opts) -> binary() when T :: t(), Opts :: term().
internal_vterm_to_binary(#vterm_map_ext{arity = Arity, pairs = Pairs}, Opts) when
    ?is_u32(Arity) andalso is_list(Pairs) andalso Arity =:= length(Pairs)
->
    % EncodedPairs = vterm_encode:internal_vterm_pairs_to_binary(Pairs, Opts),
    EncodedPairs = erlang:iolist_to_binary([
        [vterm_encode:internal_vterm_to_binary(K, Opts), vterm_encode:internal_vterm_to_binary(V, Opts)]
     || {K, V} <- Pairs
    ]),
    <<?MAP_EXT:8, Arity:32, EncodedPairs/bytes>>.

-spec simplify(T) -> Data when T :: t(), Data :: map().
simplify(#vterm_map_ext{pairs = Pairs}) ->
    SimplePairs = [{vterm:simplify(K), vterm:simplify(V)} || {K, V} <- Pairs],
    maps:from_list(SimplePairs).
