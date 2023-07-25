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
-module(vterm_list_ext).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(vterm_encode).
-behaviour(vterm_simplify).

-include("erldist_filter.hrl").
-include("erldist_filter_erts_external.hrl").

%% API
-export([
    new/3,
    internal_vterm_to_binary/2,
    simplify/1
]).

%% Types
-type t() :: #vterm_list_ext{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Len, Elements, Tail) -> T when Len :: vterm:u32(), Elements :: list(vterm:t()), Tail :: vterm:t(), T :: t().
new(Len, Elements, Tail) when
    ?is_u32(Len) andalso is_list(Elements) andalso Len =:= length(Elements) andalso ?is_vterm_t(Tail)
->
    #vterm_list_ext{len = Len, elements = Elements, tail = Tail}.

-spec internal_vterm_to_binary(T, Opts) -> binary() when T :: t(), Opts :: term().
internal_vterm_to_binary(#vterm_list_ext{len = Len, elements = Elements, tail = Tail}, Opts) when
    ?is_u32(Len) andalso is_list(Elements) andalso Len =:= length(Elements) andalso ?is_vterm_t(Tail)
->
    EncodedElements = vterm_encode:internal_vterm_elements_to_binary(Elements, Opts),
    EncodedTail = vterm_encode:internal_vterm_to_binary(Tail, Opts),
    <<?LIST_EXT:8, Len:32, EncodedElements/bytes, EncodedTail/bytes>>.

-spec simplify(T) -> [vterm:t()] when T :: t().
simplify(#vterm_list_ext{elements = Elements, tail = Tail}) ->
    simplify_list(lists:reverse(Elements), vterm:simplify(Tail)).

%% @private

-spec simplify_list([vterm:t()], [vterm:t()]) -> [vterm:t()].
simplify_list([], Tail) ->
    Tail;
simplify_list([Element | Elements], Tail) ->
    simplify_list(Elements, [vterm:simplify(Element) | Tail]).
