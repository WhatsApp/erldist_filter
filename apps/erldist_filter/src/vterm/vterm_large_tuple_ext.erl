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
-module(vterm_large_tuple_ext).
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
-type t() :: #vterm_large_tuple_ext{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Arity, Elements) -> T when Arity :: vterm:u32(), Elements :: list(vterm:t()), T :: t().
new(Arity, Elements) when ?is_u32(Arity) andalso is_list(Elements) andalso Arity =:= length(Elements) ->
    #vterm_large_tuple_ext{arity = Arity, elements = Elements}.

-spec internal_vterm_to_binary(T, Opts) -> binary() when T :: t(), Opts :: term().
internal_vterm_to_binary(#vterm_large_tuple_ext{arity = Arity, elements = Elements}, Opts) when
    ?is_u32(Arity) andalso is_list(Elements) andalso Arity =:= length(Elements)
->
    EncodedElements = vterm_encode:internal_vterm_elements_to_binary(Elements, Opts),
    <<?LARGE_TUPLE_EXT:8, Arity:32, EncodedElements/bytes>>.

-spec simplify(T) -> Data when T :: t(), Data :: tuple().
simplify(#vterm_large_tuple_ext{elements = Elements}) ->
    erlang:list_to_tuple([vterm:simplify(Element) || Element <- Elements]).
