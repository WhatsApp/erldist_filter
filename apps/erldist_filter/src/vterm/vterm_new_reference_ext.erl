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
-module(vterm_new_reference_ext).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(vterm_encode).
-behaviour(vterm_simplify).

-include("erldist_filter.hrl").
-include("erldist_filter_erts_external.hrl").

%% API
-export([
    new/4,
    internal_vterm_to_binary/2,
    simplify/1
]).

%% Types
-type t() :: #vterm_new_reference_ext{}.

-export_type([
    t/0
]).

%% Macros
-define(is_creation(X), (is_integer(X) andalso (X) >= 0 andalso (X) =< 3)).
-define(is_len(X), (is_integer(X) andalso (X) >= 1 andalso (X) =< 5)).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Len, Node, Creation, Ids) -> T when
    Len :: 0..5, Node :: vterm:atom_t(), Creation :: 1..3, Ids :: [vterm:u32()], T :: t().
new(Len, Node, Creation, Ids) when
    ?is_len(Len) andalso ?is_vterm_atom_t(Node) andalso ?is_creation(Creation) andalso is_list(Ids) andalso
        Len =:= length(Ids)
->
    #vterm_new_reference_ext{len = Len, node = Node, creation = Creation, ids = Ids}.

-spec internal_vterm_to_binary(T, Opts) -> binary() when T :: t(), Opts :: term().
internal_vterm_to_binary(#vterm_new_reference_ext{len = Len, node = Node0, creation = Creation, ids = Ids}, Opts) when
    ?is_len(Len) andalso ?is_vterm_atom_t(Node0) andalso ?is_creation(Creation) andalso is_list(Ids) andalso
        Len =:= length(Ids)
->
    Node = vterm_encode:internal_vterm_to_binary(Node0, Opts),
    EncodedIds = <<<<Id:32>> || Id <- Ids>>,
    <<?NEW_REFERENCE_EXT, Len:16, Node/binary, Creation:8, EncodedIds/binary>>.

-spec simplify(t()) -> term().
simplify(VTerm = #vterm_new_reference_ext{}) ->
    {ok, Term, <<>>} = vterm_decode:external_binary_to_term(vterm_encode:external_vterm_to_binary(VTerm, #{})),
    Term.
