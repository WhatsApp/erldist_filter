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
-module(vterm_port_ext).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(vterm_encode).
-behaviour(vterm_simplify).

-include_lib("erldist_filter/include/erldist_filter.hrl").
-include_lib("erldist_filter/include/erldist_filter_erts_external.hrl").

%% API
-export([
    new/3,
    internal_vterm_to_binary/2,
    simplify/1
]).

%% Types
-type t() :: #vterm_port_ext{}.

-export_type([
    t/0
]).

%% Macros
-define(is_creation(X), (is_integer(X) andalso (X) >= 0 andalso (X) =< 3)).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Node, Id, Creation) -> T when Node :: vterm:atom_t(), Id :: vterm:u32(), Creation :: 0..3, T :: t().
new(Node, Id, Creation) when ?is_vterm_atom_t(Node) andalso ?is_u32(Id) andalso ?is_creation(Creation) ->
    #vterm_port_ext{node = Node, id = Id, creation = Creation}.

-spec internal_vterm_to_binary(T, Opts) -> binary() when T :: t(), Opts :: term().
internal_vterm_to_binary(#vterm_port_ext{node = Node0, id = Id, creation = Creation}, Opts) when
    ?is_vterm_atom_t(Node0) andalso ?is_u32(Id) andalso ?is_creation(Creation)
->
    Node = vterm_encode:internal_vterm_to_binary(Node0, Opts),
    <<?PORT_EXT, Node/binary, Id:32, Creation:8>>.

-spec simplify(t()) -> term().
simplify(VTerm = #vterm_port_ext{}) ->
    {ok, Term, <<>>} = vterm_decode:external_binary_to_term(vterm_encode:external_vterm_to_binary(VTerm, #{})),
    Term.
