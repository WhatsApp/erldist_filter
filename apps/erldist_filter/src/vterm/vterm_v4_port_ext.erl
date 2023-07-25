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
-module(vterm_v4_port_ext).
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
-type t() :: #vterm_v4_port_ext{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Node, Id, Creation) -> T when Node :: vterm:atom_t(), Id :: vterm:u64(), Creation :: vterm:u32(), T :: t().
new(Node, Id, Creation) when ?is_vterm_atom_t(Node) andalso ?is_u64(Id) andalso ?is_u32(Creation) ->
    #vterm_v4_port_ext{node = Node, id = Id, creation = Creation}.

-spec internal_vterm_to_binary(T, Opts) -> binary() when T :: t(), Opts :: term().
internal_vterm_to_binary(#vterm_v4_port_ext{node = Node0, id = Id, creation = Creation}, Opts) when
    ?is_vterm_atom_t(Node0) andalso ?is_u64(Id) andalso ?is_u32(Creation)
->
    Node = vterm_encode:internal_vterm_to_binary(Node0, Opts),
    <<?V4_PORT_EXT, Node/binary, Id:64, Creation:32>>.

-spec simplify(t()) -> term().
simplify(VTerm = #vterm_v4_port_ext{}) ->
    {ok, Term, <<>>} = vterm_decode:external_binary_to_term(vterm_encode:external_vterm_to_binary(VTerm, #{})),
    Term.
