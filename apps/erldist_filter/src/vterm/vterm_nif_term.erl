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
-module(vterm_nif_term).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(vterm_encode).
-behaviour(vterm_simplify).

-include("erldist_filter.hrl").
-include("erldist_filter_erts_external.hrl").

%% API
-export([
    new/1,
    internal_vterm_to_binary/2,
    simplify/1
]).

%% Types
-type t() :: #vterm_nif_term{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Term) -> T when Term :: vterm:t() | term(), T :: t().
new(Term) ->
    #vterm_nif_term{term = Term}.

-spec internal_vterm_to_binary(T, Opts) -> binary() when T :: t(), Opts :: term().
internal_vterm_to_binary(#vterm_nif_term{term = VTerm}, _Opts) when ?is_vterm_the_non_value_t(VTerm) ->
    erlang:error(notsup);
internal_vterm_to_binary(#vterm_nif_term{term = VTerm}, Opts) when ?is_vterm_t(VTerm) ->
    vterm_encode:internal_vterm_to_binary(VTerm, Opts);
internal_vterm_to_binary(#vterm_nif_term{term = Term}, _Opts) ->
    vterm_encode:internal_term_to_binary(Term).

-spec simplify(T) -> term() when T :: t().
simplify(#vterm_nif_term{term = VTerm}) when ?is_vterm_the_non_value_t(VTerm) ->
    erlang:error(notsup);
simplify(#vterm_nif_term{term = VTerm}) when ?is_vterm_t(VTerm) ->
    vterm:simplify(VTerm);
simplify(#vterm_nif_term{term = Term}) ->
    Term.
