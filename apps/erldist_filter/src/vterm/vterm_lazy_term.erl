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
-module(vterm_lazy_term).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(vterm_encode).
-behaviour(vterm_simplify).

-include("erldist_filter.hrl").

%% API
-export([
    new/1,
    internal_vterm_to_binary/2,
    simplify/1
]).

%% Types
-type t() :: #vterm_lazy_term{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Slice) -> T when Slice :: binary(), T :: t().
new(Slice) ->
    #vterm_lazy_term{slice = Slice}.

-spec internal_vterm_to_binary(T, Opts) -> binary() when T :: t(), Opts :: term().
internal_vterm_to_binary(#vterm_lazy_term{slice = Slice}, Opts) ->
    {ok, VTerm, <<>>} = vterm_decode:internal_binary_to_vterm(Slice),
    Slice = vterm_encode:internal_vterm_to_binary(VTerm, Opts),
    Slice.

-spec simplify(T) -> term() when T :: t().
simplify(#vterm_lazy_term{slice = Slice}) ->
    {ok, VTerm, <<>>} = vterm_decode:internal_binary_to_vterm(Slice),
    vterm:simplify(VTerm).
