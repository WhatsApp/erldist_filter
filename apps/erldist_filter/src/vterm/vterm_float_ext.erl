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
-module(vterm_float_ext).
-compile(warn_missing_spec_all).
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
-type t() :: #vterm_float_ext{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(FloatString) -> T when FloatString :: <<_:248>>, T :: t().
new(FloatString) when is_binary(FloatString) andalso byte_size(FloatString) =:= 31 ->
    #vterm_float_ext{float_string = FloatString}.

-spec internal_vterm_to_binary(T, Opts) -> binary() when T :: t(), Opts :: term().
internal_vterm_to_binary(#vterm_float_ext{float_string = FloatString}, _Opts) when byte_size(FloatString) =:= 31 ->
    <<?FLOAT_EXT:8, FloatString:31/bytes>>.

-spec simplify(T) -> term() when T :: t().
simplify(VTerm = #vterm_float_ext{}) ->
    {ok, Term, <<>>} = vterm_decode:external_binary_to_term(vterm_encode:external_vterm_to_binary(VTerm, #{})),
    Term.
