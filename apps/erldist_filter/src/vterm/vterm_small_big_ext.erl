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
-module(vterm_small_big_ext).
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
-type t() :: #vterm_small_big_ext{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(N, Sign, D) -> T when N :: vterm:u8(), Sign :: 0..1, D :: binary(), T :: t().
new(N, Sign, D) when ?is_u8(N) andalso (Sign =:= 0 orelse Sign =:= 1) andalso N =:= byte_size(D) ->
    #vterm_small_big_ext{n = N, sign = Sign, d = D}.

-spec internal_vterm_to_binary(T, Opts) -> binary() when T :: t(), Opts :: term().
internal_vterm_to_binary(#vterm_small_big_ext{n = N, sign = Sign, d = D}, _Opts) when
    ?is_u8(N) andalso (Sign =:= 0 orelse Sign =:= 1) andalso N =:= byte_size(D)
->
    <<?SMALL_BIG_EXT:8, N:8, Sign:8, D:N/bytes>>.

-spec simplify(t()) -> term().
simplify(VTerm = #vterm_small_big_ext{}) ->
    {ok, Term, <<>>} = vterm_decode:external_binary_to_term(vterm_encode:external_vterm_to_binary(VTerm, #{})),
    Term.
