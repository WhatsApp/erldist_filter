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
-module(vterm_string_ext).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(vterm_encode).
-behaviour(vterm_simplify).

-include_lib("erldist_filter/include/erldist_filter.hrl").
-include_lib("erldist_filter/include/erldist_filter_erts_external.hrl").

%% API
-export([
    new/2,
    internal_vterm_to_binary/2,
    simplify/1
]).

%% Types
-type t() :: #vterm_string_ext{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Len, Characters) -> T when Len :: vterm:u16(), Characters :: binary(), T :: t().
new(Len, Characters) when ?is_u16(Len) andalso is_binary(Characters) andalso Len =:= byte_size(Characters) ->
    #vterm_string_ext{len = Len, characters = Characters}.

-spec internal_vterm_to_binary(T, Opts) -> binary() when T :: t(), Opts :: term().
internal_vterm_to_binary(#vterm_string_ext{len = Len, characters = Characters}, _Opts) when
    Len =:= byte_size(Characters)
->
    <<?STRING_EXT:8, Len:16, Characters/binary>>.

-spec simplify(t()) -> string().
simplify(#vterm_string_ext{characters = Characters}) ->
    erlang:binary_to_list(Characters).
