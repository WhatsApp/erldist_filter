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
-module(vterm_binary_ext).
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
-type t() :: #vterm_binary_ext{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Len, Data) -> T when Len :: vterm:u32(), Data :: binary(), T :: t().
new(Len, Data) when ?is_u32(Len) andalso is_binary(Data) andalso Len =:= byte_size(Data) ->
    #vterm_binary_ext{len = Len, data = Data}.

-spec internal_vterm_to_binary(T, Opts) -> binary() when T :: t(), Opts :: term().
internal_vterm_to_binary(#vterm_binary_ext{len = Len, data = Data}, _Opts) when Len =:= byte_size(Data) ->
    <<?BINARY_EXT:8, Len:32, Data/binary>>.

-spec simplify(T) -> Data when T :: t(), Data :: binary().
simplify(#vterm_binary_ext{data = Data}) ->
    Data.
