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
-module(vterm_integer_ext).
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
-type t() :: #vterm_integer_ext{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Value) -> T when Value :: vterm:i32(), T :: t().
new(Value) when ?is_i32(Value) ->
    #vterm_integer_ext{value = Value}.

-spec internal_vterm_to_binary(T, Opts) -> binary() when T :: t(), Opts :: term().
internal_vterm_to_binary(#vterm_integer_ext{value = Value}, _Opts) ->
    <<?INTEGER_EXT:8, Value:1/signed-big-integer-unit:32>>.

-spec simplify(T) -> Value when T :: t(), Value :: vterm:i32().
simplify(#vterm_integer_ext{value = Value}) ->
    Value.
