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
-module(vterm_small_atom_utf8_ext).
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
-type t() :: #vterm_small_atom_utf8_ext{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Len, Name) -> T when Len :: vterm:u8(), Name :: binary(), T :: t().
new(Len, Name) when ?is_u8(Len) andalso is_binary(Name) andalso Len =:= byte_size(Name) ->
    #vterm_small_atom_utf8_ext{len = Len, name = Name}.

-spec internal_vterm_to_binary(T, Opts) -> binary() when T :: t(), Opts :: term().
internal_vterm_to_binary(#vterm_small_atom_utf8_ext{len = Len, name = Name}, _Opts) when Len =:= byte_size(Name) ->
    <<?SMALL_ATOM_UTF8_EXT:8, Len:8, Name/binary>>.

-spec simplify(t()) -> atom().
simplify(#vterm_small_atom_utf8_ext{name = Name}) ->
    erlang:binary_to_atom(Name, utf8).
