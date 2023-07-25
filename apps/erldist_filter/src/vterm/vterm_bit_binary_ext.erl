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
-module(vterm_bit_binary_ext).
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
-type t() :: #vterm_bit_binary_ext{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Len, Bits, Data) -> T when Len :: vterm:u32(), Bits :: 1..8, Data :: binary(), T :: t().
new(Len, Bits, Data) when
    ?is_u32(Len) andalso (is_integer(Bits) andalso Bits >= 1 andalso Bits =< 8) andalso is_binary(Data) andalso
        Len =:= byte_size(Data)
->
    #vterm_bit_binary_ext{len = Len, bits = Bits, data = Data}.

-spec internal_vterm_to_binary(T, Opts) -> binary() when T :: t(), Opts :: term().
internal_vterm_to_binary(#vterm_bit_binary_ext{len = Len, bits = Bits, data = Data}, _Opts) when
    Len =:= byte_size(Data)
->
    <<?BIT_BINARY_EXT:8, Len:32, Bits:8, Data/bytes>>.

-spec simplify(T) -> Data when T :: t(), Data :: bitstring().
simplify(#vterm_bit_binary_ext{bits = Bits, data = Data}) ->
    BitSize = bit_size(Data) - (8 - Bits),
    <<BitBinary:BitSize/bitstring, _/bitstring>> = Data,
    BitBinary.
