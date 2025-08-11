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
-module(vdist_header_encode).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("erldist_filter/include/erldist_filter.hrl").

-callback encode_header(T :: vdist:header_t()) -> binary().

-export([
    encode_header/1
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec encode_header(T :: vdist:header_t()) -> binary().
encode_header(T = #vdist_fragment_cont{}) ->
    vdist_fragment_cont:encode_header(T);
encode_header(T = #vdist_fragment_header{}) ->
    vdist_fragment_header:encode_header(T);
encode_header(T = #vdist_normal_header{}) ->
    vdist_normal_header:encode_header(T);
encode_header(T = #vdist_pass_through_header{}) ->
    vdist_pass_through_header:encode_header(T).
