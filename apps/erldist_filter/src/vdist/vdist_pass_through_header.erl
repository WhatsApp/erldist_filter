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
-module(vdist_pass_through_header).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(vdist_header_encode).

-include("erldist_filter.hrl").
% -include("erldist_filter_erts_dist.hrl").
-include("erldist_filter_erts_external.hrl").

%% API
-export([
    new/0,
    encode_header/1
]).

%% Types
-type t() :: #vdist_pass_through_header{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new() -> T when T :: t().
new() ->
    #vdist_pass_through_header{}.

-spec encode_header(T) -> binary() when T :: t().
encode_header(#vdist_pass_through_header{}) ->
    <<?PASS_THROUGH:8>>.
