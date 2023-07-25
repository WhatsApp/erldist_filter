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
-module(vdist_fragment_cont).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(vdist_header_encode).

-include("erldist_filter.hrl").
% -include("erldist_filter_erts_dist.hrl").
-include("erldist_filter_erts_external.hrl").

%% API
-export([
    new/2,
    encode_header/1
]).

%% Types
-type t() :: #vdist_fragment_cont{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(SequenceId, FragmentId) -> T when
    SequenceId :: vdist:sequence_id(), FragmentId :: vdist:fragment_id(), T :: t().
new(SequenceId, FragmentId) when ?is_u64(SequenceId) andalso ?is_u64(FragmentId) ->
    #vdist_fragment_cont{sequence_id = SequenceId, fragment_id = FragmentId}.

-spec encode_header(T) -> binary() when T :: t().
encode_header(#vdist_fragment_cont{sequence_id = SequenceId, fragment_id = FragmentId}) ->
    <<?VERSION_MAGIC:8, ?DIST_FRAG_CONT:8, SequenceId:64, FragmentId:64>>.
