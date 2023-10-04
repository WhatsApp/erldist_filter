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
-module(vdist_dop_send).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include("erldist_filter.hrl").
-include("erldist_filter_erts_dist.hrl").

%% API
-export([
    new/2,
    has_payload/1,
    into_control_message_vterm/1,
    sequence_id/1
]).

%% Types
-type t() :: #vdist_dop_send{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Unused, ToPid) -> T when
    Unused :: vterm:t(), ToPid :: vterm:pid_t(), T :: t().
new(Unused, ToPid) when
    ?is_vterm_t(Unused) andalso ?is_vterm_pid_t(ToPid)
->
    #vdist_dop_send{unused = Unused, to_pid = ToPid}.

-spec has_payload(T) -> boolean() when T :: t().
has_payload(#vdist_dop_send{}) ->
    true.

-spec into_control_message_vterm(T) -> vterm:tuple_t() when T :: t().
into_control_message_vterm(#vdist_dop_send{unused = Unused, to_pid = ToPid}) ->
    vterm_small_tuple_ext:new(3, [vterm_small_integer_ext:new(?DOP_SEND), Unused, ToPid]).

-spec sequence_id(T) -> vdist:sequence_id() when T :: t().
sequence_id(#vdist_dop_send{}) ->
    0.
