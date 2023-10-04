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
-module(vdist_dop_send_sender_tt).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include("erldist_filter.hrl").
-include("erldist_filter_erts_dist.hrl").

%% API
-export([
    new/3,
    has_payload/1,
    into_control_message_vterm/1,
    sequence_id/1
]).

%% Types
-type t() :: #vdist_dop_send_sender_tt{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(FromPid, ToPid, TraceToken) -> T when
    FromPid :: vterm:pid_t(), ToPid :: vterm:pid_t(), TraceToken :: vterm:t(), T :: t().
new(FromPid, ToPid, TraceToken) when
    ?is_vterm_pid_t(FromPid) andalso ?is_vterm_pid_t(ToPid) andalso ?is_vterm_t(TraceToken)
->
    #vdist_dop_send_sender_tt{from_pid = FromPid, to_pid = ToPid, trace_token = TraceToken}.

-spec has_payload(T) -> boolean() when T :: t().
has_payload(#vdist_dop_send_sender_tt{}) ->
    true.

-spec into_control_message_vterm(T) -> vterm:tuple_t() when T :: t().
into_control_message_vterm(#vdist_dop_send_sender_tt{from_pid = FromPid, to_pid = ToPid, trace_token = TraceToken}) ->
    vterm_small_tuple_ext:new(4, [vterm_small_integer_ext:new(?DOP_SEND_SENDER_TT), FromPid, ToPid, TraceToken]).

-spec sequence_id(T) -> vdist:sequence_id() when T :: t().
sequence_id(#vdist_dop_send_sender_tt{from_pid = FromPid}) when ?is_vterm_pid_t(FromPid) ->
    vterm_pid:sequence_id(FromPid).
