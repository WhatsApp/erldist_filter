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
-module(vdist_dop_send_tt).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("erldist_filter/include/erldist_filter.hrl").
-include_lib("erldist_filter/include/erldist_filter_erts_dist.hrl").

%% API
-export([
    new/3,
    has_payload/1,
    into_control_message_vterm/1,
    sequence_id/1
]).

%% Types
-type t() :: #vdist_dop_send_tt{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Unused, ToPid, TraceToken) -> T when
    Unused :: vterm:t(), ToPid :: vterm:pid_t(), TraceToken :: vterm:t(), T :: t().
new(Unused, ToPid, TraceToken) when
    ?is_vterm_t(Unused) andalso ?is_vterm_pid_t(ToPid) andalso ?is_vterm_t(TraceToken)
->
    #vdist_dop_send_tt{unused = Unused, to_pid = ToPid, trace_token = TraceToken}.

-spec has_payload(T) -> boolean() when T :: t().
has_payload(#vdist_dop_send_tt{}) ->
    true.

-spec into_control_message_vterm(T) -> vterm:tuple_t() when T :: t().
into_control_message_vterm(#vdist_dop_send_tt{unused = Unused, to_pid = ToPid, trace_token = TraceToken}) ->
    vterm_small_tuple_ext:new(4, [vterm_small_integer_ext:new(?DOP_SEND_TT), Unused, ToPid, TraceToken]).

-spec sequence_id(T) -> vdist:sequence_id() when T :: t().
sequence_id(#vdist_dop_send_tt{}) ->
    0.
