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
-module(vdist_dop_reg_send_tt).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("erldist_filter/include/erldist_filter.hrl").
-include_lib("erldist_filter/include/erldist_filter_erts_dist.hrl").

%% API
-export([
    new/4,
    has_payload/1,
    into_control_message_vterm/1,
    sequence_id/1
]).

%% Types
-type t() :: #vdist_dop_reg_send_tt{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(FromPid, Unused, ToName, TraceToken) -> T when
    FromPid :: vterm:pid_t(), Unused :: vterm:t(), ToName :: vterm:atom_t(), TraceToken :: vterm:t(), T :: t().
new(FromPid, Unused, ToName, TraceToken) when
    ?is_vterm_pid_t(FromPid) andalso ?is_vterm_t(Unused) andalso ?is_vterm_atom_t(ToName) andalso
        ?is_vterm_t(TraceToken)
->
    #vdist_dop_reg_send_tt{from_pid = FromPid, unused = Unused, to_name = ToName, trace_token = TraceToken}.

-spec has_payload(T) -> boolean() when T :: t().
has_payload(#vdist_dop_reg_send_tt{}) ->
    true.

-spec into_control_message_vterm(T) -> vterm:tuple_t() when T :: t().
into_control_message_vterm(#vdist_dop_reg_send_tt{
    from_pid = FromPid, unused = Unused, to_name = ToName, trace_token = TraceToken
}) ->
    vterm_small_tuple_ext:new(5, [vterm_small_integer_ext:new(?DOP_REG_SEND_TT), FromPid, Unused, ToName, TraceToken]).

-spec sequence_id(T) -> vdist:sequence_id() when T :: t().
sequence_id(#vdist_dop_reg_send_tt{from_pid = FromPid}) when ?is_vterm_pid_t(FromPid) ->
    vterm_pid:sequence_id(FromPid).
