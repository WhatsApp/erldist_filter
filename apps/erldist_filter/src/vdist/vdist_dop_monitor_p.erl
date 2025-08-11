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
-module(vdist_dop_monitor_p).
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
-type t() :: #vdist_dop_monitor_p{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(FromPid, ToProc, Ref) -> T when
    FromPid :: vterm:pid_t(), ToProc :: vterm:pid_t() | vterm:atom_t(), Ref :: vterm:reference_t(), T :: t().
new(FromPid, ToProc, Ref) when
    ?is_vterm_pid_t(FromPid) andalso (?is_vterm_pid_t(ToProc) orelse ?is_vterm_atom_t(ToProc)) andalso
        ?is_vterm_reference_t(Ref)
->
    #vdist_dop_monitor_p{from_pid = FromPid, to_proc = ToProc, ref = Ref}.

-spec has_payload(T) -> boolean() when T :: t().
has_payload(#vdist_dop_monitor_p{}) ->
    false.

-spec into_control_message_vterm(T) -> vterm:tuple_t() when T :: t().
into_control_message_vterm(#vdist_dop_monitor_p{from_pid = FromPid, to_proc = ToProc, ref = Ref}) ->
    vterm_small_tuple_ext:new(4, [vterm_small_integer_ext:new(?DOP_MONITOR_P), FromPid, ToProc, Ref]).

-spec sequence_id(T) -> vdist:sequence_id() when T :: t().
sequence_id(#vdist_dop_monitor_p{from_pid = FromPid}) when ?is_vterm_pid_t(FromPid) ->
    vterm_pid:sequence_id(FromPid).
