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
-module(vdist_dop_monitor_p_exit).
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
-type t() :: #vdist_dop_monitor_p_exit{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(FromProc, ToPid, Ref, Reason) -> T when
    FromProc :: vterm:pid_t() | vterm:atom_t(),
    ToPid :: vterm:pid_t() | vterm:atom_t(),
    Ref :: vterm:reference_t(),
    Reason :: vterm:t(),
    T :: t().
new(FromProc, ToPid, Ref, Reason) when
    (?is_vterm_pid_t(FromProc) orelse ?is_vterm_atom_t(FromProc)) andalso ?is_vterm_pid_t(ToPid) andalso
        ?is_vterm_reference_t(Ref) andalso ?is_vterm_t(Reason)
->
    #vdist_dop_monitor_p_exit{from_proc = FromProc, to_pid = ToPid, ref = Ref, reason = Reason}.

-spec has_payload(T) -> boolean() when T :: t().
has_payload(#vdist_dop_monitor_p_exit{}) ->
    false.

-spec into_control_message_vterm(T) -> vterm:tuple_t() when T :: t().
into_control_message_vterm(#vdist_dop_monitor_p_exit{from_proc = FromProc, to_pid = ToPid, ref = Ref, reason = Reason}) ->
    vterm_small_tuple_ext:new(5, [vterm_small_integer_ext:new(?DOP_MONITOR_P_EXIT), FromProc, ToPid, Ref, Reason]).

-spec sequence_id(T) -> vdist:sequence_id() when T :: t().
sequence_id(#vdist_dop_monitor_p_exit{from_proc = FromProc}) when ?is_vterm_atom_t(FromProc) ->
    vterm_atom:sequence_id(FromProc);
sequence_id(#vdist_dop_monitor_p_exit{from_proc = FromProc}) when ?is_vterm_pid_t(FromProc) ->
    vterm_pid:sequence_id(FromProc).
