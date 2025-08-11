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
-module(vdist).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("erldist_filter/include/erldist_filter.hrl").

%% API
-export([
    simplify/1
]).

%% Types
% -type atom_cache_index() :: 0..(?ERTS_USE_ATOM_CACHE_SIZE - 1).
-type atom_cache_index() :: 0..2038.
-type atom_cache_ref_entry() :: vdist_new_atom_cache_ref_entry:t() | vdist_old_atom_cache_ref_entry:t().
-type control_message() :: dop_t().
-type sequence_id() :: vterm:u64().
-type fragment_id() :: vterm:u64().
-type header_t() ::
    vdist_normal_header:t()
    | vdist_fragment_header:t()
    | vdist_fragment_cont:t()
    | vdist_pass_through_header:t().
-type dop_without_payload_t() ::
    vdist_dop_demonitor_p:t()
    | vdist_dop_exit:t()
    | vdist_dop_exit_tt:t()
    | vdist_dop_exit2:t()
    | vdist_dop_exit2_tt:t()
    | vdist_dop_group_leader:t()
    | vdist_dop_link:t()
    | vdist_dop_monitor_p:t()
    | vdist_dop_monitor_p_exit:t()
    | vdist_dop_spawn_reply:t()
    | vdist_dop_spawn_reply_tt:t()
    | vdist_dop_unlink:t()
    | vdist_dop_unlink_id:t()
    | vdist_dop_unlink_id_ack:t().
-type dop_with_payload_t() ::
    vdist_dop_alias_send:t()
    | vdist_dop_alias_send_tt:t()
    | vdist_dop_payload_exit:t()
    | vdist_dop_payload_exit_tt:t()
    | vdist_dop_payload_exit2:t()
    | vdist_dop_payload_exit2_tt:t()
    | vdist_dop_payload_monitor_p_exit:t()
    | vdist_dop_reg_send:t()
    | vdist_dop_reg_send_tt:t()
    | vdist_dop_send:t()
    | vdist_dop_send_sender:t()
    | vdist_dop_send_sender_tt:t()
    | vdist_dop_send_tt:t()
    | vdist_dop_spawn_request:t()
    | vdist_dop_spawn_request_tt:t().
-type dop_t() :: dop_without_payload_t() | dop_with_payload_t().

-export_type([
    atom_cache_index/0,
    atom_cache_ref_entry/0,
    control_message/0,
    sequence_id/0,
    fragment_id/0,
    header_t/0,
    dop_without_payload_t/0,
    dop_with_payload_t/0,
    dop_t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec simplify(DOP) -> T when DOP :: vdist:dop_t(), T :: dynamic().
simplify(DOP) when ?is_vdist_dop_t(DOP) ->
    VT = vdist_dop:dop_to_control_message_vterm(DOP),
    vterm:simplify(VT).
