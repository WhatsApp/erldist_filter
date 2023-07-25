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
%%% Created :  19 Jun 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(udist).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include("udist.hrl").

%% Public API
-export([
    cast_to_dop/1,
    cast_to_raw_dop/1,
    get_dop_group/1,
    get_dop_name/1
]).

%% Types
-type raw_dop_alias_send_t() :: {33, pid(), reference()}.
-type raw_dop_alias_send_tt_t() :: {34, pid(), reference(), term()}.
-type raw_dop_demonitor_p_t() :: {20, pid(), atom() | pid(), reference()}.
-type raw_dop_exit_t() :: {3, pid(), pid(), term()}.
-type raw_dop_exit2_t() :: {8, pid(), pid(), term()}.
-type raw_dop_exit2_tt_t() :: {18, pid(), pid(), term(), term()}.
-type raw_dop_exit_tt_t() :: {13, pid(), pid(), term(), term()}.
-type raw_dop_group_leader_t() :: {7, pid(), pid()}.
-type raw_dop_link_t() :: {1, pid(), pid()}.
-type raw_dop_monitor_p_t() :: {19, pid(), atom() | pid(), reference()}.
-type raw_dop_monitor_p_exit_t() :: {21, atom() | pid(), pid(), reference(), term()}.
-type raw_dop_payload_exit_t() :: {24, pid(), pid()}.
-type raw_dop_payload_exit2_t() :: {26, pid(), pid()}.
-type raw_dop_payload_exit2_tt_t() :: {27, pid(), pid(), term()}.
-type raw_dop_payload_exit_tt_t() :: {25, pid(), pid(), term()}.
-type raw_dop_payload_monitor_p_exit_t() :: {28, atom() | pid(), pid(), reference()}.
-type raw_dop_reg_send_t() :: {6, pid(), term(), atom()}.
-type raw_dop_reg_send_tt_t() :: {16, pid(), term(), atom(), term()}.
-type raw_dop_send_t() :: {2, term(), pid()}.
-type raw_dop_send_sender_t() :: {22, pid(), pid()}.
-type raw_dop_send_sender_tt_t() :: {23, pid(), pid(), term()}.
-type raw_dop_send_tt_t() :: {12, term(), pid(), term()}.
-type raw_dop_spawn_reply_t() :: {31, reference(), pid(), integer(), atom() | pid()}.
-type raw_dop_spawn_reply_tt_t() :: {32, reference(), pid(), integer(), atom() | pid(), term()}.
-type raw_dop_spawn_request_t() :: {29, reference(), pid(), pid(), {atom(), atom(), integer()}, [term()]}.
-type raw_dop_spawn_request_tt_t() :: {30, reference(), pid(), pid(), {atom(), atom(), integer()}, [term()], term()}.
-type raw_dop_unlink_t() :: {4, pid(), pid()}.
-type raw_dop_unlink_id_t() :: {35, integer(), pid(), pid()}.
-type raw_dop_unlink_id_ack_t() :: {36, integer(), pid(), pid()}.

-type raw_dop_without_payload_t() ::
    raw_dop_demonitor_p_t()
    | raw_dop_exit_t()
    | raw_dop_exit2_t()
    | raw_dop_exit2_tt_t()
    | raw_dop_exit_tt_t()
    | raw_dop_group_leader_t()
    | raw_dop_link_t()
    | raw_dop_monitor_p_t()
    | raw_dop_monitor_p_exit_t()
    | raw_dop_spawn_reply_t()
    | raw_dop_spawn_reply_tt_t()
    | raw_dop_unlink_t()
    | raw_dop_unlink_id_t()
    | raw_dop_unlink_id_ack_t().

-type raw_dop_with_payload_t() ::
    raw_dop_alias_send_t()
    | raw_dop_alias_send_tt_t()
    | raw_dop_payload_exit_t()
    | raw_dop_payload_exit2_t()
    | raw_dop_payload_exit2_tt_t()
    | raw_dop_payload_exit_tt_t()
    | raw_dop_payload_monitor_p_exit_t()
    | raw_dop_reg_send_t()
    | raw_dop_reg_send_tt_t()
    | raw_dop_send_t()
    | raw_dop_send_sender_t()
    | raw_dop_send_sender_tt_t()
    | raw_dop_send_tt_t()
    | raw_dop_spawn_request_t()
    | raw_dop_spawn_request_tt_t().

-type raw_dop_t() :: raw_dop_without_payload_t() | raw_dop_with_payload_t().

-type dop_alias_send_t() :: #udist_dop_alias_send{}.
-type dop_alias_send_tt_t() :: #udist_dop_alias_send_tt{}.
-type dop_demonitor_p_t() :: #udist_dop_demonitor_p{}.
-type dop_exit_t() :: #udist_dop_exit{}.
-type dop_exit2_t() :: #udist_dop_exit2{}.
-type dop_exit2_tt_t() :: #udist_dop_exit2_tt{}.
-type dop_exit_tt_t() :: #udist_dop_exit_tt{}.
-type dop_group_leader_t() :: #udist_dop_group_leader{}.
-type dop_link_t() :: #udist_dop_link{}.
-type dop_monitor_p_t() :: #udist_dop_monitor_p{}.
-type dop_monitor_p_exit_t() :: #udist_dop_monitor_p_exit{}.
-type dop_payload_exit_t() :: #udist_dop_payload_exit{}.
-type dop_payload_exit2_t() :: #udist_dop_payload_exit2{}.
-type dop_payload_exit2_tt_t() :: #udist_dop_payload_exit2_tt{}.
-type dop_payload_exit_tt_t() :: #udist_dop_payload_exit_tt{}.
-type dop_payload_monitor_p_exit_t() :: #udist_dop_payload_monitor_p_exit{}.
-type dop_reg_send_t() :: #udist_dop_reg_send{}.
-type dop_reg_send_tt_t() :: #udist_dop_reg_send_tt{}.
-type dop_send_t() :: #udist_dop_send{}.
-type dop_send_sender_t() :: #udist_dop_send_sender{}.
-type dop_send_sender_tt_t() :: #udist_dop_send_sender_tt{}.
-type dop_send_tt_t() :: #udist_dop_send_tt{}.
-type dop_spawn_reply_t() :: #udist_dop_spawn_reply{}.
-type dop_spawn_reply_tt_t() :: #udist_dop_spawn_reply_tt{}.
-type dop_spawn_request_t() :: #udist_dop_spawn_request{}.
-type dop_spawn_request_tt_t() :: #udist_dop_spawn_request_tt{}.
-type dop_unlink_t() :: #udist_dop_unlink{}.
-type dop_unlink_id_t() :: #udist_dop_unlink_id{}.
-type dop_unlink_id_ack_t() :: #udist_dop_unlink_id_ack{}.

-type dop_without_payload_t() ::
    dop_demonitor_p_t()
    | dop_exit_t()
    | dop_exit2_t()
    | dop_exit2_tt_t()
    | dop_exit_tt_t()
    | dop_group_leader_t()
    | dop_link_t()
    | dop_monitor_p_t()
    | dop_monitor_p_exit_t()
    | dop_spawn_reply_t()
    | dop_spawn_reply_tt_t()
    | dop_unlink_t()
    | dop_unlink_id_t()
    | dop_unlink_id_ack_t().

-type dop_with_payload_t() ::
    dop_alias_send_t()
    | dop_alias_send_tt_t()
    | dop_payload_exit_t()
    | dop_payload_exit2_t()
    | dop_payload_exit2_tt_t()
    | dop_payload_exit_tt_t()
    | dop_payload_monitor_p_exit_t()
    | dop_reg_send_t()
    | dop_reg_send_tt_t()
    | dop_send_t()
    | dop_send_sender_t()
    | dop_send_sender_tt_t()
    | dop_send_tt_t()
    | dop_spawn_request_t()
    | dop_spawn_request_tt_t().

-type dop_t() :: dop_without_payload_t() | dop_with_payload_t().

-export_type([
    raw_dop_alias_send_t/0,
    raw_dop_alias_send_tt_t/0,
    raw_dop_demonitor_p_t/0,
    raw_dop_exit_t/0,
    raw_dop_exit2_t/0,
    raw_dop_exit2_tt_t/0,
    raw_dop_exit_tt_t/0,
    raw_dop_group_leader_t/0,
    raw_dop_link_t/0,
    raw_dop_monitor_p_t/0,
    raw_dop_monitor_p_exit_t/0,
    raw_dop_payload_exit_t/0,
    raw_dop_payload_exit2_t/0,
    raw_dop_payload_exit2_tt_t/0,
    raw_dop_payload_exit_tt_t/0,
    raw_dop_payload_monitor_p_exit_t/0,
    raw_dop_reg_send_t/0,
    raw_dop_reg_send_tt_t/0,
    raw_dop_send_t/0,
    raw_dop_send_sender_t/0,
    raw_dop_send_sender_tt_t/0,
    raw_dop_send_tt_t/0,
    raw_dop_spawn_reply_t/0,
    raw_dop_spawn_reply_tt_t/0,
    raw_dop_spawn_request_t/0,
    raw_dop_spawn_request_tt_t/0,
    raw_dop_unlink_t/0,
    raw_dop_unlink_id_t/0,
    raw_dop_unlink_id_ack_t/0,
    raw_dop_without_payload_t/0,
    raw_dop_with_payload_t/0,
    raw_dop_t/0,
    dop_alias_send_t/0,
    dop_alias_send_tt_t/0,
    dop_demonitor_p_t/0,
    dop_exit_t/0,
    dop_exit2_t/0,
    dop_exit2_tt_t/0,
    dop_exit_tt_t/0,
    dop_group_leader_t/0,
    dop_link_t/0,
    dop_monitor_p_t/0,
    dop_monitor_p_exit_t/0,
    dop_payload_exit_t/0,
    dop_payload_exit2_t/0,
    dop_payload_exit2_tt_t/0,
    dop_payload_exit_tt_t/0,
    dop_payload_monitor_p_exit_t/0,
    dop_reg_send_t/0,
    dop_reg_send_tt_t/0,
    dop_send_t/0,
    dop_send_sender_t/0,
    dop_send_sender_tt_t/0,
    dop_send_tt_t/0,
    dop_spawn_reply_t/0,
    dop_spawn_reply_tt_t/0,
    dop_spawn_request_t/0,
    dop_spawn_request_tt_t/0,
    dop_unlink_t/0,
    dop_unlink_id_t/0,
    dop_unlink_id_ack_t/0,
    dop_without_payload_t/0,
    dop_with_payload_t/0,
    dop_t/0
]).

%%%=============================================================================
%%% Public API functions
%%%=============================================================================

-spec cast_to_dop
    (raw_dop_alias_send_t()) -> dop_alias_send_t();
    (raw_dop_alias_send_tt_t()) -> dop_alias_send_tt_t();
    (raw_dop_demonitor_p_t()) -> dop_demonitor_p_t();
    (raw_dop_exit_t()) -> dop_exit_t();
    (raw_dop_exit2_t()) -> dop_exit2_t();
    (raw_dop_exit2_tt_t()) -> dop_exit2_tt_t();
    (raw_dop_exit_tt_t()) -> dop_exit_tt_t();
    (raw_dop_group_leader_t()) -> dop_group_leader_t();
    (raw_dop_link_t()) -> dop_link_t();
    (raw_dop_monitor_p_t()) -> dop_monitor_p_t();
    (raw_dop_monitor_p_exit_t()) -> dop_monitor_p_exit_t();
    (raw_dop_payload_exit_t()) -> dop_payload_exit_t();
    (raw_dop_payload_exit2_t()) -> dop_payload_exit2_t();
    (raw_dop_payload_exit2_tt_t()) -> dop_payload_exit2_tt_t();
    (raw_dop_payload_exit_tt_t()) -> dop_payload_exit_tt_t();
    (raw_dop_payload_monitor_p_exit_t()) -> dop_payload_monitor_p_exit_t();
    (raw_dop_reg_send_t()) -> dop_reg_send_t();
    (raw_dop_reg_send_tt_t()) -> dop_reg_send_tt_t();
    (raw_dop_send_t()) -> dop_send_t();
    (raw_dop_send_sender_t()) -> dop_send_sender_t();
    (raw_dop_send_sender_tt_t()) -> dop_send_sender_tt_t();
    (raw_dop_send_tt_t()) -> dop_send_tt_t();
    (raw_dop_spawn_reply_t()) -> dop_spawn_reply_t();
    (raw_dop_spawn_reply_tt_t()) -> dop_spawn_reply_tt_t();
    (raw_dop_spawn_request_t()) -> dop_spawn_request_t();
    (raw_dop_spawn_request_tt_t()) -> dop_spawn_request_tt_t();
    (raw_dop_unlink_t()) -> dop_unlink_t();
    (raw_dop_unlink_id_t()) -> dop_unlink_id_t();
    (raw_dop_unlink_id_ack_t()) -> dop_unlink_id_ack_t().
cast_to_dop(T = {?DOP_ALIAS_SEND, _, _}) -> setelement(1, T, udist_dop_alias_send);
cast_to_dop(T = {?DOP_ALIAS_SEND_TT, _, _, _}) -> setelement(1, T, udist_dop_alias_send_tt);
cast_to_dop(T = {?DOP_DEMONITOR_P, _, _, _}) -> setelement(1, T, udist_dop_demonitor_p);
cast_to_dop(T = {?DOP_EXIT, _, _, _}) -> setelement(1, T, udist_dop_exit);
cast_to_dop(T = {?DOP_EXIT2, _, _, _}) -> setelement(1, T, udist_dop_exit2);
cast_to_dop(T = {?DOP_EXIT2_TT, _, _, _, _}) -> setelement(1, T, udist_dop_exit2_tt);
cast_to_dop(T = {?DOP_EXIT_TT, _, _, _, _}) -> setelement(1, T, udist_dop_exit_tt);
cast_to_dop(T = {?DOP_GROUP_LEADER, _, _}) -> setelement(1, T, udist_dop_group_leader);
cast_to_dop(T = {?DOP_LINK, _, _}) -> setelement(1, T, udist_dop_link);
cast_to_dop(T = {?DOP_MONITOR_P, _, _, _}) -> setelement(1, T, udist_dop_monitor_p);
cast_to_dop(T = {?DOP_MONITOR_P_EXIT, _, _, _, _}) -> setelement(1, T, udist_dop_monitor_p_exit);
cast_to_dop(T = {?DOP_PAYLOAD_EXIT, _, _}) -> setelement(1, T, udist_dop_payload_exit);
cast_to_dop(T = {?DOP_PAYLOAD_EXIT2, _, _}) -> setelement(1, T, udist_dop_payload_exit2);
cast_to_dop(T = {?DOP_PAYLOAD_EXIT2_TT, _, _, _}) -> setelement(1, T, udist_dop_payload_exit2_tt);
cast_to_dop(T = {?DOP_PAYLOAD_EXIT_TT, _, _, _}) -> setelement(1, T, udist_dop_payload_exit_tt);
cast_to_dop(T = {?DOP_PAYLOAD_MONITOR_P_EXIT, _, _, _}) -> setelement(1, T, udist_dop_payload_monitor_p_exit);
cast_to_dop(T = {?DOP_REG_SEND, _, _, _}) -> setelement(1, T, udist_dop_reg_send);
cast_to_dop(T = {?DOP_REG_SEND_TT, _, _, _, _}) -> setelement(1, T, udist_dop_reg_send_tt);
cast_to_dop(T = {?DOP_SEND, _, _}) -> setelement(1, T, udist_dop_send);
cast_to_dop(T = {?DOP_SEND_SENDER, _, _}) -> setelement(1, T, udist_dop_send_sender);
cast_to_dop(T = {?DOP_SEND_SENDER_TT, _, _, _}) -> setelement(1, T, udist_dop_send_sender_tt);
cast_to_dop(T = {?DOP_SEND_TT, _, _, _}) -> setelement(1, T, udist_dop_send_tt);
cast_to_dop(T = {?DOP_SPAWN_REPLY, _, _, _, _}) -> setelement(1, T, udist_dop_spawn_reply);
cast_to_dop(T = {?DOP_SPAWN_REPLY_TT, _, _, _, _, _}) -> setelement(1, T, udist_dop_spawn_reply_tt);
cast_to_dop(T = {?DOP_SPAWN_REQUEST, _, _, _, _, _}) -> setelement(1, T, udist_dop_spawn_request);
cast_to_dop(T = {?DOP_SPAWN_REQUEST_TT, _, _, _, _, _, _}) -> setelement(1, T, udist_dop_spawn_request_tt);
cast_to_dop(T = {?DOP_UNLINK, _, _}) -> setelement(1, T, udist_dop_unlink);
cast_to_dop(T = {?DOP_UNLINK_ID, _, _, _}) -> setelement(1, T, udist_dop_unlink_id);
cast_to_dop(T = {?DOP_UNLINK_ID_ACK, _, _, _}) -> setelement(1, T, udist_dop_unlink_id_ack).

-spec cast_to_raw_dop
    (dop_alias_send_t()) -> raw_dop_alias_send_t();
    (dop_alias_send_tt_t()) -> raw_dop_alias_send_tt_t();
    (dop_demonitor_p_t()) -> raw_dop_demonitor_p_t();
    (dop_exit_t()) -> raw_dop_exit_t();
    (dop_exit2_t()) -> raw_dop_exit2_t();
    (dop_exit2_tt_t()) -> raw_dop_exit2_tt_t();
    (dop_exit_tt_t()) -> raw_dop_exit_tt_t();
    (dop_group_leader_t()) -> raw_dop_group_leader_t();
    (dop_link_t()) -> raw_dop_link_t();
    (dop_monitor_p_t()) -> raw_dop_monitor_p_t();
    (dop_monitor_p_exit_t()) -> raw_dop_monitor_p_exit_t();
    (dop_payload_exit_t()) -> raw_dop_payload_exit_t();
    (dop_payload_exit2_t()) -> raw_dop_payload_exit2_t();
    (dop_payload_exit2_tt_t()) -> raw_dop_payload_exit2_tt_t();
    (dop_payload_exit_tt_t()) -> raw_dop_payload_exit_tt_t();
    (dop_payload_monitor_p_exit_t()) -> raw_dop_payload_monitor_p_exit_t();
    (dop_reg_send_t()) -> raw_dop_reg_send_t();
    (dop_reg_send_tt_t()) -> raw_dop_reg_send_tt_t();
    (dop_send_t()) -> raw_dop_send_t();
    (dop_send_sender_t()) -> raw_dop_send_sender_t();
    (dop_send_sender_tt_t()) -> raw_dop_send_sender_tt_t();
    (dop_send_tt_t()) -> raw_dop_send_tt_t();
    (dop_spawn_reply_t()) -> raw_dop_spawn_reply_t();
    (dop_spawn_reply_tt_t()) -> raw_dop_spawn_reply_tt_t();
    (dop_spawn_request_t()) -> raw_dop_spawn_request_t();
    (dop_spawn_request_tt_t()) -> raw_dop_spawn_request_tt_t();
    (dop_unlink_t()) -> raw_dop_unlink_t();
    (dop_unlink_id_t()) -> raw_dop_unlink_id_t();
    (dop_unlink_id_ack_t()) -> raw_dop_unlink_id_ack_t().
cast_to_raw_dop(T = #udist_dop_alias_send{}) -> setelement(1, T, ?DOP_ALIAS_SEND);
cast_to_raw_dop(T = #udist_dop_alias_send_tt{}) -> setelement(1, T, ?DOP_ALIAS_SEND_TT);
cast_to_raw_dop(T = #udist_dop_demonitor_p{}) -> setelement(1, T, ?DOP_DEMONITOR_P);
cast_to_raw_dop(T = #udist_dop_exit{}) -> setelement(1, T, ?DOP_EXIT);
cast_to_raw_dop(T = #udist_dop_exit2{}) -> setelement(1, T, ?DOP_EXIT2);
cast_to_raw_dop(T = #udist_dop_exit2_tt{}) -> setelement(1, T, ?DOP_EXIT2_TT);
cast_to_raw_dop(T = #udist_dop_exit_tt{}) -> setelement(1, T, ?DOP_EXIT_TT);
cast_to_raw_dop(T = #udist_dop_group_leader{}) -> setelement(1, T, ?DOP_GROUP_LEADER);
cast_to_raw_dop(T = #udist_dop_link{}) -> setelement(1, T, ?DOP_LINK);
cast_to_raw_dop(T = #udist_dop_monitor_p{}) -> setelement(1, T, ?DOP_MONITOR_P);
cast_to_raw_dop(T = #udist_dop_monitor_p_exit{}) -> setelement(1, T, ?DOP_MONITOR_P_EXIT);
cast_to_raw_dop(T = #udist_dop_payload_exit{}) -> setelement(1, T, ?DOP_PAYLOAD_EXIT);
cast_to_raw_dop(T = #udist_dop_payload_exit2{}) -> setelement(1, T, ?DOP_PAYLOAD_EXIT2);
cast_to_raw_dop(T = #udist_dop_payload_exit2_tt{}) -> setelement(1, T, ?DOP_PAYLOAD_EXIT2_TT);
cast_to_raw_dop(T = #udist_dop_payload_exit_tt{}) -> setelement(1, T, ?DOP_PAYLOAD_EXIT_TT);
cast_to_raw_dop(T = #udist_dop_payload_monitor_p_exit{}) -> setelement(1, T, ?DOP_PAYLOAD_MONITOR_P_EXIT);
cast_to_raw_dop(T = #udist_dop_reg_send{}) -> setelement(1, T, ?DOP_REG_SEND);
cast_to_raw_dop(T = #udist_dop_reg_send_tt{}) -> setelement(1, T, ?DOP_REG_SEND_TT);
cast_to_raw_dop(T = #udist_dop_send{}) -> setelement(1, T, ?DOP_SEND);
cast_to_raw_dop(T = #udist_dop_send_sender{}) -> setelement(1, T, ?DOP_SEND_SENDER);
cast_to_raw_dop(T = #udist_dop_send_sender_tt{}) -> setelement(1, T, ?DOP_SEND_SENDER_TT);
cast_to_raw_dop(T = #udist_dop_send_tt{}) -> setelement(1, T, ?DOP_SEND_TT);
cast_to_raw_dop(T = #udist_dop_spawn_reply{}) -> setelement(1, T, ?DOP_SPAWN_REPLY);
cast_to_raw_dop(T = #udist_dop_spawn_reply_tt{}) -> setelement(1, T, ?DOP_SPAWN_REPLY_TT);
cast_to_raw_dop(T = #udist_dop_spawn_request{}) -> setelement(1, T, ?DOP_SPAWN_REQUEST);
cast_to_raw_dop(T = #udist_dop_spawn_request_tt{}) -> setelement(1, T, ?DOP_SPAWN_REQUEST_TT);
cast_to_raw_dop(T = #udist_dop_unlink{}) -> setelement(1, T, ?DOP_UNLINK);
cast_to_raw_dop(T = #udist_dop_unlink_id{}) -> setelement(1, T, ?DOP_UNLINK_ID);
cast_to_raw_dop(T = #udist_dop_unlink_id_ack{}) -> setelement(1, T, ?DOP_UNLINK_ID_ACK).

-spec get_dop_group
    (dop_exit_t()) -> 'exit';
    (dop_exit_tt_t()) -> 'exit';
    (dop_payload_exit_t()) -> 'exit';
    (dop_payload_exit_tt_t()) -> 'exit';
    (dop_exit2_t()) -> 'exit2';
    (dop_exit2_tt_t()) -> 'exit2';
    (dop_payload_exit2_t()) -> 'exit2';
    (dop_payload_exit2_tt_t()) -> 'exit2';
    (dop_group_leader_t()) -> 'group_leader';
    (dop_link_t()) -> 'link';
    (dop_monitor_p_t()) -> 'monitor_related';
    (dop_demonitor_p_t()) -> 'monitor_related';
    (dop_monitor_p_exit_t()) -> 'monitor_related';
    (dop_payload_monitor_p_exit_t()) -> 'monitor_related';
    (dop_alias_send_t()) -> 'send_to_alias';
    (dop_alias_send_tt_t()) -> 'send_to_alias';
    (dop_reg_send_t()) -> 'send_to_name';
    (dop_reg_send_tt_t()) -> 'send_to_name';
    (dop_send_t()) -> 'send_to_pid';
    (dop_send_tt_t()) -> 'send_to_pid';
    (dop_send_sender_t()) -> 'send_to_pid';
    (dop_send_sender_tt_t()) -> 'send_to_pid';
    (dop_spawn_reply_t()) -> 'spawn_reply';
    (dop_spawn_reply_tt_t()) -> 'spawn_reply';
    (dop_spawn_request_t()) -> 'spawn_request';
    (dop_spawn_request_tt_t()) -> 'spawn_request';
    (dop_unlink_t()) -> 'unlink';
    (dop_unlink_id_t()) -> 'unlink';
    (dop_unlink_id_ack_t()) -> 'unlink'.
get_dop_group(#udist_dop_exit{}) -> 'exit';
get_dop_group(#udist_dop_exit_tt{}) -> 'exit';
get_dop_group(#udist_dop_payload_exit{}) -> 'exit';
get_dop_group(#udist_dop_payload_exit_tt{}) -> 'exit';
get_dop_group(#udist_dop_exit2{}) -> 'exit2';
get_dop_group(#udist_dop_exit2_tt{}) -> 'exit2';
get_dop_group(#udist_dop_payload_exit2{}) -> 'exit2';
get_dop_group(#udist_dop_payload_exit2_tt{}) -> 'exit2';
get_dop_group(#udist_dop_group_leader{}) -> 'group_leader';
get_dop_group(#udist_dop_link{}) -> 'link';
get_dop_group(#udist_dop_monitor_p{}) -> 'monitor_related';
get_dop_group(#udist_dop_demonitor_p{}) -> 'monitor_related';
get_dop_group(#udist_dop_monitor_p_exit{}) -> 'monitor_related';
get_dop_group(#udist_dop_payload_monitor_p_exit{}) -> 'monitor_related';
get_dop_group(#udist_dop_alias_send{}) -> 'send_to_alias';
get_dop_group(#udist_dop_alias_send_tt{}) -> 'send_to_alias';
get_dop_group(#udist_dop_reg_send{}) -> 'send_to_name';
get_dop_group(#udist_dop_reg_send_tt{}) -> 'send_to_name';
get_dop_group(#udist_dop_send{}) -> 'send_to_pid';
get_dop_group(#udist_dop_send_tt{}) -> 'send_to_pid';
get_dop_group(#udist_dop_send_sender{}) -> 'send_to_pid';
get_dop_group(#udist_dop_send_sender_tt{}) -> 'send_to_pid';
get_dop_group(#udist_dop_spawn_reply{}) -> 'spawn_reply';
get_dop_group(#udist_dop_spawn_reply_tt{}) -> 'spawn_reply';
get_dop_group(#udist_dop_spawn_request{}) -> 'spawn_request';
get_dop_group(#udist_dop_spawn_request_tt{}) -> 'spawn_request';
get_dop_group(#udist_dop_unlink{}) -> 'unlink';
get_dop_group(#udist_dop_unlink_id{}) -> 'unlink';
get_dop_group(#udist_dop_unlink_id_ack{}) -> 'unlink'.

-spec get_dop_name
    (dop_alias_send_t()) -> 'DOP_ALIAS_SEND';
    (dop_alias_send_tt_t()) -> 'DOP_ALIAS_SEND_TT';
    (dop_demonitor_p_t()) -> 'DOP_DEMONITOR_P';
    (dop_exit_t()) -> 'DOP_EXIT';
    (dop_exit2_t()) -> 'DOP_EXIT2';
    (dop_exit2_tt_t()) -> 'DOP_EXIT2_TT';
    (dop_exit_tt_t()) -> 'DOP_EXIT_TT';
    (dop_group_leader_t()) -> 'DOP_GROUP_LEADER';
    (dop_link_t()) -> 'DOP_LINK';
    (dop_monitor_p_t()) -> 'DOP_MONITOR_P';
    (dop_monitor_p_exit_t()) -> 'DOP_MONITOR_P_EXIT';
    (dop_payload_exit_t()) -> 'DOP_PAYLOAD_EXIT';
    (dop_payload_exit2_t()) -> 'DOP_PAYLOAD_EXIT2';
    (dop_payload_exit2_tt_t()) -> 'DOP_PAYLOAD_EXIT2_TT';
    (dop_payload_exit_tt_t()) -> 'DOP_PAYLOAD_EXIT_TT';
    (dop_payload_monitor_p_exit_t()) -> 'DOP_PAYLOAD_MONITOR_P_EXIT';
    (dop_reg_send_t()) -> 'DOP_REG_SEND';
    (dop_reg_send_tt_t()) -> 'DOP_REG_SEND_TT';
    (dop_send_t()) -> 'DOP_SEND';
    (dop_send_sender_t()) -> 'DOP_SEND_SENDER';
    (dop_send_sender_tt_t()) -> 'DOP_SEND_SENDER_TT';
    (dop_send_tt_t()) -> 'DOP_SEND_TT';
    (dop_spawn_reply_t()) -> 'DOP_SPAWN_REPLY';
    (dop_spawn_reply_tt_t()) -> 'DOP_SPAWN_REPLY_TT';
    (dop_spawn_request_t()) -> 'DOP_SPAWN_REQUEST';
    (dop_spawn_request_tt_t()) -> 'DOP_SPAWN_REQUEST_TT';
    (dop_unlink_t()) -> 'DOP_UNLINK';
    (dop_unlink_id_t()) -> 'DOP_UNLINK_ID';
    (dop_unlink_id_ack_t()) -> 'DOP_UNLINK_ID_ACK'.
get_dop_name(#udist_dop_alias_send{}) -> 'DOP_ALIAS_SEND';
get_dop_name(#udist_dop_alias_send_tt{}) -> 'DOP_ALIAS_SEND_TT';
get_dop_name(#udist_dop_demonitor_p{}) -> 'DOP_DEMONITOR_P';
get_dop_name(#udist_dop_exit{}) -> 'DOP_EXIT';
get_dop_name(#udist_dop_exit2{}) -> 'DOP_EXIT2';
get_dop_name(#udist_dop_exit2_tt{}) -> 'DOP_EXIT2_TT';
get_dop_name(#udist_dop_exit_tt{}) -> 'DOP_EXIT_TT';
get_dop_name(#udist_dop_group_leader{}) -> 'DOP_GROUP_LEADER';
get_dop_name(#udist_dop_link{}) -> 'DOP_LINK';
get_dop_name(#udist_dop_monitor_p{}) -> 'DOP_MONITOR_P';
get_dop_name(#udist_dop_monitor_p_exit{}) -> 'DOP_MONITOR_P_EXIT';
get_dop_name(#udist_dop_payload_exit{}) -> 'DOP_PAYLOAD_EXIT';
get_dop_name(#udist_dop_payload_exit2{}) -> 'DOP_PAYLOAD_EXIT2';
get_dop_name(#udist_dop_payload_exit2_tt{}) -> 'DOP_PAYLOAD_EXIT2_TT';
get_dop_name(#udist_dop_payload_exit_tt{}) -> 'DOP_PAYLOAD_EXIT_TT';
get_dop_name(#udist_dop_payload_monitor_p_exit{}) -> 'DOP_PAYLOAD_MONITOR_P_EXIT';
get_dop_name(#udist_dop_reg_send{}) -> 'DOP_REG_SEND';
get_dop_name(#udist_dop_reg_send_tt{}) -> 'DOP_REG_SEND_TT';
get_dop_name(#udist_dop_send{}) -> 'DOP_SEND';
get_dop_name(#udist_dop_send_sender{}) -> 'DOP_SEND_SENDER';
get_dop_name(#udist_dop_send_sender_tt{}) -> 'DOP_SEND_SENDER_TT';
get_dop_name(#udist_dop_send_tt{}) -> 'DOP_SEND_TT';
get_dop_name(#udist_dop_spawn_reply{}) -> 'DOP_SPAWN_REPLY';
get_dop_name(#udist_dop_spawn_reply_tt{}) -> 'DOP_SPAWN_REPLY_TT';
get_dop_name(#udist_dop_spawn_request{}) -> 'DOP_SPAWN_REQUEST';
get_dop_name(#udist_dop_spawn_request_tt{}) -> 'DOP_SPAWN_REQUEST_TT';
get_dop_name(#udist_dop_unlink{}) -> 'DOP_UNLINK';
get_dop_name(#udist_dop_unlink_id{}) -> 'DOP_UNLINK_ID';
get_dop_name(#udist_dop_unlink_id_ack{}) -> 'DOP_UNLINK_ID_ACK'.
