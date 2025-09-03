%%% % @format
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
%% @oncall whatsapp_clr
-ifndef(UDIST_HRL).

-define(UDIST_HRL, 1).

-include_lib("erldist_filter/include/erldist_filter_erts_dist.hrl").

-record(udist_dop_alias_send, {
    from_pid :: pid(),
    alias :: reference()
}).

-record(udist_dop_alias_send_tt, {
    from_pid :: pid(),
    alias :: reference(),
    token :: term()
}).

-record(udist_dop_altact_sig_send, {
    flags :: integer(),
    sender_pid :: pid(),
    to :: pid() | atom() | reference(),
    token :: none | {some, term()}
}).

-record(udist_dop_demonitor_p, {
    from_pid :: pid(),
    to_proc :: atom() | pid(),
    ref :: reference()
}).

-record(udist_dop_exit, {
    from_pid :: pid(),
    to_pid :: pid(),
    reason :: term()
}).

-record(udist_dop_exit2, {
    from_pid :: pid(),
    to_pid :: pid(),
    reason :: term()
}).

-record(udist_dop_exit2_tt, {
    from_pid :: pid(),
    to_pid :: pid(),
    trace_token :: term(),
    reason :: term()
}).

-record(udist_dop_exit_tt, {
    from_pid :: pid(),
    to_pid :: pid(),
    trace_token :: term(),
    reason :: term()
}).

-record(udist_dop_group_leader, {
    from_pid :: pid(),
    to_pid :: pid()
}).

-record(udist_dop_link, {
    from_pid :: pid(),
    to_pid :: pid()
}).

-record(udist_dop_monitor_p, {
    from_pid :: pid(),
    to_proc :: atom() | pid(),
    ref :: reference()
}).

-record(udist_dop_monitor_p_exit, {
    from_proc :: atom() | pid(),
    to_pid :: pid(),
    ref :: reference(),
    reason :: term()
}).

-record(udist_dop_payload_exit, {
    from_pid :: pid(),
    to_pid :: pid()
}).

-record(udist_dop_payload_exit2, {
    from_pid :: pid(),
    to_pid :: pid()
}).

-record(udist_dop_payload_exit2_tt, {
    from_pid :: pid(),
    to_pid :: pid(),
    trace_token :: term()
}).

-record(udist_dop_payload_exit_tt, {
    from_pid :: pid(),
    to_pid :: pid(),
    trace_token :: term()
}).

-record(udist_dop_payload_monitor_p_exit, {
    from_proc :: atom() | pid(),
    to_pid :: pid(),
    ref :: reference()
}).

-record(udist_dop_reg_send, {
    from_pid :: pid(),
    unused :: term(),
    to_name :: atom()
}).

-record(udist_dop_reg_send_tt, {
    from_pid :: pid(),
    unused :: term(),
    to_name :: atom(),
    trace_token :: term()
}).

-record(udist_dop_send, {
    unused :: term(),
    to_pid :: pid()
}).

-record(udist_dop_send_sender, {
    from_pid :: pid(),
    to_pid :: pid()
}).

-record(udist_dop_send_sender_tt, {
    from_pid :: pid(),
    to_pid :: pid(),
    trace_token :: term()
}).

-record(udist_dop_send_tt, {
    unused :: term(),
    to_pid :: pid(),
    trace_token :: term()
}).

-record(udist_dop_spawn_reply, {
    req_id :: reference(),
    to :: pid(),
    flags :: integer(),
    result :: atom() | pid()
}).

-record(udist_dop_spawn_reply_tt, {
    req_id :: reference(),
    to :: pid(),
    flags :: integer(),
    result :: atom() | pid(),
    token :: term()
}).

-record(udist_dop_spawn_request, {
    req_id :: reference(),
    from :: pid(),
    group_leader :: pid(),
    mfa :: {atom(), atom(), integer()},
    opt_list :: [term()]
}).

-record(udist_dop_spawn_request_tt, {
    req_id :: reference(),
    from :: pid(),
    group_leader :: pid(),
    mfa :: {atom(), atom(), integer()},
    opt_list :: [term()],
    token :: term()
}).

-record(udist_dop_unlink, {
    from_pid :: pid(),
    to_pid :: pid()
}).

-record(udist_dop_unlink_id, {
    id :: integer(),
    from_pid :: pid(),
    to_pid :: pid()
}).

-record(udist_dop_unlink_id_ack, {
    id :: integer(),
    from_pid :: pid(),
    to_pid :: pid()
}).

-define(is_udist_dop_exit_t(T),
    (is_record(T, udist_dop_exit) orelse
        is_record(T, udist_dop_exit_tt) orelse
        is_record(T, udist_dop_payload_exit) orelse
        is_record(T, udist_dop_payload_exit_tt))
).

-define(is_udist_dop_exit2_t(T),
    (is_record(T, udist_dop_exit2) orelse
        is_record(T, udist_dop_exit2_tt) orelse
        is_record(T, udist_dop_payload_exit2) orelse
        is_record(T, udist_dop_payload_exit2_tt) orelse
        (is_record(T, udist_dop_altact_sig_send) andalso
            (T)#udist_dop_altact_sig_send.flags band ?ERTS_DOP_ALTACT_SIG_FLG_EXIT =/= 0))
).

-define(is_udist_dop_group_leader_t(T),
    (is_record(T, udist_dop_group_leader))
).

-define(is_udist_dop_link_t(T),
    (is_record(T, udist_dop_link))
).

-define(is_udist_dop_monitor_related_t(T),
    (is_record(T, udist_dop_monitor_p) orelse
        is_record(T, udist_dop_demonitor_p) orelse
        is_record(T, udist_dop_monitor_p_exit) orelse
        is_record(T, udist_dop_payload_monitor_p_exit))
).

-define(is_udist_dop_send_to_alias_t(T),
    (is_record(T, udist_dop_alias_send) orelse
        is_record(T, udist_dop_alias_send_tt) orelse
        (is_record(T, udist_dop_altact_sig_send) andalso
            (T)#udist_dop_altact_sig_send.flags band ?ERTS_DOP_ALTACT_SIG_FLG_ALIAS =/= 0))
).

-define(is_udist_dop_send_to_name_t(T),
    (is_record(T, udist_dop_reg_send) orelse
        is_record(T, udist_dop_reg_send_tt) orelse
        (is_record(T, udist_dop_altact_sig_send) andalso
            (T)#udist_dop_altact_sig_send.flags band ?ERTS_DOP_ALTACT_SIG_FLG_NAME =/= 0))
).

-define(is_udist_dop_send_to_pid_t(T),
    (is_record(T, udist_dop_send) orelse
        is_record(T, udist_dop_send_tt) orelse
        is_record(T, udist_dop_send_sender) orelse
        is_record(T, udist_dop_send_sender_tt) orelse
        (is_record(T, udist_dop_altact_sig_send) andalso
            (T)#udist_dop_altact_sig_send.flags band
                (?ERTS_DOP_ALTACT_SIG_FLG_ALIAS bor ?ERTS_DOP_ALTACT_SIG_FLG_EXIT bor ?ERTS_DOP_ALTACT_SIG_FLG_NAME) =:=
                0))
).

-define(is_udist_dop_spawn_reply_t(T),
    (is_record(T, udist_dop_spawn_reply) orelse
        is_record(T, udist_dop_spawn_reply_tt))
).

-define(is_udist_dop_spawn_request_t(T),
    (is_record(T, udist_dop_spawn_request) orelse
        is_record(T, udist_dop_spawn_request_tt))
).

-define(is_udist_dop_unlink_t(T),
    (is_record(T, udist_dop_unlink) orelse
        is_record(T, udist_dop_unlink_id) orelse
        is_record(T, udist_dop_unlink_id_ack))
).

-define(is_udist_dop_without_payload_t(T),
    (is_record(T, udist_dop_demonitor_p) orelse
        is_record(T, udist_dop_exit) orelse
        is_record(T, udist_dop_exit2) orelse
        is_record(T, udist_dop_exit2_tt) orelse
        is_record(T, udist_dop_exit_tt) orelse
        is_record(T, udist_dop_group_leader) orelse
        is_record(T, udist_dop_link) orelse
        is_record(T, udist_dop_monitor_p) orelse
        is_record(T, udist_dop_monitor_p_exit) orelse
        is_record(T, udist_dop_spawn_reply) orelse
        is_record(T, udist_dop_spawn_reply_tt) orelse
        is_record(T, udist_dop_unlink) orelse
        is_record(T, udist_dop_unlink_id) orelse
        is_record(T, udist_dop_unlink_id_ack))
).

-define(is_udist_dop_with_payload_t(T),
    (is_record(T, udist_dop_alias_send) orelse
        is_record(T, udist_dop_alias_send_tt) orelse
        is_record(T, udist_dop_altact_sig_send) orelse
        is_record(T, udist_dop_payload_exit) orelse
        is_record(T, udist_dop_payload_exit2) orelse
        is_record(T, udist_dop_payload_exit2_tt) orelse
        is_record(T, udist_dop_payload_exit_tt) orelse
        is_record(T, udist_dop_payload_monitor_p_exit) orelse
        is_record(T, udist_dop_reg_send) orelse
        is_record(T, udist_dop_reg_send_tt) orelse
        is_record(T, udist_dop_send) orelse
        is_record(T, udist_dop_send_sender) orelse
        is_record(T, udist_dop_send_sender_tt) orelse
        is_record(T, udist_dop_send_tt) orelse
        is_record(T, udist_dop_spawn_request) orelse
        is_record(T, udist_dop_spawn_request_tt))
).

-define(is_udist_dop_t(T),
    (?is_udist_dop_without_payload_t(T) orelse ?is_udist_dop_with_payload_t(T))
).

-endif.
