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
%%% Created :  11 May 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(erldist_filter_config_SUITE).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% ct callbacks
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2
]).

%% Test Cases
-export([
    compact_fragments_fragment_spawn_request_test/0,
    compact_fragments_fragment_spawn_request_test/1,
    compact_fragments_normal_spawn_request_test/0,
    compact_fragments_normal_spawn_request_test/1,
    compact_fragments_pass_through_spawn_request_test/0,
    compact_fragments_pass_through_spawn_request_test/1,
    logging_fragment_spawn_request_test/0,
    logging_fragment_spawn_request_test/1,
    logging_normal_spawn_request_test/0,
    logging_normal_spawn_request_test/1,
    logging_pass_through_spawn_request_test/0,
    logging_pass_through_spawn_request_test/1,
    traffic_drop_fragment_exit_test/0,
    traffic_drop_fragment_exit_test/1,
    traffic_drop_fragment_exit2_test/0,
    traffic_drop_fragment_exit2_test/1,
    traffic_drop_fragment_group_leader_test/0,
    traffic_drop_fragment_group_leader_test/1,
    traffic_drop_fragment_link_test/0,
    traffic_drop_fragment_link_test/1,
    traffic_drop_fragment_monitor_related_test/0,
    traffic_drop_fragment_monitor_related_test/1,
    traffic_drop_fragment_send_to_alias_test/0,
    traffic_drop_fragment_send_to_alias_test/1,
    traffic_drop_fragment_send_to_name_test/0,
    traffic_drop_fragment_send_to_name_test/1,
    traffic_drop_fragment_send_to_pid_test/0,
    traffic_drop_fragment_send_to_pid_test/1,
    traffic_drop_fragment_spawn_reply_test/0,
    traffic_drop_fragment_spawn_reply_test/1,
    traffic_drop_fragment_spawn_request_test/0,
    traffic_drop_fragment_spawn_request_test/1,
    traffic_drop_fragment_unlink_test/0,
    traffic_drop_fragment_unlink_test/1,
    traffic_drop_normal_exit_test/0,
    traffic_drop_normal_exit_test/1,
    traffic_drop_normal_exit2_test/0,
    traffic_drop_normal_exit2_test/1,
    traffic_drop_normal_group_leader_test/0,
    traffic_drop_normal_group_leader_test/1,
    traffic_drop_normal_link_test/0,
    traffic_drop_normal_link_test/1,
    traffic_drop_normal_monitor_related_test/0,
    traffic_drop_normal_monitor_related_test/1,
    traffic_drop_normal_send_to_alias_test/0,
    traffic_drop_normal_send_to_alias_test/1,
    traffic_drop_normal_send_to_name_test/0,
    traffic_drop_normal_send_to_name_test/1,
    traffic_drop_normal_send_to_pid_test/0,
    traffic_drop_normal_send_to_pid_test/1,
    traffic_drop_normal_spawn_reply_test/0,
    traffic_drop_normal_spawn_reply_test/1,
    traffic_drop_normal_spawn_request_test/0,
    traffic_drop_normal_spawn_request_test/1,
    traffic_drop_normal_unlink_test/0,
    traffic_drop_normal_unlink_test/1,
    traffic_drop_pass_through_exit_test/0,
    traffic_drop_pass_through_exit_test/1,
    traffic_drop_pass_through_exit2_test/0,
    traffic_drop_pass_through_exit2_test/1,
    traffic_drop_pass_through_group_leader_test/0,
    traffic_drop_pass_through_group_leader_test/1,
    traffic_drop_pass_through_link_test/0,
    traffic_drop_pass_through_link_test/1,
    traffic_drop_pass_through_monitor_related_test/0,
    traffic_drop_pass_through_monitor_related_test/1,
    traffic_drop_pass_through_send_to_alias_test/0,
    traffic_drop_pass_through_send_to_alias_test/1,
    traffic_drop_pass_through_send_to_name_test/0,
    traffic_drop_pass_through_send_to_name_test/1,
    traffic_drop_pass_through_send_to_pid_test/0,
    traffic_drop_pass_through_send_to_pid_test/1,
    traffic_drop_pass_through_spawn_reply_test/0,
    traffic_drop_pass_through_spawn_reply_test/1,
    traffic_drop_pass_through_spawn_request_test/0,
    traffic_drop_pass_through_spawn_request_test/1,
    traffic_drop_pass_through_unlink_test/0,
    traffic_drop_pass_through_unlink_test/1,
    traffic_redirect_fragment_exit_test/0,
    traffic_redirect_fragment_exit_test/1,
    traffic_redirect_fragment_exit2_test/0,
    traffic_redirect_fragment_exit2_test/1,
    traffic_redirect_fragment_group_leader_test/0,
    traffic_redirect_fragment_group_leader_test/1,
    traffic_redirect_fragment_link_test/0,
    traffic_redirect_fragment_link_test/1,
    traffic_redirect_fragment_monitor_related_test/0,
    traffic_redirect_fragment_monitor_related_test/1,
    traffic_redirect_fragment_send_to_alias_test/0,
    traffic_redirect_fragment_send_to_alias_test/1,
    traffic_redirect_fragment_send_to_name_test/0,
    traffic_redirect_fragment_send_to_name_test/1,
    traffic_redirect_fragment_send_to_pid_test/0,
    traffic_redirect_fragment_send_to_pid_test/1,
    traffic_redirect_fragment_spawn_reply_test/0,
    traffic_redirect_fragment_spawn_reply_test/1,
    traffic_redirect_fragment_spawn_request_test/0,
    traffic_redirect_fragment_spawn_request_test/1,
    traffic_redirect_fragment_unlink_test/0,
    traffic_redirect_fragment_unlink_test/1,
    traffic_redirect_normal_exit_test/0,
    traffic_redirect_normal_exit_test/1,
    traffic_redirect_normal_exit2_test/0,
    traffic_redirect_normal_exit2_test/1,
    traffic_redirect_normal_group_leader_test/0,
    traffic_redirect_normal_group_leader_test/1,
    traffic_redirect_normal_link_test/0,
    traffic_redirect_normal_link_test/1,
    traffic_redirect_normal_monitor_related_test/0,
    traffic_redirect_normal_monitor_related_test/1,
    traffic_redirect_normal_send_to_alias_test/0,
    traffic_redirect_normal_send_to_alias_test/1,
    traffic_redirect_normal_send_to_name_test/0,
    traffic_redirect_normal_send_to_name_test/1,
    traffic_redirect_normal_send_to_pid_test/0,
    traffic_redirect_normal_send_to_pid_test/1,
    traffic_redirect_normal_spawn_reply_test/0,
    traffic_redirect_normal_spawn_reply_test/1,
    traffic_redirect_normal_spawn_request_test/0,
    traffic_redirect_normal_spawn_request_test/1,
    traffic_redirect_normal_unlink_test/0,
    traffic_redirect_normal_unlink_test/1,
    traffic_redirect_pass_through_exit_test/0,
    traffic_redirect_pass_through_exit_test/1,
    traffic_redirect_pass_through_exit2_test/0,
    traffic_redirect_pass_through_exit2_test/1,
    traffic_redirect_pass_through_group_leader_test/0,
    traffic_redirect_pass_through_group_leader_test/1,
    traffic_redirect_pass_through_link_test/0,
    traffic_redirect_pass_through_link_test/1,
    traffic_redirect_pass_through_monitor_related_test/0,
    traffic_redirect_pass_through_monitor_related_test/1,
    traffic_redirect_pass_through_send_to_alias_test/0,
    traffic_redirect_pass_through_send_to_alias_test/1,
    traffic_redirect_pass_through_send_to_name_test/0,
    traffic_redirect_pass_through_send_to_name_test/1,
    traffic_redirect_pass_through_send_to_pid_test/0,
    traffic_redirect_pass_through_send_to_pid_test/1,
    traffic_redirect_pass_through_spawn_reply_test/0,
    traffic_redirect_pass_through_spawn_reply_test/1,
    traffic_redirect_pass_through_spawn_request_test/0,
    traffic_redirect_pass_through_spawn_request_test/1,
    traffic_redirect_pass_through_unlink_test/0,
    traffic_redirect_pass_through_unlink_test/1
]).

%%%=============================================================================
%%% ct callbacks
%%%=============================================================================

all() ->
    [
        {group, config}
    ].

groups() ->
    [
        {config, [shuffle], [
            compact_fragments_fragment_spawn_request_test,
            compact_fragments_normal_spawn_request_test,
            compact_fragments_pass_through_spawn_request_test,
            logging_fragment_spawn_request_test,
            logging_normal_spawn_request_test,
            logging_pass_through_spawn_request_test,
            traffic_drop_fragment_exit_test,
            traffic_drop_fragment_exit_test,
            traffic_drop_fragment_exit2_test,
            traffic_drop_fragment_exit2_test,
            traffic_drop_fragment_group_leader_test,
            traffic_drop_fragment_group_leader_test,
            traffic_drop_fragment_link_test,
            traffic_drop_fragment_link_test,
            traffic_drop_fragment_monitor_related_test,
            traffic_drop_fragment_monitor_related_test,
            traffic_drop_fragment_send_to_alias_test,
            traffic_drop_fragment_send_to_alias_test,
            traffic_drop_fragment_send_to_name_test,
            traffic_drop_fragment_send_to_name_test,
            traffic_drop_fragment_send_to_pid_test,
            traffic_drop_fragment_send_to_pid_test,
            traffic_drop_fragment_spawn_reply_test,
            traffic_drop_fragment_spawn_reply_test,
            traffic_drop_fragment_spawn_request_test,
            traffic_drop_fragment_spawn_request_test,
            traffic_drop_fragment_unlink_test,
            traffic_drop_fragment_unlink_test,
            traffic_drop_normal_exit_test,
            traffic_drop_normal_exit_test,
            traffic_drop_normal_exit2_test,
            traffic_drop_normal_exit2_test,
            traffic_drop_normal_group_leader_test,
            traffic_drop_normal_group_leader_test,
            traffic_drop_normal_link_test,
            traffic_drop_normal_link_test,
            traffic_drop_normal_monitor_related_test,
            traffic_drop_normal_monitor_related_test,
            traffic_drop_normal_send_to_alias_test,
            traffic_drop_normal_send_to_alias_test,
            traffic_drop_normal_send_to_name_test,
            traffic_drop_normal_send_to_name_test,
            traffic_drop_normal_send_to_pid_test,
            traffic_drop_normal_send_to_pid_test,
            traffic_drop_normal_spawn_reply_test,
            traffic_drop_normal_spawn_reply_test,
            traffic_drop_normal_spawn_request_test,
            traffic_drop_normal_spawn_request_test,
            traffic_drop_normal_unlink_test,
            traffic_drop_normal_unlink_test,
            traffic_drop_pass_through_exit_test,
            traffic_drop_pass_through_exit_test,
            traffic_drop_pass_through_exit2_test,
            traffic_drop_pass_through_exit2_test,
            traffic_drop_pass_through_group_leader_test,
            traffic_drop_pass_through_group_leader_test,
            traffic_drop_pass_through_link_test,
            traffic_drop_pass_through_link_test,
            traffic_drop_pass_through_monitor_related_test,
            traffic_drop_pass_through_monitor_related_test,
            traffic_drop_pass_through_send_to_alias_test,
            traffic_drop_pass_through_send_to_alias_test,
            traffic_drop_pass_through_send_to_name_test,
            traffic_drop_pass_through_send_to_name_test,
            traffic_drop_pass_through_send_to_pid_test,
            traffic_drop_pass_through_send_to_pid_test,
            traffic_drop_pass_through_spawn_reply_test,
            traffic_drop_pass_through_spawn_reply_test,
            traffic_drop_pass_through_spawn_request_test,
            traffic_drop_pass_through_spawn_request_test,
            traffic_drop_pass_through_unlink_test,
            traffic_drop_pass_through_unlink_test,
            traffic_redirect_fragment_exit_test,
            traffic_redirect_fragment_exit_test,
            traffic_redirect_fragment_exit2_test,
            traffic_redirect_fragment_exit2_test,
            traffic_redirect_fragment_group_leader_test,
            traffic_redirect_fragment_group_leader_test,
            traffic_redirect_fragment_link_test,
            traffic_redirect_fragment_link_test,
            traffic_redirect_fragment_monitor_related_test,
            traffic_redirect_fragment_monitor_related_test,
            traffic_redirect_fragment_send_to_alias_test,
            traffic_redirect_fragment_send_to_alias_test,
            traffic_redirect_fragment_send_to_name_test,
            traffic_redirect_fragment_send_to_name_test,
            traffic_redirect_fragment_send_to_pid_test,
            traffic_redirect_fragment_send_to_pid_test,
            traffic_redirect_fragment_spawn_reply_test,
            traffic_redirect_fragment_spawn_reply_test,
            traffic_redirect_fragment_spawn_request_test,
            traffic_redirect_fragment_spawn_request_test,
            traffic_redirect_fragment_unlink_test,
            traffic_redirect_fragment_unlink_test,
            traffic_redirect_normal_exit_test,
            traffic_redirect_normal_exit_test,
            traffic_redirect_normal_exit2_test,
            traffic_redirect_normal_exit2_test,
            traffic_redirect_normal_group_leader_test,
            traffic_redirect_normal_group_leader_test,
            traffic_redirect_normal_link_test,
            traffic_redirect_normal_link_test,
            traffic_redirect_normal_monitor_related_test,
            traffic_redirect_normal_monitor_related_test,
            traffic_redirect_normal_send_to_alias_test,
            traffic_redirect_normal_send_to_alias_test,
            traffic_redirect_normal_send_to_name_test,
            traffic_redirect_normal_send_to_name_test,
            traffic_redirect_normal_send_to_pid_test,
            traffic_redirect_normal_send_to_pid_test,
            traffic_redirect_normal_spawn_reply_test,
            traffic_redirect_normal_spawn_reply_test,
            traffic_redirect_normal_spawn_request_test,
            traffic_redirect_normal_spawn_request_test,
            traffic_redirect_normal_unlink_test,
            traffic_redirect_normal_unlink_test,
            traffic_redirect_pass_through_exit_test,
            traffic_redirect_pass_through_exit_test,
            traffic_redirect_pass_through_exit2_test,
            traffic_redirect_pass_through_exit2_test,
            traffic_redirect_pass_through_group_leader_test,
            traffic_redirect_pass_through_group_leader_test,
            traffic_redirect_pass_through_link_test,
            traffic_redirect_pass_through_link_test,
            traffic_redirect_pass_through_monitor_related_test,
            traffic_redirect_pass_through_monitor_related_test,
            traffic_redirect_pass_through_send_to_alias_test,
            traffic_redirect_pass_through_send_to_alias_test,
            traffic_redirect_pass_through_send_to_name_test,
            traffic_redirect_pass_through_send_to_name_test,
            traffic_redirect_pass_through_send_to_pid_test,
            traffic_redirect_pass_through_send_to_pid_test,
            traffic_redirect_pass_through_spawn_reply_test,
            traffic_redirect_pass_through_spawn_reply_test,
            traffic_redirect_pass_through_spawn_request_test,
            traffic_redirect_pass_through_spawn_request_test,
            traffic_redirect_pass_through_unlink_test,
            traffic_redirect_pass_through_unlink_test
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    ok = config_set_default(),
    Config.

end_per_group(_Group, _Config) ->
    ok = config_set_default(),
    ok.

%%%=============================================================================
%%% Test Cases
%%%=============================================================================

compact_fragments_spawn_request_test(_Config, DFlags, SendOptions) ->
    ConfigMap = #{
        compact_fragments => true, deep_packet_inspection => false, logging => false, redirect_dist_operations => false
    },
    ok = config_set(ConfigMap),
    PacketSize = 4,
    [A, B, C] = [a, b, c],
    Sysname = 'nonode@nohost',
    Channel = erldist_filter_nif:channel_open(PacketSize, Sysname, 0, 0, DFlags),
    C0 = vedf_channel:new(PacketSize, DFlags, ConfigMap),
    ControlA = spawn_request_noop(0, 0, 0),
    ControlB = spawn_request_noop(1, 1, 1),
    ControlC = spawn_request_noop(2, 2, 2),
    LargeBin = binary:copy(<<"a">>, 255),
    Payload = [{LargeBin, A, B, C}],
    {ok, P0, C0} = vedf_channel:send_encode(C0, ControlA, vterm:expand(Payload), SendOptions),
    {ok, A1, C1} = vedf_channel:recv(C0, P0),
    {ok, P1, C1} = vedf_channel:send_encode(C1, ControlB, vterm:expand(Payload), SendOptions),
    {ok, A2, C2} = vedf_channel:recv(C1, P1),
    {ok, P2, C2} = vedf_channel:send_encode(C2, ControlC, vterm:expand(Payload), SendOptions),
    {ok, A3, C3} = vedf_channel:recv(C2, P2),
    AllPackets = lists:concat([P0, P1, P2]),
    AllActions = lists:concat([A1, A2, A3]),
    ?assertEqual(AllActions, simplify_logger_time(erldist_filter_nif:channel_recv(Channel, AllPackets))),
    _ = C3,
    ok = erldist_filter_nif:channel_close(Channel),
    ok = config_set_default(),
    ok.

compact_fragments_fragment_spawn_request_test() ->
    [
        {doc, "Tests sending multiple fragment packets to erldist_filter_nif and that compact_fragments works"},
        {timetrap, {seconds, 60}}
    ].

compact_fragments_fragment_spawn_request_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    compact_fragments_spawn_request_test(Config, DFlags, SendOptions).

compact_fragments_normal_spawn_request_test() ->
    [
        {doc, "Tests sending multiple normal packets to erldist_filter_nif and that compact_fragments works"},
        {timetrap, {seconds, 60}}
    ].

compact_fragments_normal_spawn_request_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    compact_fragments_spawn_request_test(Config, DFlags, SendOptions).

compact_fragments_pass_through_spawn_request_test() ->
    [
        {doc, "Tests sending multiple pass-through packets to erldist_filter_nif and that compact_fragments works"},
        {timetrap, {seconds, 60}}
    ].

compact_fragments_pass_through_spawn_request_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    compact_fragments_spawn_request_test(Config, DFlags, SendOptions).

logging_spawn_request_test(_Config, DFlags, SendOptions) ->
    ConfigMap = #{
        compact_fragments => true, deep_packet_inspection => true, logging => true, redirect_dist_operations => false
    },
    ok = config_set(ConfigMap),
    ?assertEqual([], erldist_filter_nif:logger_list()),
    _OldCapacity = erldist_filter_nif:logger_set_capacity(2),
    Logger = erldist_filter_nif:logger_open(),
    LoggerSelectHandle = flush_logger(Logger),
    PacketSize = 4,
    [A, B, C] = [a, b, c],
    Sysname = 'nonode@nohost',
    Channel = erldist_filter_nif:channel_open(PacketSize, Sysname, 0, 0, DFlags),
    C0 = vedf_channel:new(PacketSize, DFlags, ConfigMap#{sysname => Sysname}),
    ControlA = spawn_request_noop(0, 0, 0),
    ControlB = spawn_request_noop(1, 1, 1),
    ControlC = spawn_request_noop(2, 2, 2),
    LargeBin = binary:copy(<<"a">>, 255),
    Payload = [{LargeBin, A, B, C}],
    {ok, P0, C0} = vedf_channel:send_encode(C0, ControlA, vterm:expand(Payload), SendOptions),
    {ok, A1, C1} = vedf_channel:recv(C0, P0),
    {ok, P1, C1} = vedf_channel:send_encode(C1, ControlB, vterm:expand(Payload), SendOptions),
    {ok, A2, C2} = vedf_channel:recv(C1, P1),
    {ok, P2, C2} = vedf_channel:send_encode(C2, ControlC, vterm:expand(Payload), SendOptions),
    {ok, A3, C3} = vedf_channel:recv(C2, P2),
    AllPackets = lists:concat([P0, P1, P2]),
    AllActions0 = lists:concat([A1, A2, A3]),
    {AllActions, [_Log_A, Log_B, Log_C]} = extract_action_logs(AllActions0, [], []),
    {log, _, {_, {Sysname, Atoms_B, Control_B, Payload_B}}} = Log_B,
    {log, _, {_, {Sysname, Atoms_C, Control_C, Payload_C}}} = Log_C,
    ?assertEqual(AllActions, erldist_filter_nif:channel_recv(Channel, AllPackets)),
    _ = C3,
    LoggerMessage = first_message_in_mailbox(1000),
    ?assertEqual({'$logger', Logger, select, LoggerSelectHandle}, LoggerMessage),
    ?assertMatch(
        {2, 1, [{_, {Sysname, Atoms_B, Control_B, Payload_B}}, {_, {Sysname, Atoms_C, Control_C, Payload_C}}]},
        erldist_filter_nif:logger_recv(Logger)
    ),
    ok = erldist_filter_nif:channel_close(Channel),
    _ = flush_logger(Logger),
    ok = erldist_filter_nif:logger_close(Logger),
    ok = config_set_default(),
    ok.

logging_fragment_spawn_request_test() ->
    [
        {doc, "Tests sending multiple fragment packets to erldist_filter_nif and that logging works"},
        {timetrap, {seconds, 60}}
    ].

logging_fragment_spawn_request_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    logging_spawn_request_test(Config, DFlags, SendOptions).

logging_normal_spawn_request_test() ->
    [
        {doc, "Tests sending multiple normal packets to erldist_filter_nif and that logging works"},
        {timetrap, {seconds, 60}}
    ].

logging_normal_spawn_request_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    logging_spawn_request_test(Config, DFlags, SendOptions).

logging_pass_through_spawn_request_test() ->
    [
        {doc, "Tests sending multiple pass-through packets to erldist_filter_nif and that logging works"},
        {timetrap, {seconds, 60}}
    ].

logging_pass_through_spawn_request_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    logging_spawn_request_test(Config, DFlags, SendOptions).

traffic_drop_fragment_exit_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that dropping works for all exit type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_fragment_exit_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_exit_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_fragment_exit2_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that dropping works for all exit2 type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_fragment_exit2_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_exit2_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_fragment_group_leader_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that dropping works for all group_leader type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_fragment_group_leader_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_group_leader_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_fragment_link_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that dropping works for all link type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_fragment_link_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_link_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_fragment_monitor_related_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that dropping works for all monitor_related type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_fragment_monitor_related_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_monitor_related_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_fragment_send_to_alias_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that dropping works for all send_to_alias type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_fragment_send_to_alias_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_send_to_alias_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_fragment_send_to_name_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that dropping works for all send_to_name type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_fragment_send_to_name_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_send_to_name_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_fragment_send_to_pid_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that dropping works for all send_to_pid type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_fragment_send_to_pid_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_send_to_pid_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_fragment_spawn_reply_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that dropping works for all spawn_reply type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_fragment_spawn_reply_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_spawn_reply_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_fragment_spawn_request_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that dropping works for all spawn_request type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_fragment_spawn_request_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_spawn_request_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_fragment_unlink_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that dropping works for all unlink type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_fragment_unlink_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_unlink_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_normal_exit_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that dropping works for all exit type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_normal_exit_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_exit_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_normal_exit2_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that dropping works for all exit2 type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_normal_exit2_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_exit2_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_normal_group_leader_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that dropping works for all group_leader type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_normal_group_leader_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_group_leader_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_normal_link_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that dropping works for all link type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_normal_link_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_link_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_normal_monitor_related_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that dropping works for all monitor_related type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_normal_monitor_related_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_monitor_related_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_normal_send_to_alias_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that dropping works for all send_to_alias type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_normal_send_to_alias_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_send_to_alias_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_normal_send_to_name_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that dropping works for all send_to_name type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_normal_send_to_name_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_send_to_name_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_normal_send_to_pid_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that dropping works for all send_to_pid type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_normal_send_to_pid_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_send_to_pid_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_normal_spawn_reply_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that dropping works for all spawn_reply type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_normal_spawn_reply_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_spawn_reply_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_normal_spawn_request_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that dropping works for all spawn_request type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_normal_spawn_request_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_spawn_request_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_normal_unlink_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that dropping works for all unlink type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_normal_unlink_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_unlink_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_pass_through_exit_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that dropping works for all exit type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_pass_through_exit_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_exit_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_pass_through_exit2_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that dropping works for all exit2 type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_pass_through_exit2_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_exit2_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_pass_through_group_leader_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that dropping works for all group_leader type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_pass_through_group_leader_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_group_leader_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_pass_through_link_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that dropping works for all link type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_pass_through_link_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_link_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_pass_through_monitor_related_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that dropping works for all monitor_related type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_pass_through_monitor_related_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_monitor_related_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_pass_through_send_to_alias_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that dropping works for all send_to_alias type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_pass_through_send_to_alias_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_send_to_alias_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_pass_through_send_to_name_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that dropping works for all send_to_name type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_pass_through_send_to_name_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_send_to_name_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_pass_through_send_to_pid_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that dropping works for all send_to_pid type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_pass_through_send_to_pid_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_send_to_pid_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_pass_through_spawn_reply_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that dropping works for all spawn_reply type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_pass_through_spawn_reply_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_spawn_reply_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_pass_through_spawn_request_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that dropping works for all spawn_request type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_pass_through_spawn_request_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_spawn_request_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_drop_pass_through_unlink_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that dropping works for all unlink type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_drop_pass_through_unlink_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_unlink_test(Config, DFlags, SendOptions, #{redirect_dist_operations => false}).

traffic_redirect_fragment_exit_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that redirecting works for all exit type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_fragment_exit_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_exit_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_fragment_exit2_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that redirecting works for all exit2 type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_fragment_exit2_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_exit2_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_fragment_group_leader_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that redirecting works for all group_leader type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_fragment_group_leader_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_group_leader_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_fragment_link_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that redirecting works for all link type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_fragment_link_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_link_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_fragment_monitor_related_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that redirecting works for all monitor_related type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_fragment_monitor_related_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_monitor_related_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_fragment_send_to_alias_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that redirecting works for all send_to_alias type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_fragment_send_to_alias_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_send_to_alias_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_fragment_send_to_name_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that redirecting works for all send_to_name type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_fragment_send_to_name_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_send_to_name_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_fragment_send_to_pid_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that redirecting works for all send_to_pid type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_fragment_send_to_pid_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_send_to_pid_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_fragment_spawn_reply_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that redirecting works for all spawn_reply type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_fragment_spawn_reply_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_spawn_reply_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_fragment_spawn_request_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that redirecting works for all spawn_request type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_fragment_spawn_request_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_spawn_request_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_fragment_unlink_test() ->
    [
        {doc,
            "Tests sending fragment traffic to erldist_filter_nif and checks that redirecting works for all unlink type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_fragment_unlink_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    dpi_unlink_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_normal_exit_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that redirecting works for all exit type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_normal_exit_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_exit_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_normal_exit2_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that redirecting works for all exit2 type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_normal_exit2_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_exit2_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_normal_group_leader_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that redirecting works for all group_leader type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_normal_group_leader_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_group_leader_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_normal_link_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that redirecting works for all link type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_normal_link_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_link_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_normal_monitor_related_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that redirecting works for all monitor_related type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_normal_monitor_related_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_monitor_related_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_normal_send_to_alias_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that redirecting works for all send_to_alias type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_normal_send_to_alias_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_send_to_alias_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_normal_send_to_name_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that redirecting works for all send_to_name type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_normal_send_to_name_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_send_to_name_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_normal_send_to_pid_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that redirecting works for all send_to_pid type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_normal_send_to_pid_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_send_to_pid_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_normal_spawn_reply_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that redirecting works for all spawn_reply type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_normal_spawn_reply_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_spawn_reply_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_normal_spawn_request_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that redirecting works for all spawn_request type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_normal_spawn_request_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_spawn_request_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_normal_unlink_test() ->
    [
        {doc,
            "Tests sending normal traffic to erldist_filter_nif and checks that redirecting works for all unlink type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_normal_unlink_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => normal},
    dpi_unlink_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_pass_through_exit_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that redirecting works for all exit type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_pass_through_exit_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_exit_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_pass_through_exit2_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that redirecting works for all exit2 type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_pass_through_exit2_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_exit2_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_pass_through_group_leader_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that redirecting works for all group_leader type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_pass_through_group_leader_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_group_leader_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_pass_through_link_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that redirecting works for all link type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_pass_through_link_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_link_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_pass_through_monitor_related_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that redirecting works for all monitor_related type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_pass_through_monitor_related_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_monitor_related_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_pass_through_send_to_alias_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that redirecting works for all send_to_alias type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_pass_through_send_to_alias_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_send_to_alias_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_pass_through_send_to_name_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that redirecting works for all send_to_name type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_pass_through_send_to_name_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_send_to_name_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_pass_through_send_to_pid_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that redirecting works for all send_to_pid type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_pass_through_send_to_pid_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_send_to_pid_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_pass_through_spawn_reply_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that redirecting works for all spawn_reply type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_pass_through_spawn_reply_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_spawn_reply_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_pass_through_spawn_request_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that redirecting works for all spawn_request type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_pass_through_spawn_request_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_spawn_request_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

traffic_redirect_pass_through_unlink_test() ->
    [
        {doc,
            "Tests sending pass-through traffic to erldist_filter_nif and checks that redirecting works for all unlink type operations"},
        {timetrap, {seconds, 60}}
    ].

traffic_redirect_pass_through_unlink_test(Config) ->
    DFlags = maps:get('DFLAG_DIST_MANDATORY', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => pass_through},
    dpi_unlink_test(Config, DFlags, SendOptions, #{redirect_dist_operations => true}).

%%%-----------------------------------------------------------------------------
%%% Internal Deep Packet Inspection test functions
%%%-----------------------------------------------------------------------------

dpi_exit_test(_Config, DFlags, SendOptions, ConfigMap0) ->
    ConfigMap = ConfigMap0#{compact_fragments => true, deep_packet_inspection => true, logging => true},
    ok = config_set(ConfigMap),
    PacketSize = 4,
    [A, B, C] = [a, b, c],
    Sysname = 'nonode@nohost',
    Channel = erldist_filter_nif:channel_open(PacketSize, Sysname, 0, 0, DFlags),
    C0 = vedf_channel:new(PacketSize, DFlags, ConfigMap#{sysname => Sysname}),
    Token = vterm:expand(<<"token">>),
    LargeBin = binary:copy(<<"a">>, 255),
    NestedPayload = [{LargeBin, A, B, C}],
    ?assertEqual([], erldist_filter_nif:logger_list()),
    _OldCapacity = erldist_filter_nif:logger_set_capacity(4),
    Logger = erldist_filter_nif:logger_open(),
    LoggerSelectHandle = flush_logger(Logger),
    ControlA = dop_exit_noop(0, 0, 0),
    ControlB = dop_exit_noop(1, 0, 0),
    ControlC = dop_exit_tt_noop(2, 0, 0, Token),
    ControlD = dop_payload_exit_noop(3, 0, 0),
    ControlE = dop_payload_exit_tt_noop(4, 0, 0, Token),
    {ok, P0, C0} = vedf_channel:send_encode(C0, ControlA, SendOptions),
    {ok, A1, C1} = vedf_channel:recv(C0, P0),
    {ok, P1, C1} = vedf_channel:send_encode(C1, ControlB, SendOptions),
    {ok, A2, C2} = vedf_channel:recv(C1, P1),
    {ok, P2, C2} = vedf_channel:send_encode(C2, ControlC, SendOptions),
    {ok, A3, C3} = vedf_channel:recv(C2, P2),
    {ok, P3, C3} = vedf_channel:send_encode(C3, ControlD, vterm:expand(NestedPayload), SendOptions),
    {ok, A4, C4} = vedf_channel:recv(C3, P3),
    {ok, P4, C4} = vedf_channel:send_encode(C4, ControlE, vterm:expand(NestedPayload), SendOptions),
    {ok, A5, C5} = vedf_channel:recv(C4, P4),
    AllPackets = lists:concat([P0, P1, P2, P3, P4]),
    AllActions0 = lists:concat([A1, A2, A3, A4, A5]),
    {AllActions, [_Log_A, Log_B, Log_C, Log_D, Log_E]} = extract_action_logs(AllActions0, [], []),
    {log, _, {_, {Sysname, Atoms_B, Control_B, Payload_B}}} = Log_B,
    {log, _, {_, {Sysname, Atoms_C, Control_C, Payload_C}}} = Log_C,
    {log, _, {_, {Sysname, Atoms_D, Control_D, Payload_D}}} = Log_D,
    {log, _, {_, {Sysname, Atoms_E, Control_E, Payload_E}}} = Log_E,
    ?assertEqual(AllActions, erldist_filter_nif:channel_recv(Channel, AllPackets)),
    _ = C5,
    LoggerMessage = first_message_in_mailbox(1000),
    ?assertEqual({'$logger', Logger, select, LoggerSelectHandle}, LoggerMessage),
    LoggerRecv = erldist_filter_nif:logger_recv(Logger),
    ?assertMatch({4, 1, [_, _, _, _]}, LoggerRecv),
    {_, _, LoggerEvents0} = LoggerRecv,
    [
        LogEvent_B,
        LogEvent_C,
        LogEvent_D,
        LogEvent_E
    ] = simplify_logger_events(LoggerEvents0, 1),
    ?assertEqual({1, {Sysname, Atoms_B, Control_B, Payload_B}}, LogEvent_B),
    ?assertEqual({2, {Sysname, Atoms_C, Control_C, Payload_C}}, LogEvent_C),
    ?assertEqual({3, {Sysname, Atoms_D, Control_D, Payload_D}}, LogEvent_D),
    ?assertEqual({4, {Sysname, Atoms_E, Control_E, Payload_E}}, LogEvent_E),
    ok = erldist_filter_nif:channel_close(Channel),
    _ = flush_logger(Logger),
    ok = erldist_filter_nif:logger_close(Logger),
    ok = config_set_default(),
    ok.

dpi_exit2_test(_Config, DFlags, SendOptions, ConfigMap0) ->
    ConfigMap = ConfigMap0#{compact_fragments => true, deep_packet_inspection => true, logging => true},
    ok = config_set(ConfigMap),
    PacketSize = 4,
    [A, B, C] = [a, b, c],
    Sysname = 'nonode@nohost',
    Channel = erldist_filter_nif:channel_open(PacketSize, Sysname, 0, 0, DFlags),
    C0 = vedf_channel:new(PacketSize, DFlags, ConfigMap#{sysname => Sysname}),
    Token = vterm:expand(<<"token">>),
    LargeBin = binary:copy(<<"a">>, 255),
    NestedPayload = [{LargeBin, A, B, C}],
    ?assertEqual([], erldist_filter_nif:logger_list()),
    _OldCapacity = erldist_filter_nif:logger_set_capacity(4),
    Logger = erldist_filter_nif:logger_open(),
    LoggerSelectHandle = flush_logger(Logger),
    ControlA = dop_exit2_noop(0, 0, 0),
    ControlB = dop_exit2_noop(1, 0, 0),
    ControlC = dop_exit2_tt_noop(2, 0, 0, Token),
    ControlD = dop_payload_exit2_noop(3, 0, 0),
    ControlE = dop_payload_exit2_tt_noop(4, 0, 0, Token),
    {ok, P0, C0} = vedf_channel:send_encode(C0, ControlA, SendOptions),
    {ok, A1, C1} = vedf_channel:recv(C0, P0),
    {ok, P1, C1} = vedf_channel:send_encode(C1, ControlB, SendOptions),
    {ok, A2, C2} = vedf_channel:recv(C1, P1),
    {ok, P2, C2} = vedf_channel:send_encode(C2, ControlC, SendOptions),
    {ok, A3, C3} = vedf_channel:recv(C2, P2),
    {ok, P3, C3} = vedf_channel:send_encode(C3, ControlD, vterm:expand(NestedPayload), SendOptions),
    {ok, A4, C4} = vedf_channel:recv(C3, P3),
    {ok, P4, C4} = vedf_channel:send_encode(C4, ControlE, vterm:expand(NestedPayload), SendOptions),
    {ok, A5, C5} = vedf_channel:recv(C4, P4),
    AllPackets = lists:concat([P0, P1, P2, P3, P4]),
    AllActions0 = lists:concat([A1, A2, A3, A4, A5]),
    {AllActions, [_Log_A, Log_B, Log_C, Log_D, Log_E]} = extract_action_logs(AllActions0, [], []),
    {log, _, {_, {Sysname, Atoms_B, Control_B, Payload_B}}} = Log_B,
    {log, _, {_, {Sysname, Atoms_C, Control_C, Payload_C}}} = Log_C,
    {log, _, {_, {Sysname, Atoms_D, Control_D, Payload_D}}} = Log_D,
    {log, _, {_, {Sysname, Atoms_E, Control_E, Payload_E}}} = Log_E,
    ?assertEqual(AllActions, erldist_filter_nif:channel_recv(Channel, AllPackets)),
    _ = C5,
    LoggerMessage = first_message_in_mailbox(1000),
    ?assertEqual({'$logger', Logger, select, LoggerSelectHandle}, LoggerMessage),
    LoggerRecv = erldist_filter_nif:logger_recv(Logger),
    ?assertMatch({4, 1, [_, _, _, _]}, LoggerRecv),
    {_, _, LoggerEvents0} = LoggerRecv,
    [
        LogEvent_B,
        LogEvent_C,
        LogEvent_D,
        LogEvent_E
    ] = simplify_logger_events(LoggerEvents0, 1),
    ?assertEqual({1, {Sysname, Atoms_B, Control_B, Payload_B}}, LogEvent_B),
    ?assertEqual({2, {Sysname, Atoms_C, Control_C, Payload_C}}, LogEvent_C),
    ?assertEqual({3, {Sysname, Atoms_D, Control_D, Payload_D}}, LogEvent_D),
    ?assertEqual({4, {Sysname, Atoms_E, Control_E, Payload_E}}, LogEvent_E),
    ok = erldist_filter_nif:channel_close(Channel),
    _ = flush_logger(Logger),
    ok = erldist_filter_nif:logger_close(Logger),
    ok = config_set_default(),
    ok.

dpi_group_leader_test(_Config, DFlags, SendOptions, ConfigMap0) ->
    ConfigMap = ConfigMap0#{compact_fragments => true, deep_packet_inspection => true, logging => true},
    ok = config_set(ConfigMap),
    PacketSize = 4,
    [A, B, C] = [a, b, c],
    Sysname = 'nonode@nohost',
    Channel = erldist_filter_nif:channel_open(PacketSize, Sysname, 0, 0, DFlags),
    C0 = vedf_channel:new(PacketSize, DFlags, ConfigMap#{sysname => Sysname}),
    Token = vterm:expand(<<"token">>),
    LargeBin = binary:copy(<<"a">>, 255),
    NestedPayload = [{LargeBin, A, B, C}],
    ?assertEqual([], erldist_filter_nif:logger_list()),
    _ = Token,
    _ = NestedPayload,
    _OldCapacity = erldist_filter_nif:logger_set_capacity(1),
    Logger = erldist_filter_nif:logger_open(),
    LoggerSelectHandle = flush_logger(Logger),
    ControlA = dop_group_leader_noop(0, 0, 0),
    ControlB = dop_group_leader_noop(1, 0, 0),
    {ok, P0, C0} = vedf_channel:send_encode(C0, ControlA, SendOptions),
    {ok, A1, C1} = vedf_channel:recv(C0, P0),
    {ok, P1, C1} = vedf_channel:send_encode(C1, ControlB, SendOptions),
    {ok, A2, C2} = vedf_channel:recv(C1, P1),
    AllPackets = lists:concat([P0, P1]),
    AllActions0 = lists:concat([A1, A2]),
    {AllActions, [_Log_A, Log_B]} = extract_action_logs(AllActions0, [], []),
    {log, _, {_, {Sysname, Atoms_B, Control_B, Payload_B}}} = Log_B,
    ?assertEqual(AllActions, erldist_filter_nif:channel_recv(Channel, AllPackets)),
    _ = C2,
    LoggerMessage = first_message_in_mailbox(1000),
    ?assertEqual({'$logger', Logger, select, LoggerSelectHandle}, LoggerMessage),
    LoggerRecv = erldist_filter_nif:logger_recv(Logger),
    ?assertMatch({1, 1, [_]}, LoggerRecv),
    {_, _, LoggerEvents0} = LoggerRecv,
    [LogEvent_B] = simplify_logger_events(LoggerEvents0, 1),
    ?assertEqual({1, {Sysname, Atoms_B, Control_B, Payload_B}}, LogEvent_B),
    ok = erldist_filter_nif:channel_close(Channel),
    _ = flush_logger(Logger),
    ok = erldist_filter_nif:logger_close(Logger),
    ok = config_set_default(),
    ok.

dpi_link_test(_Config, DFlags, SendOptions, ConfigMap0) ->
    ConfigMap = ConfigMap0#{compact_fragments => true, deep_packet_inspection => true, logging => true},
    ok = config_set(ConfigMap),
    PacketSize = 4,
    [A, B, C] = [a, b, c],
    Sysname = 'nonode@nohost',
    Channel = erldist_filter_nif:channel_open(PacketSize, Sysname, 0, 0, DFlags),
    C0 = vedf_channel:new(PacketSize, DFlags, ConfigMap#{sysname => Sysname}),
    Token = vterm:expand(<<"token">>),
    LargeBin = binary:copy(<<"a">>, 255),
    NestedPayload = [{LargeBin, A, B, C}],
    ?assertEqual([], erldist_filter_nif:logger_list()),
    _ = Token,
    _ = NestedPayload,
    _OldCapacity = erldist_filter_nif:logger_set_capacity(1),
    Logger = erldist_filter_nif:logger_open(),
    LoggerSelectHandle = flush_logger(Logger),
    ControlA = dop_link_noop(0, 0, 0),
    ControlB = dop_link_noop(1, 0, 0),
    {ok, P0, C0} = vedf_channel:send_encode(C0, ControlA, SendOptions),
    {ok, A1, C1} = vedf_channel:recv(C0, P0),
    {ok, P1, C1} = vedf_channel:send_encode(C1, ControlB, SendOptions),
    {ok, A2, C2} = vedf_channel:recv(C1, P1),
    AllPackets = lists:concat([P0, P1]),
    AllActions0 = lists:concat([A1, A2]),
    {AllActions, [_Log_A, Log_B]} = extract_action_logs(AllActions0, [], []),
    {log, _, {_, {Sysname, Atoms_B, Control_B, Payload_B}}} = Log_B,
    ?assertEqual(AllActions, erldist_filter_nif:channel_recv(Channel, AllPackets)),
    _ = C2,
    LoggerMessage = first_message_in_mailbox(1000),
    ?assertEqual({'$logger', Logger, select, LoggerSelectHandle}, LoggerMessage),
    LoggerRecv = erldist_filter_nif:logger_recv(Logger),
    ?assertMatch({1, 1, [_]}, LoggerRecv),
    {_, _, LoggerEvents0} = LoggerRecv,
    [LogEvent_B] = simplify_logger_events(LoggerEvents0, 1),
    ?assertEqual({1, {Sysname, Atoms_B, Control_B, Payload_B}}, LogEvent_B),
    ok = erldist_filter_nif:channel_close(Channel),
    _ = flush_logger(Logger),
    ok = erldist_filter_nif:logger_close(Logger),
    ok = config_set_default(),
    ok.

dpi_monitor_related_test(_Config, DFlags, SendOptions, ConfigMap0) ->
    ConfigMap = ConfigMap0#{compact_fragments => true, deep_packet_inspection => true, logging => true},
    ok = config_set(ConfigMap),
    PacketSize = 4,
    [A, B, C] = [a, b, c],
    Sysname = 'nonode@nohost',
    Channel = erldist_filter_nif:channel_open(PacketSize, Sysname, 0, 0, DFlags),
    C0 = vedf_channel:new(PacketSize, DFlags, ConfigMap#{sysname => Sysname}),
    Token = vterm:expand(<<"token">>),
    LargeBin = binary:copy(<<"a">>, 255),
    NestedPayload = [{LargeBin, A, B, C}],
    ?assertEqual([], erldist_filter_nif:logger_list()),
    _ = Token,
    _OldCapacity = erldist_filter_nif:logger_set_capacity(4),
    Logger = erldist_filter_nif:logger_open(),
    LoggerSelectHandle = flush_logger(Logger),
    ControlA = dop_monitor_p_noop(0, 0, 0),
    ControlB = dop_monitor_p_noop(1, 0, 0),
    ControlC = dop_demonitor_p_noop(2, 0, 0),
    ControlD = dop_monitor_p_exit_noop(3, 0, 0),
    ControlE = dop_payload_monitor_p_exit_noop(4, 0, 0),
    {ok, P0, C0} = vedf_channel:send_encode(C0, ControlA, SendOptions),
    {ok, A1, C1} = vedf_channel:recv(C0, P0),
    {ok, P1, C1} = vedf_channel:send_encode(C1, ControlB, SendOptions),
    {ok, A2, C2} = vedf_channel:recv(C1, P1),
    {ok, P2, C2} = vedf_channel:send_encode(C2, ControlC, SendOptions),
    {ok, A3, C3} = vedf_channel:recv(C2, P2),
    {ok, P3, C3} = vedf_channel:send_encode(C3, ControlD, SendOptions),
    {ok, A4, C4} = vedf_channel:recv(C3, P3),
    {ok, P4, C4} = vedf_channel:send_encode(C4, ControlE, vterm:expand(NestedPayload), SendOptions),
    {ok, A5, C5} = vedf_channel:recv(C4, P4),
    AllPackets = lists:concat([P0, P1, P2, P3, P4]),
    AllActions0 = lists:concat([A1, A2, A3, A4, A5]),
    {AllActions, []} = extract_action_logs(AllActions0, [], []),
    ?assertEqual(AllActions, erldist_filter_nif:channel_recv(Channel, AllPackets)),
    _ = C5,
    _ = LoggerSelectHandle,
    ok = erldist_filter_nif:channel_close(Channel),
    _ = flush_logger(Logger),
    ok = erldist_filter_nif:logger_close(Logger),
    ok = config_set_default(),
    ok.

dpi_send_to_alias_test(_Config, DFlags, SendOptions, ConfigMap0) ->
    ConfigMap = ConfigMap0#{compact_fragments => true, deep_packet_inspection => true, logging => true},
    ok = config_set(ConfigMap),
    PacketSize = 4,
    [A, B, C] = [a, b, c],
    Sysname = 'nonode@nohost',
    Channel = erldist_filter_nif:channel_open(PacketSize, Sysname, 0, 0, DFlags),
    C0 = vedf_channel:new(PacketSize, DFlags, ConfigMap#{sysname => Sysname}),
    Token = vterm:expand(<<"token">>),
    LargeBin = binary:copy(<<"a">>, 255),
    NestedPayload = [{LargeBin, A, B, C}],
    ?assertEqual([], erldist_filter_nif:logger_list()),
    _OldCapacity = erldist_filter_nif:logger_set_capacity(2),
    Logger = erldist_filter_nif:logger_open(),
    LoggerSelectHandle = flush_logger(Logger),
    PayloadSendSystemCall = {system, [], NestedPayload},
    PayloadSendGenCall = {'$gen_call', [], NestedPayload},
    ControlA = dop_alias_send_noop(0, 0, 0),
    ControlB = dop_alias_send_noop(1, 0, 0),
    ControlC = dop_alias_send_tt_noop(0, 0, 0, Token),
    ControlD = dop_alias_send_tt_noop(1, 0, 0, Token),
    ControlE = dop_alias_send_noop(2, 0, 0),
    {ok, P0, C0} = vedf_channel:send_encode(C0, ControlA, vterm:expand(PayloadSendSystemCall), SendOptions),
    {ok, A1, C1} = vedf_channel:recv(C0, P0),
    {ok, P1, C1} = vedf_channel:send_encode(C1, ControlB, vterm:expand(PayloadSendGenCall), SendOptions),
    {ok, A2, C2} = vedf_channel:recv(C1, P1),
    {ok, P2, C2} = vedf_channel:send_encode(C2, ControlC, vterm:expand(PayloadSendSystemCall), SendOptions),
    {ok, A3, C3} = vedf_channel:recv(C2, P2),
    {ok, P3, C3} = vedf_channel:send_encode(C3, ControlD, vterm:expand(PayloadSendGenCall), SendOptions),
    {ok, A4, C4} = vedf_channel:recv(C3, P3),
    {ok, P4, C4} = vedf_channel:send_encode(C4, ControlE, vterm:expand(PayloadSendSystemCall), SendOptions),
    {ok, A5, C5} = vedf_channel:recv(C4, P4),
    AllPackets = lists:concat([P0, P1, P2, P3, P4]),
    AllActions0 = lists:concat([A1, A2, A3, A4, A5]),
    {AllActions, [_Log_A, Log_C, Log_E]} = extract_action_logs(AllActions0, [], []),
    {log, _, {_, {Sysname, Atoms_C, Control_C, Payload_C}}} = Log_C,
    {log, _, {_, {Sysname, Atoms_E, Control_E, Payload_E}}} = Log_E,
    ?assertEqual(AllActions, erldist_filter_nif:channel_recv(Channel, AllPackets)),
    _ = C5,
    LoggerMessage = first_message_in_mailbox(1000),
    ?assertEqual({'$logger', Logger, select, LoggerSelectHandle}, LoggerMessage),
    LoggerRecv = erldist_filter_nif:logger_recv(Logger),
    ?assertMatch({2, 1, [_, _]}, LoggerRecv),
    {_, _, LoggerEvents0} = LoggerRecv,
    [
        LogEvent_C,
        LogEvent_E
    ] = simplify_logger_events(LoggerEvents0, 1),
    ?assertEqual({1, {Sysname, Atoms_C, Control_C, Payload_C}}, LogEvent_C),
    ?assertEqual({2, {Sysname, Atoms_E, Control_E, Payload_E}}, LogEvent_E),
    ok = erldist_filter_nif:channel_close(Channel),
    _ = flush_logger(Logger),
    ok = erldist_filter_nif:logger_close(Logger),
    ok = config_set_default(),
    ok.

dpi_send_to_name_test(_Config, DFlags, SendOptions, ConfigMap0) ->
    ConfigMap = ConfigMap0#{compact_fragments => true, deep_packet_inspection => true, logging => true},
    ok = config_set(ConfigMap),
    PacketSize = 4,
    [A, B, C] = [a, b, c],
    Sysname = 'nonode@nohost',
    Channel = erldist_filter_nif:channel_open(PacketSize, Sysname, 0, 0, DFlags),
    C0 = vedf_channel:new(PacketSize, DFlags, ConfigMap#{sysname => Sysname}),
    Token = vterm:expand(<<"token">>),
    LargeBin = binary:copy(<<"a">>, 255),
    NestedPayload = [{LargeBin, A, B, C}],
    ?assertEqual([], erldist_filter_nif:logger_list()),
    _OldCapacity = erldist_filter_nif:logger_set_capacity(5),
    Logger = erldist_filter_nif:logger_open(),
    LoggerSelectHandle = flush_logger(Logger),
    PayloadSendSystemCall = {system, [], NestedPayload},
    PayloadSendGenCall = {'$gen_call', [], NestedPayload},
    PayloadSendGenCallIsAuth = {'$gen_call', [], {is_auth, []}},
    PayloadSendFeaturesRequest = {[], features_request},
    PayloadSendFeaturesReply = {features_reply, [], []},
    AtomNetKernel = vterm_small_atom_utf8_ext:new(10, <<"net_kernel">>),
    AtomRex = vterm_small_atom_utf8_ext:new(3, <<"rex">>),
    ControlA = dop_reg_send_noop(0, 0, 0, AtomNetKernel),
    ControlB = dop_reg_send_noop(1, 0, 0, AtomNetKernel),
    ControlC = dop_reg_send_noop(2, 0, 0, AtomNetKernel),
    ControlD = dop_reg_send_tt_noop(3, 0, 0, AtomNetKernel, Token),
    ControlE = dop_reg_send_tt_noop(4, 0, 0, AtomNetKernel, Token),
    ControlF = dop_reg_send_noop(5, 0, 0, AtomRex),
    ControlG = dop_reg_send_noop(6, 0, 0, AtomRex),
    ControlH = dop_reg_send_tt_noop(7, 0, 0, AtomRex, Token),
    ControlI = dop_reg_send_tt_noop(8, 0, 0, AtomRex, Token),
    {ok, P0, C0} = vedf_channel:send_encode(C0, ControlA, vterm:expand(PayloadSendGenCall), SendOptions),
    {ok, A1, C1} = vedf_channel:recv(C0, P0),
    {ok, P1, C1} = vedf_channel:send_encode(C1, ControlB, vterm:expand(PayloadSendGenCall), SendOptions),
    {ok, A2, C2} = vedf_channel:recv(C1, P1),
    {ok, P2, C2} = vedf_channel:send_encode(C2, ControlC, vterm:expand(PayloadSendGenCallIsAuth), SendOptions),
    {ok, A3, C3} = vedf_channel:recv(C2, P2),
    {ok, P3, C3} = vedf_channel:send_encode(C3, ControlD, vterm:expand(PayloadSendGenCall), SendOptions),
    {ok, A4, C4} = vedf_channel:recv(C3, P3),
    {ok, P4, C4} = vedf_channel:send_encode(C4, ControlE, vterm:expand(PayloadSendSystemCall), SendOptions),
    {ok, A5, C5} = vedf_channel:recv(C4, P4),
    {ok, P5, C5} = vedf_channel:send_encode(C5, ControlF, vterm:expand(PayloadSendFeaturesRequest), SendOptions),
    {ok, A6, C6} = vedf_channel:recv(C5, P5),
    {ok, P6, C6} = vedf_channel:send_encode(C6, ControlG, vterm:expand(PayloadSendFeaturesReply), SendOptions),
    {ok, A7, C7} = vedf_channel:recv(C6, P6),
    {ok, P7, C7} = vedf_channel:send_encode(C7, ControlH, vterm:expand(PayloadSendGenCall), SendOptions),
    {ok, A8, C8} = vedf_channel:recv(C7, P7),
    {ok, P8, C8} = vedf_channel:send_encode(C8, ControlI, vterm:expand(PayloadSendSystemCall), SendOptions),
    {ok, A9, C9} = vedf_channel:recv(C8, P8),
    AllPackets = lists:concat([P0, P1, P2, P3, P4, P5, P6, P7, P8]),
    AllActions0 = lists:concat([A1, A2, A3, A4, A5, A6, A7, A8, A9]),
    {AllActions, [_Log_A, Log_B, Log_D, Log_E, Log_H, Log_I]} = extract_action_logs(AllActions0, [], []),
    {log, _, {_, {Sysname, Atoms_B, Control_B, Payload_B}}} = Log_B,
    {log, _, {_, {Sysname, Atoms_D, Control_D, Payload_D}}} = Log_D,
    {log, _, {_, {Sysname, Atoms_E, Control_E, Payload_E}}} = Log_E,
    {log, _, {_, {Sysname, Atoms_H, Control_H, Payload_H}}} = Log_H,
    {log, _, {_, {Sysname, Atoms_I, Control_I, Payload_I}}} = Log_I,
    ?assertEqual(AllActions, erldist_filter_nif:channel_recv(Channel, AllPackets)),
    _ = C9,
    LoggerMessage = first_message_in_mailbox(1000),
    ?assertEqual({'$logger', Logger, select, LoggerSelectHandle}, LoggerMessage),
    LoggerRecv = erldist_filter_nif:logger_recv(Logger),
    ?assertMatch({5, 1, [_, _, _, _, _]}, LoggerRecv),
    {_, _, LoggerEvents0} = LoggerRecv,
    [
        LogEvent_B,
        LogEvent_D,
        LogEvent_E,
        LogEvent_H,
        LogEvent_I
    ] = simplify_logger_events(LoggerEvents0, 1),
    ?assertEqual({1, {Sysname, Atoms_B, Control_B, Payload_B}}, LogEvent_B),
    ?assertEqual({2, {Sysname, Atoms_D, Control_D, Payload_D}}, LogEvent_D),
    ?assertEqual({3, {Sysname, Atoms_E, Control_E, Payload_E}}, LogEvent_E),
    ?assertEqual({4, {Sysname, Atoms_H, Control_H, Payload_H}}, LogEvent_H),
    ?assertEqual({5, {Sysname, Atoms_I, Control_I, Payload_I}}, LogEvent_I),
    ok = erldist_filter_nif:channel_close(Channel),
    _ = flush_logger(Logger),
    ok = erldist_filter_nif:logger_close(Logger),
    ok = config_set_default(),
    ok.

dpi_send_to_pid_test(_Config, DFlags, SendOptions, ConfigMap0) ->
    ConfigMap = ConfigMap0#{compact_fragments => true, deep_packet_inspection => true, logging => true},
    ok = config_set(ConfigMap),
    PacketSize = 4,
    [A, B, C] = [a, b, c],
    Sysname = 'nonode@nohost',
    Channel = erldist_filter_nif:channel_open(PacketSize, Sysname, 0, 0, DFlags),
    C0 = vedf_channel:new(PacketSize, DFlags, ConfigMap#{sysname => Sysname}),
    Token = vterm:expand(<<"token">>),
    LargeBin = binary:copy(<<"a">>, 255),
    NestedPayload = [{LargeBin, A, B, C}],
    ?assertEqual([], erldist_filter_nif:logger_list()),
    _OldCapacity = erldist_filter_nif:logger_set_capacity(6),
    Logger = erldist_filter_nif:logger_open(),
    LoggerSelectHandle = flush_logger(Logger),
    PayloadSendSystemCall = {system, [], NestedPayload},
    PayloadSendExit = {'EXIT', [], []},
    PayloadSendGenCast = {'$gen_cast', {try_again_restart, []}},
    PayloadSendGenCall = {'$gen_call', [], NestedPayload},
    PayloadSendGenCallStartChild = {'$gen_call', [], {start_child, []}},
    PayloadSendIoRequest = {io_request, [], [], {put_chars, unicode, remote, rce, []}},
    PayloadSendIoReply = {io_reply, [], []},
    ControlA = dop_send_noop(0, 0, 0),
    ControlB = dop_send_noop(1, 0, 0),
    ControlC = dop_send_noop(2, 0, 0),
    ControlD = dop_send_tt_noop(3, 0, 0, Token),
    ControlE = dop_send_tt_noop(4, 0, 0, Token),
    ControlF = dop_send_sender_noop(5, 0, 0),
    ControlG = dop_send_sender_noop(6, 0, 0),
    ControlH = dop_send_sender_tt_noop(7, 0, 0, Token),
    ControlI = dop_send_sender_tt_noop(8, 0, 0, Token),
    ControlJ = dop_send_sender_noop(9, 0, 0),
    ControlK = dop_send_sender_tt_noop(10, 0, 0, Token),
    {ok, P0, C0} = vedf_channel:send_encode(C0, ControlA, vterm:expand(PayloadSendSystemCall), SendOptions),
    {ok, A1, C1} = vedf_channel:recv(C0, P0),
    {ok, P1, C1} = vedf_channel:send_encode(C1, ControlB, vterm:expand(PayloadSendSystemCall), SendOptions),
    {ok, A2, C2} = vedf_channel:recv(C1, P1),
    {ok, P2, C2} = vedf_channel:send_encode(C2, ControlC, vterm:expand(PayloadSendGenCall), SendOptions),
    {ok, A3, C3} = vedf_channel:recv(C2, P2),
    {ok, P3, C3} = vedf_channel:send_encode(C3, ControlD, vterm:expand(PayloadSendExit), SendOptions),
    {ok, A4, C4} = vedf_channel:recv(C3, P3),
    {ok, P4, C4} = vedf_channel:send_encode(C4, ControlE, vterm:expand(PayloadSendGenCall), SendOptions),
    {ok, A5, C5} = vedf_channel:recv(C4, P4),
    {ok, P5, C5} = vedf_channel:send_encode(C5, ControlF, vterm:expand(PayloadSendGenCast), SendOptions),
    {ok, A6, C6} = vedf_channel:recv(C5, P5),
    {ok, P6, C6} = vedf_channel:send_encode(C6, ControlG, vterm:expand(PayloadSendGenCall), SendOptions),
    {ok, A7, C7} = vedf_channel:recv(C6, P6),
    {ok, P7, C7} = vedf_channel:send_encode(C7, ControlH, vterm:expand(PayloadSendGenCallStartChild), SendOptions),
    {ok, A8, C8} = vedf_channel:recv(C7, P7),
    {ok, P8, C8} = vedf_channel:send_encode(C8, ControlI, vterm:expand(PayloadSendGenCall), SendOptions),
    {ok, A9, C9} = vedf_channel:recv(C8, P8),
    {ok, P9, C9} = vedf_channel:send_encode(C9, ControlJ, vterm:expand(PayloadSendIoRequest), SendOptions),
    {ok, AA, CA} = vedf_channel:recv(C9, P9),
    {ok, PA, CA} = vedf_channel:send_encode(CA, ControlK, vterm:expand(PayloadSendIoReply), SendOptions),
    {ok, AB, CB} = vedf_channel:recv(CA, PA),
    AllPackets = lists:concat([P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, PA]),
    AllActions0 = lists:concat([A1, A2, A3, A4, A5, A6, A7, A8, A9, AA, AB]),
    {AllActions, [_Log_A, Log_B, Log_D, Log_F, Log_H, Log_J, Log_K]} = extract_action_logs(AllActions0, [], []),
    {log, _, {_, {Sysname, Atoms_B, Control_B, Payload_B}}} = Log_B,
    {log, _, {_, {Sysname, Atoms_D, Control_D, Payload_D}}} = Log_D,
    {log, _, {_, {Sysname, Atoms_F, Control_F, Payload_F}}} = Log_F,
    {log, _, {_, {Sysname, Atoms_H, Control_H, Payload_H}}} = Log_H,
    {log, _, {_, {Sysname, Atoms_J, Control_J, Payload_J}}} = Log_J,
    {log, _, {_, {Sysname, Atoms_K, Control_K, Payload_K}}} = Log_K,
    ?assertEqual(AllActions, erldist_filter_nif:channel_recv(Channel, AllPackets)),
    _ = CB,
    LoggerMessage = first_message_in_mailbox(1000),
    ?assertEqual({'$logger', Logger, select, LoggerSelectHandle}, LoggerMessage),
    LoggerRecv = erldist_filter_nif:logger_recv(Logger),
    ?assertMatch({6, 1, [_, _, _, _, _, _]}, LoggerRecv),
    {_, _, LoggerEvents0} = LoggerRecv,
    [
        LogEvent_B,
        LogEvent_D,
        LogEvent_F,
        LogEvent_H,
        LogEvent_J,
        LogEvent_K
    ] = simplify_logger_events(LoggerEvents0, 1),
    ?assertEqual({1, {Sysname, Atoms_B, Control_B, Payload_B}}, LogEvent_B),
    ?assertEqual({2, {Sysname, Atoms_D, Control_D, Payload_D}}, LogEvent_D),
    ?assertEqual({3, {Sysname, Atoms_F, Control_F, Payload_F}}, LogEvent_F),
    ?assertEqual({4, {Sysname, Atoms_H, Control_H, Payload_H}}, LogEvent_H),
    ?assertEqual({5, {Sysname, Atoms_J, Control_J, Payload_J}}, LogEvent_J),
    ?assertEqual({6, {Sysname, Atoms_K, Control_K, Payload_K}}, LogEvent_K),
    ok = erldist_filter_nif:channel_close(Channel),
    _ = flush_logger(Logger),
    ok = erldist_filter_nif:logger_close(Logger),
    ok = config_set_default(),
    ok.

dpi_spawn_reply_test(_Config, DFlags, SendOptions, ConfigMap0) ->
    ConfigMap = ConfigMap0#{compact_fragments => true, deep_packet_inspection => true, logging => true},
    ok = config_set(ConfigMap),
    PacketSize = 4,
    [A, B, C] = [a, b, c],
    Sysname = 'nonode@nohost',
    Channel = erldist_filter_nif:channel_open(PacketSize, Sysname, 0, 0, DFlags),
    C0 = vedf_channel:new(PacketSize, DFlags, ConfigMap#{sysname => Sysname}),
    Token = vterm:expand(<<"token">>),
    LargeBin = binary:copy(<<"a">>, 255),
    NestedPayload = [{LargeBin, A, B, C}],
    ?assertEqual([], erldist_filter_nif:logger_list()),
    _ = NestedPayload,
    _OldCapacity = erldist_filter_nif:logger_set_capacity(4),
    Logger = erldist_filter_nif:logger_open(),
    LoggerSelectHandle = flush_logger(Logger),
    ControlA = dop_spawn_reply_noop(0, 0, 0),
    ControlB = dop_spawn_reply_tt_noop(1, 0, 0, Token),
    {ok, P0, C0} = vedf_channel:send_encode(C0, ControlA, SendOptions),
    {ok, A1, C1} = vedf_channel:recv(C0, P0),
    {ok, P1, C1} = vedf_channel:send_encode(C1, ControlB, SendOptions),
    {ok, A2, C2} = vedf_channel:recv(C1, P1),
    AllPackets = lists:concat([P0, P1]),
    AllActions0 = lists:concat([A1, A2]),
    {AllActions, []} = extract_action_logs(AllActions0, [], []),
    ?assertEqual(AllActions, erldist_filter_nif:channel_recv(Channel, AllPackets)),
    _ = C2,
    _ = LoggerSelectHandle,
    ok = erldist_filter_nif:channel_close(Channel),
    _ = flush_logger(Logger),
    ok = erldist_filter_nif:logger_close(Logger),
    ok = config_set_default(),
    ok.

dpi_spawn_request_test(_Config, DFlags, SendOptions, ConfigMap0) ->
    ConfigMap = ConfigMap0#{compact_fragments => true, deep_packet_inspection => true, logging => true},
    ok = config_set(ConfigMap),
    PacketSize = 4,
    [A, B, C] = [a, b, c],
    Sysname = 'nonode@nohost',
    Channel = erldist_filter_nif:channel_open(PacketSize, Sysname, 0, 0, DFlags),
    C0 = vedf_channel:new(PacketSize, DFlags, ConfigMap#{sysname => Sysname}),
    Token = vterm:expand(<<"token">>),
    LargeBin = binary:copy(<<"a">>, 255),
    NestedPayload = [{LargeBin, A, B, C}],
    ?assertEqual([], erldist_filter_nif:logger_list()),
    _OldCapacity = erldist_filter_nif:logger_set_capacity(2),
    Logger = erldist_filter_nif:logger_open(),
    LoggerSelectHandle = flush_logger(Logger),
    ControlA = dop_spawn_request_noop(0, 0, 0),
    ControlB = dop_spawn_request_noop(1, 0, 0),
    ControlC = dop_spawn_request_tt_noop(1, 0, 0, Token),
    {ok, P0, C0} = vedf_channel:send_encode(C0, ControlA, vterm:expand(NestedPayload), SendOptions),
    {ok, A1, C1} = vedf_channel:recv(C0, P0),
    {ok, P1, C1} = vedf_channel:send_encode(C1, ControlB, vterm:expand(NestedPayload), SendOptions),
    {ok, A2, C2} = vedf_channel:recv(C1, P1),
    {ok, P2, C2} = vedf_channel:send_encode(C2, ControlC, vterm:expand(NestedPayload), SendOptions),
    {ok, A3, C3} = vedf_channel:recv(C2, P2),
    AllPackets = lists:concat([P0, P1, P2]),
    AllActions0 = lists:concat([A1, A2, A3]),
    {AllActions, [_Log_A, Log_B, Log_C]} = extract_action_logs(AllActions0, [], []),
    {log, _, {_, {Sysname, Atoms_B, Control_B, Payload_B}}} = Log_B,
    {log, _, {_, {Sysname, Atoms_C, Control_C, Payload_C}}} = Log_C,
    ?assertEqual(AllActions, erldist_filter_nif:channel_recv(Channel, AllPackets)),
    _ = C3,
    LoggerMessage = first_message_in_mailbox(1000),
    ?assertEqual({'$logger', Logger, select, LoggerSelectHandle}, LoggerMessage),
    LoggerRecv = erldist_filter_nif:logger_recv(Logger),
    ?assertMatch({2, 1, [_, _]}, LoggerRecv),
    {_, _, LoggerEvents0} = LoggerRecv,
    [
        LogEvent_B,
        LogEvent_C
    ] = simplify_logger_events(LoggerEvents0, 1),
    ?assertEqual({1, {Sysname, Atoms_B, Control_B, Payload_B}}, LogEvent_B),
    ?assertEqual({2, {Sysname, Atoms_C, Control_C, Payload_C}}, LogEvent_C),
    ok = erldist_filter_nif:channel_close(Channel),
    _ = flush_logger(Logger),
    ok = erldist_filter_nif:logger_close(Logger),
    ok = config_set_default(),
    ok.

dpi_unlink_test(_Config, DFlags, SendOptions, ConfigMap0) ->
    ConfigMap = ConfigMap0#{compact_fragments => true, deep_packet_inspection => true, logging => true},
    ok = config_set(ConfigMap),
    PacketSize = 4,
    [A, B, C] = [a, b, c],
    Sysname = 'nonode@nohost',
    Channel = erldist_filter_nif:channel_open(PacketSize, Sysname, 0, 0, DFlags),
    C0 = vedf_channel:new(PacketSize, DFlags, ConfigMap#{sysname => Sysname}),
    Token = vterm:expand(<<"token">>),
    LargeBin = binary:copy(<<"a">>, 255),
    NestedPayload = [{LargeBin, A, B, C}],
    ?assertEqual([], erldist_filter_nif:logger_list()),
    _ = Token,
    _ = NestedPayload,
    _OldCapacity = erldist_filter_nif:logger_set_capacity(4),
    Logger = erldist_filter_nif:logger_open(),
    LoggerSelectHandle = flush_logger(Logger),
    ControlA = dop_unlink_noop(0, 0, 0),
    ControlB = dop_unlink_id_noop(1, 0, 0),
    ControlC = dop_unlink_id_ack_noop(2, 0, 0),
    {ok, P0, C0} = vedf_channel:send_encode(C0, ControlA, SendOptions),
    {ok, A1, C1} = vedf_channel:recv(C0, P0),
    {ok, P1, C1} = vedf_channel:send_encode(C1, ControlB, SendOptions),
    {ok, A2, C2} = vedf_channel:recv(C1, P1),
    {ok, P2, C2} = vedf_channel:send_encode(C2, ControlC, SendOptions),
    {ok, A3, C3} = vedf_channel:recv(C2, P2),
    AllPackets = lists:concat([P0, P1, P2]),
    AllActions0 = lists:concat([A1, A2, A3]),
    {AllActions, []} = extract_action_logs(AllActions0, [], []),
    ?assertEqual(AllActions, erldist_filter_nif:channel_recv(Channel, AllPackets)),
    _ = C3,
    _ = LoggerSelectHandle,
    ok = erldist_filter_nif:channel_close(Channel),
    _ = flush_logger(Logger),
    ok = erldist_filter_nif:logger_close(Logger),
    ok = config_set_default(),
    ok.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
config_set_default() ->
    config_set(#{}).

%% @private
config_set(Config) ->
    Current = erldist_filter_nif:config_get(),
    ConfigKeys = maps:keys(Current),
    Defaults = maps:fold(
        fun(Key, Value, Acc) ->
            case Value of
                _ when is_boolean(Value) ->
                    Acc#{Key => false};
                _ when is_integer(Value) ->
                    Acc#{Key => 1}
            end
        end,
        maps:new(),
        Current
    ),
    config_set(maps:to_list(Config), ConfigKeys, Defaults).

%% @private
config_set([{Key, Val} | Rest], ConfigKeys, Defaults) when is_boolean(Val) ->
    ok = erldist_filter_nif:config_set(Key, Val),
    config_set(Rest, ConfigKeys -- [Key], Defaults);
config_set([], [Key | ConfigKeys], Defaults) ->
    ok = erldist_filter_nif:config_set(Key, maps:get(Key, Defaults)),
    config_set([], ConfigKeys, Defaults);
config_set([], [], _Defaults) ->
    ok.

%% @private
extract_action_logs([Action = {log, _, _} | Rest], Actions, Logs) ->
    extract_action_logs(Rest, Actions, [Action | Logs]);
extract_action_logs([Action | Rest], Actions, Logs) ->
    extract_action_logs(Rest, [Action | Actions], Logs);
extract_action_logs([], Actions, Logs) ->
    {lists:reverse(Actions), lists:reverse(Logs)}.

%% @private
first_message_in_mailbox(Timeout) ->
    receive
        Msg ->
            Msg
    after Timeout ->
        erlang:error(timeout_waiting_for_first_message_in_mailbox)
    end.

%% @private
flush_logger(Logger) ->
    case erldist_filter_nif:logger_recv(Logger) of
        {_QueueSize, _QueueDrop, _Events} ->
            flush_logger(Logger);
        {select, LoggerSelectHandle} ->
            LoggerSelectHandle
    end.

%% @private
dop_alias_send_noop(Id, Serial, Creation) ->
    EmptyAtom = vterm_atom_utf8_ext:new(0, <<>>),
    FromPid = vterm_new_pid_ext:new(EmptyAtom, Id, Serial, Creation),
    Alias = vterm_newer_reference_ext:new(1, EmptyAtom, Creation, [Id]),
    vdist_dop_alias_send:new(FromPid, Alias).

%% @private
dop_alias_send_tt_noop(Id, Serial, Creation, Token) ->
    EmptyAtom = vterm_atom_utf8_ext:new(0, <<>>),
    FromPid = vterm_new_pid_ext:new(EmptyAtom, Id, Serial, Creation),
    Alias = vterm_newer_reference_ext:new(1, EmptyAtom, Creation, [Id]),
    vdist_dop_alias_send_tt:new(FromPid, Alias, Token).

%% @private
dop_exit_noop(Id, Serial, Creation) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    Reason = vterm_atom_utf8_ext:new(6, <<"killed">>),
    FromPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    ToPid = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    vdist_dop_exit:new(FromPid, ToPid, Reason).

%% @private
dop_exit_tt_noop(Id, Serial, Creation, Token) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    Reason = vterm_atom_utf8_ext:new(6, <<"killed">>),
    FromPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    ToPid = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    vdist_dop_exit_tt:new(FromPid, ToPid, Token, Reason).

%% @private
dop_payload_exit_noop(Id, Serial, Creation) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    FromPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    ToPid = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    vdist_dop_payload_exit:new(FromPid, ToPid).

%% @private
dop_payload_exit_tt_noop(Id, Serial, Creation, Token) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    FromPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    ToPid = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    vdist_dop_payload_exit_tt:new(FromPid, ToPid, Token).

%% @private
dop_exit2_noop(Id, Serial, Creation) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    Reason = vterm_atom_utf8_ext:new(4, <<"kill">>),
    FromPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    ToPid = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    vdist_dop_exit2:new(FromPid, ToPid, Reason).

%% @private
dop_exit2_tt_noop(Id, Serial, Creation, Token) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    Reason = vterm_atom_utf8_ext:new(4, <<"kill">>),
    FromPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    ToPid = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    vdist_dop_exit2_tt:new(FromPid, ToPid, Token, Reason).

%% @private
dop_payload_exit2_noop(Id, Serial, Creation) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    FromPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    ToPid = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    vdist_dop_payload_exit2:new(FromPid, ToPid).

%% @private
dop_payload_exit2_tt_noop(Id, Serial, Creation, Token) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    FromPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    ToPid = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    vdist_dop_payload_exit2_tt:new(FromPid, ToPid, Token).

%% @private
dop_group_leader_noop(Id, Serial, Creation) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    FromPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    ToPid = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    vdist_dop_group_leader:new(FromPid, ToPid).

%% @private
dop_link_noop(Id, Serial, Creation) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    FromPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    ToPid = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    vdist_dop_link:new(FromPid, ToPid).

%% @private
dop_monitor_p_noop(Id, Serial, Creation) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    FromPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    ToProc = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    Ref = vterm_newer_reference_ext:new(1, NodeA, Creation, [Id]),
    vdist_dop_monitor_p:new(FromPid, ToProc, Ref).

%% @private
dop_demonitor_p_noop(Id, Serial, Creation) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    FromPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    ToProc = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    Ref = vterm_newer_reference_ext:new(1, NodeA, Creation, [Id]),
    vdist_dop_demonitor_p:new(FromPid, ToProc, Ref).

%% @private
dop_monitor_p_exit_noop(Id, Serial, Creation) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    FromProc = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    ToPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    Ref = vterm_newer_reference_ext:new(1, NodeA, Creation, [Id]),
    Reason = vterm_atom_utf8_ext:new(6, <<"killed">>),
    vdist_dop_monitor_p_exit:new(FromProc, ToPid, Ref, Reason).

%% @private
dop_payload_monitor_p_exit_noop(Id, Serial, Creation) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    FromProc = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    ToPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    Ref = vterm_newer_reference_ext:new(1, NodeA, Creation, [Id]),
    vdist_dop_payload_monitor_p_exit:new(FromProc, ToPid, Ref).

%% @private
dop_reg_send_noop(Id, Serial, Creation, ToName) ->
    Unused = vterm_atom_utf8_ext:new(0, <<>>),
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    FromPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    vdist_dop_reg_send:new(FromPid, Unused, ToName).

%% @private
dop_reg_send_tt_noop(Id, Serial, Creation, ToName, Token) ->
    Unused = vterm_atom_utf8_ext:new(0, <<>>),
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    FromPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    vdist_dop_reg_send_tt:new(FromPid, Unused, ToName, Token).

%% @private
dop_send_noop(Id, Serial, Creation) ->
    Unused = vterm_atom_utf8_ext:new(0, <<>>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    ToPid = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    vdist_dop_send:new(Unused, ToPid).

%% @private
dop_send_tt_noop(Id, Serial, Creation, Token) ->
    Unused = vterm_atom_utf8_ext:new(0, <<>>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    ToPid = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    vdist_dop_send_tt:new(Unused, ToPid, Token).

%% @private
dop_send_sender_noop(Id, Serial, Creation) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    FromPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    ToPid = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    vdist_dop_send_sender:new(FromPid, ToPid).

%% @private
dop_send_sender_tt_noop(Id, Serial, Creation, Token) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    FromPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    ToPid = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    vdist_dop_send_sender_tt:new(FromPid, ToPid, Token).

%% @private
dop_spawn_reply_noop(Id, Serial, Creation) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    ReqId = vterm_newer_reference_ext:new(1, NodeA, Creation, [Id]),
    To = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    Flags = vterm_small_integer_ext:new(2),
    Result = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    vdist_dop_spawn_reply:new(ReqId, To, Flags, Result).

%% @private
dop_spawn_reply_tt_noop(Id, Serial, Creation, Token) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    ReqId = vterm_newer_reference_ext:new(1, NodeA, Creation, [Id]),
    To = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    Flags = vterm_small_integer_ext:new(2),
    Result = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    vdist_dop_spawn_reply_tt:new(ReqId, To, Flags, Result, Token).

%% @private
dop_spawn_request_noop(Id, Serial, Creation) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    UndefinedAtom = vterm_atom_utf8_ext:new(9, <<"undefined">>),
    ReqId = vterm_newer_reference_ext:new(1, NodeA, Creation, [Id]),
    FromPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    GroupLeaderPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    Module = UndefinedAtom,
    Function = UndefinedAtom,
    Arity = vterm_small_integer_ext:new(1),
    OptList = [],
    vdist_dop_spawn_request:new(ReqId, FromPid, GroupLeaderPid, Module, Function, Arity, OptList).

%% @private
dop_spawn_request_tt_noop(Id, Serial, Creation, Token) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    UndefinedAtom = vterm_atom_utf8_ext:new(9, <<"undefined">>),
    ReqId = vterm_newer_reference_ext:new(1, NodeA, Creation, [Id]),
    FromPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    GroupLeaderPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    Module = UndefinedAtom,
    Function = UndefinedAtom,
    Arity = vterm_small_integer_ext:new(1),
    OptList = [],
    vdist_dop_spawn_request_tt:new(ReqId, FromPid, GroupLeaderPid, Module, Function, Arity, OptList, Token).

%% @private
dop_unlink_noop(Id, Serial, Creation) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    FromPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    ToPid = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    vdist_dop_unlink:new(FromPid, ToPid).

%% @private
dop_unlink_id_noop(Id, Serial, Creation) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    FromPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    ToPid = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    IdInteger = vterm_integer_ext:new(Id + 1024),
    vdist_dop_unlink_id:new(IdInteger, FromPid, ToPid).

%% @private
dop_unlink_id_ack_noop(Id, Serial, Creation) ->
    NodeA = vterm_atom_utf8_ext:new(1, <<"a">>),
    NodeB = vterm_atom_utf8_ext:new(1, <<"b">>),
    FromPid = vterm_new_pid_ext:new(NodeA, Id, Serial, Creation),
    ToPid = vterm_new_pid_ext:new(NodeB, Id, Serial, Creation),
    IdInteger = vterm_integer_ext:new(Id + 1024),
    vdist_dop_unlink_id_ack:new(IdInteger, FromPid, ToPid).

%% @private
spawn_request_noop(Id, Serial, Creation) ->
    EmptyAtom = vterm_atom_utf8_ext:new(0, <<>>),
    UndefinedAtom = vterm_atom_utf8_ext:new(9, <<"undefined">>),
    ReqId = vterm_newer_reference_ext:new(1, EmptyAtom, Creation, [Id]),
    FromPid = vterm_new_pid_ext:new(EmptyAtom, Id, Serial, Creation),
    GroupLeaderPid = vterm_new_pid_ext:new(EmptyAtom, Id, Serial, Creation),
    Module = UndefinedAtom,
    Function = UndefinedAtom,
    Arity = vterm_small_integer_ext:new(1),
    OptList = [],
    vdist_dop_spawn_request:new(ReqId, FromPid, GroupLeaderPid, Module, Function, Arity, OptList).

% %% @private
% simplify_logger_events(Events) ->
%     simplify_logger_events(Events, 0).

%% @private
simplify_logger_events(Events, SimpleTime) ->
    simplify_logger_events(Events, SimpleTime, []).

%% @private
simplify_logger_events([{_LoggerTime, LoggerEvent} | Events], SimpleTime, Acc) ->
    Event = {SimpleTime, LoggerEvent},
    simplify_logger_events(Events, SimpleTime + 1, [Event | Acc]);
simplify_logger_events([], _SimpleTime, Acc) ->
    lists:reverse(Acc).

%% @private
simplify_logger_time(Events) ->
    simplify_logger_time(Events, 0).

%% @private
simplify_logger_time(Events, SimpleTime) ->
    simplify_logger_time(Events, SimpleTime, []).

%% @private
simplify_logger_time([{log, FragmentCount, {_LoggerTime, LoggerEvent}} | Events], SimpleTime, Acc) ->
    Event = {log, FragmentCount, {SimpleTime, LoggerEvent}},
    simplify_logger_time(Events, SimpleTime + 1, [Event | Acc]);
simplify_logger_time([Event | Events], SimpleTime, Acc) ->
    simplify_logger_time(Events, SimpleTime, [Event | Acc]);
simplify_logger_time([], _SimpleTime, Acc) ->
    lists:reverse(Acc).
