%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(erldist_filter_edge_cases_SUITE).
-moduledoc """
## Edge Case Test Suite for Erlang Distribution Filter

This Common Test suite contains regression tests for edge cases and bugs discovered in the
`erldist_filter` NIF implementation.

### Test Cases

- **decode_length_trap_bug**: Tests a specific bug (T142904516) where decoding control terms
  that are exactly the size of the trapping slice caused issues. The test uses a
  counterexample generated from property-based testing to reproduce and verify the fix.

The test suite uses generated test vectors stored in config files to replay specific
sequences of channel operations that previously triggered bugs.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2023-01-17", modified => "2025-08-21"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("stdlib/include/assert.hrl").

-behaviour(ct_suite).

%% ct_suite callbacks
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
    counterexample8444249432900889_decode_length_trap_bug/0,
    counterexample8444249432900889_decode_length_trap_bug/1,
    packet_size_0_tick/0,
    packet_size_0_tick/1,
    packet_size_1_tick/0,
    packet_size_1_tick/1,
    packet_size_2_tick/0,
    packet_size_2_tick/1,
    packet_size_4_tick/0,
    packet_size_4_tick/1,
    packet_size_8_tick/0,
    packet_size_8_tick/1
]).

%% Macros
-define(NIF_SPBT_SHIM, erldist_filter_nif_spbt_shim).

%%%=============================================================================
%%% ct_suite callbacks
%%%=============================================================================

-spec all() -> erldist_filter_test:all().
all() ->
    [
        {group, edge_cases}
    ].

-spec groups() -> erldist_filter_test:groups().
groups() ->
    [
        {edge_cases, [parallel], [
            counterexample8444249432900889_decode_length_trap_bug,
            packet_size_0_tick,
            packet_size_1_tick,
            packet_size_2_tick,
            packet_size_4_tick,
            packet_size_8_tick
        ]}
    ].

-spec init_per_suite(Config :: ct_suite:ct_config()) -> erldist_filter_test:init_per_suite().
init_per_suite(Config) ->
    Config.

-spec end_per_suite(Config :: ct_suite:ct_config()) -> erldist_filter_test:end_per_suite().
end_per_suite(_Config) ->
    ok.

-spec init_per_group(GroupName :: ct_suite:ct_groupname(), Config :: ct_suite:ct_config()) ->
    erldist_filter_test:init_per_group().
init_per_group(_Group, Config) ->
    Config.

-spec end_per_group(GroupName :: ct_suite:ct_groupname(), Config :: ct_suite:ct_config()) ->
    erldist_filter_test:end_per_group().
end_per_group(_Group, _Config) ->
    ok.

%%%=============================================================================
%%% Test Cases
%%%=============================================================================

-spec counterexample8444249432900889_decode_length_trap_bug() -> erldist_filter_test:testcase_info().
counterexample8444249432900889_decode_length_trap_bug() ->
    [
        {doc, "Bug when decoding control term that is the exact size of trapping slice, see T142904516"},
        {timetrap, {seconds, 120}}
    ].

-spec counterexample8444249432900889_decode_length_trap_bug(Config :: ct_suite:ct_config()) ->
    erldist_filter_test:testcase().
counterexample8444249432900889_decode_length_trap_bug(Config) ->
    DataDir = test_server:lookup_config(data_dir, Config),
    TestVectorsFile = filename:join([DataDir, "counterexample8444249432900889_decode_length_trap_bug.config"]),
    {ok, [Commands]} = file:consult(TestVectorsFile),
    emulate_simple_commands(Commands).

-spec packet_size_0_tick() -> erldist_filter_test:testcase_info().
packet_size_0_tick() ->
    [
        {doc, "Bug when decoding tick message on fastpath with packet size 0"},
        {timetrap, {seconds, 30}}
    ].

-spec packet_size_0_tick(Config :: ct_suite:ct_config()) ->
    erldist_filter_test:testcase().
packet_size_0_tick(_Config) ->
    %% For packet size 0, the tick message is empty and is not counted as a packet.
    %% For slowpath, the tick message is counted as a packet only once.
    Tick = <<>>,
    PacketSize = 0,
    Sysname = 'packet-size-x@127.0.0.1',
    Creation = 1756475325,
    ConnectionId = 4804673,
    DistributionFlags = 193405616061,
    Channel = erldist_filter_nif:channel_open(PacketSize, Sysname, Creation, ConnectionId, DistributionFlags),
    ?assertMatch(
        #{rx := #{stats := #{packet_count := 0, emit_count := 0, dist_tick_count := 0, fastpath := 0, slowpath := 0}}},
        erldist_filter_nif:channel_inspect(Channel)
    ),
    ?assertMatch([], erldist_filter_nif:channel_recv(Channel, [Tick])),
    ?assertMatch(
        #{rx := #{stats := #{packet_count := 0, emit_count := 0, dist_tick_count := 0, fastpath := 0, slowpath := 0}}},
        erldist_filter_nif:channel_inspect(Channel)
    ),
    ?assertMatch([], erldist_filter_nif:channel_recv(Channel, [Tick, Tick])),
    ?assertMatch(
        #{rx := #{stats := #{packet_count := 1, emit_count := 0, dist_tick_count := 1, fastpath := 0, slowpath := 1}}},
        erldist_filter_nif:channel_inspect(Channel)
    ),
    ok = erldist_filter_nif:channel_close(Channel),
    ok.

-spec packet_size_1_tick() -> erldist_filter_test:testcase_info().
packet_size_1_tick() ->
    [
        {doc, "Bug when decoding tick message on fastpath with packet size 1"},
        {timetrap, {seconds, 30}}
    ].

-spec packet_size_1_tick(Config :: ct_suite:ct_config()) ->
    erldist_filter_test:testcase().
packet_size_1_tick(Config) ->
    packet_size_x_tick([{tick, <<0>>}, {packet_size, 1} | Config]).

-spec packet_size_2_tick() -> erldist_filter_test:testcase_info().
packet_size_2_tick() ->
    [
        {doc, "Bug when decoding tick message on fastpath with packet size 2"},
        {timetrap, {seconds, 30}}
    ].

-spec packet_size_2_tick(Config :: ct_suite:ct_config()) ->
    erldist_filter_test:testcase().
packet_size_2_tick(Config) ->
    packet_size_x_tick([{tick, <<0, 0>>}, {packet_size, 2} | Config]).

-spec packet_size_4_tick() -> erldist_filter_test:testcase_info().
packet_size_4_tick() ->
    [
        {doc, "Bug when decoding tick message on fastpath with packet size 4"},
        {timetrap, {seconds, 30}}
    ].

-spec packet_size_4_tick(Config :: ct_suite:ct_config()) ->
    erldist_filter_test:testcase().
packet_size_4_tick(Config) ->
    packet_size_x_tick([{tick, <<0, 0, 0, 0>>}, {packet_size, 4} | Config]).

-spec packet_size_8_tick() -> erldist_filter_test:testcase_info().
packet_size_8_tick() ->
    [
        {doc, "Bug when decoding tick message on fastpath with packet size 8"},
        {timetrap, {seconds, 30}}
    ].

-spec packet_size_8_tick(Config :: ct_suite:ct_config()) ->
    erldist_filter_test:testcase().
packet_size_8_tick(Config) ->
    packet_size_x_tick([{tick, <<0, 0, 0, 0, 0, 0, 0, 0>>}, {packet_size, 8} | Config]).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec emulate_simple_commands(CommandList) -> ok when
    CommandList :: [Command],
    Command :: {'set', SymbolicVar, SymbolicCall},
    SymbolicVar :: proper_statem:symbolic_var(),
    SymbolicCall :: proper_statem:symbolic_call().
emulate_simple_commands([{set, Var1, {call, ?NIF_SPBT_SHIM, Fun = channel_open, Args}} | Commands]) ->
    {true, Channel} = emulate_simple_command(Fun, Args),
    ok = emulate_simple_commands({channel, Var1}, {channel, Channel}, Commands),
    ok = erldist_filter_nif:channel_close(Channel),
    ok.

%% @private
-spec emulate_simple_commands(Var, Channel, CommandList) -> ok when
    Var :: SymbolicVar,
    Channel :: erldist_filter_nif:channel(),
    CommandList :: [Command],
    Command :: {'set', SymbolicVar, SymbolicCall},
    SymbolicVar :: proper_statem:symbolic_var(),
    SymbolicCall :: proper_statem:symbolic_call().
emulate_simple_commands(Var, Channel, [{set, _, {call, ?NIF_SPBT_SHIM, Fun, [Var | Args0]}} | Commands]) ->
    Args = [Channel | Args0],
    ?assertMatch({true, _}, emulate_simple_command(Fun, Args)),
    emulate_simple_commands(Var, Channel, Commands);
emulate_simple_commands(_Var, _Channel, []) ->
    ok.

%% @private
-spec emulate_simple_command(FunName, Arguments) -> {true, Result} when
    FunName :: atom(),
    Arguments :: [Argument],
    Argument :: dynamic(),
    Result :: dynamic().
emulate_simple_command(channel_open, [PacketSize, Sysname, Creation, ConnectionId, DistributionFlags]) ->
    Channel = erldist_filter_nif:channel_open(PacketSize, Sysname, Creation, ConnectionId, DistributionFlags),
    {true, Channel};
emulate_simple_command(channel_dop_with_payload, [{channel, Channel}, #{packets := Packets}]) ->
    Result = erldist_filter_nif:channel_recv(Channel, Packets),
    {true, Result};
emulate_simple_command(channel_get_rx_atom_cache, [{channel, Channel}]) ->
    #{rx := #{atom_cache := RxAtomCache}} = erldist_filter_nif:channel_inspect(Channel),
    {true, RxAtomCache}.

%% @private
-spec packet_size_x_tick(Config :: ct_suite:ct_config()) ->
    erldist_filter_test:testcase().
packet_size_x_tick(Config) ->
    {tick, Tick} = lists:keyfind(tick, 1, Config),
    {packet_size, PacketSize} = lists:keyfind(packet_size, 1, Config),
    Sysname = 'packet-size-x@127.0.0.1',
    Creation = 1756475325,
    ConnectionId = 4804673,
    DistributionFlags = 193405616061,
    Channel = erldist_filter_nif:channel_open(PacketSize, Sysname, Creation, ConnectionId, DistributionFlags),
    ?assertMatch(
        #{rx := #{stats := #{packet_count := 0, emit_count := 0, dist_tick_count := 0, fastpath := 0, slowpath := 0}}},
        erldist_filter_nif:channel_inspect(Channel)
    ),
    ?assertMatch([], erldist_filter_nif:channel_recv(Channel, [Tick])),
    ?assertMatch(
        #{rx := #{stats := #{packet_count := 1, emit_count := 0, dist_tick_count := 1, fastpath := 1, slowpath := 0}}},
        erldist_filter_nif:channel_inspect(Channel)
    ),
    ?assertMatch([], erldist_filter_nif:channel_recv(Channel, [Tick, Tick])),
    ?assertMatch(
        #{rx := #{stats := #{packet_count := 3, emit_count := 0, dist_tick_count := 3, fastpath := 1, slowpath := 1}}},
        erldist_filter_nif:channel_inspect(Channel)
    ),
    ok = erldist_filter_nif:channel_close(Channel),
    ok.
