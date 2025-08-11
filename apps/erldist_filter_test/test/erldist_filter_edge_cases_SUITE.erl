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
%%% Created :  17 Jan 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
-module(erldist_filter_edge_cases_SUITE).
-typing([eqwalizer]).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

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
    counterexample8444249432900889_decode_length_trap_bug/0,
    counterexample8444249432900889_decode_length_trap_bug/1
]).

%% Macros
-define(NIF_SPBT_SHIM, erldist_filter_nif_spbt_shim).

%%%=============================================================================
%%% ct callbacks
%%%=============================================================================

all() ->
    [
        {group, edge_cases}
    ].

groups() ->
    [
        {edge_cases, [parallel], [
            counterexample8444249432900889_decode_length_trap_bug
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

%%%=============================================================================
%%% Test Cases
%%%=============================================================================

counterexample8444249432900889_decode_length_trap_bug() ->
    [
        {doc, "Bug when decoding control term that is the exact size of trapping slice, see T142904516"},
        {timetrap, {seconds, 120}}
    ].

counterexample8444249432900889_decode_length_trap_bug(Config) ->
    DataDir = test_server:lookup_config(data_dir, Config),
    TestVectorsFile = filename:join([DataDir, "counterexample8444249432900889_decode_length_trap_bug.config"]),
    {ok, [Commands]} = file:consult(TestVectorsFile),
    emulate_simple_commands(Commands).

%% @private
emulate_simple_commands([{set, Var1, {call, ?NIF_SPBT_SHIM, Fun = channel_open, Args}} | Commands]) ->
    {true, Channel} = emulate_simple_command(Fun, Args),
    emulate_simple_commands({channel, Var1}, {channel, Channel}, Commands).

%% @private
emulate_simple_commands(Var, Channel, [{set, _, {call, ?NIF_SPBT_SHIM, Fun, [Var | Args0]}} | Commands]) ->
    Args = [Channel | Args0],
    ?assertMatch({true, _}, emulate_simple_command(Fun, Args)),
    emulate_simple_commands(Var, Channel, Commands);
emulate_simple_commands(_Var, _Channel, []) ->
    ok.

%% @private
emulate_simple_command(channel_open, [PacketSize, Sysname, Creation, ConnectionId, DistributionFlags]) ->
    Channel = erldist_filter_nif:channel_open(PacketSize, Sysname, Creation, ConnectionId, DistributionFlags),
    {true, Channel};
emulate_simple_command(channel_dop_with_payload, [{channel, Channel}, #{packets := Packets}]) ->
    Result = erldist_filter_nif:channel_recv(Channel, Packets),
    {true, Result};
emulate_simple_command(channel_get_rx_atom_cache, [{channel, Channel}]) ->
    #{rx := #{atom_cache := RxAtomCache}} = erldist_filter_nif:channel_inspect(Channel),
    {true, RxAtomCache}.
