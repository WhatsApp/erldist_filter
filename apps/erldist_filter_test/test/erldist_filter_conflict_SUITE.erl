%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(erldist_filter_conflict_SUITE).
-moduledoc """
# Erlang Distribution Filter Conflict Test Suite

This Common Test suite provides comprehensive testing for atom cache conflict handling
in the `erldist_filter_nif` implementation. The tests verify that the NIF correctly
manages atom cache entries when conflicts occur during distributed Erlang communication.

## Test Coverage

The suite focuses on several critical conflict scenarios:

### Atom Cache Management
- **Multiple packet handling**: Validates processing of sequential packets with different atoms
- **Non-conflicting atoms**: Tests basic atom cache operations without hash conflicts
- **Hash conflicts**: Verifies proper handling when different atoms hash to the same cache index
- **Cache overwriting**: Tests overwrite behavior with and without rewrite operations
- **Rollback scenarios**: Validates proper rollback when cache operations need to be undone
- **Full cache conditions**: Tests behavior when the atom cache is at capacity

### Edge Cases
- **Zero packet size**: Special handling for packets with zero size
- **Fragment processing**: Tests packet fragmentation and reassembly
- **Cache rollback**: Verifies rollback mechanisms work correctly

## Test Architecture

Each test case follows a common pattern:
1. Initialize a channel with specific distribution flags
2. Generate non-conflicting or deliberately conflicting atom sets
3. Send encoded messages through the channel
4. Verify that the NIF produces identical results to the reference implementation

## Implementation Details

The suite uses several helper functions to:
- Generate atoms that hash to specific cache indices (`find_hash_conflict/1`)
- Create sets of non-conflicting atoms (`gen_non_conflicting_alpha_set/1`)
- Convert between different atom representations for testing

All tests run in parallel within the `conflicts` group to maximize testing efficiency
while ensuring thread safety of the underlying NIF implementation.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2022-10-26", modified => "2025-08-21"}.
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
    case0_multiple_packets/0,
    case0_multiple_packets/1,
    case1_read_without_atom_cache_conflicts/0,
    case1_read_without_atom_cache_conflicts/1,
    case1_write_without_atom_cache_conflicts/0,
    case1_write_without_atom_cache_conflicts/1,
    case1_overwrite_without_atom_cache_conflicts/0,
    case1_overwrite_without_atom_cache_conflicts/1,
    case2_overwrite_without_rewrite_with_rollback/0,
    case2_overwrite_without_rewrite_with_rollback/1,
    case3_overwrite_with_rewrite_with_rollback/0,
    case3_overwrite_with_rewrite_with_rollback/1,
    case3_overwrite_with_full_cache/0,
    case3_overwrite_with_full_cache/1,
    case4_zero_packet_size/0,
    case4_zero_packet_size/1
]).

%%%=============================================================================
%%% ct_suite callbacks
%%%=============================================================================

-spec all() -> erldist_filter_test:all().
all() ->
    [
        {group, conflicts}
    ].

-spec groups() -> erldist_filter_test:groups().
groups() ->
    [
        {conflicts, [parallel], [
            case0_multiple_packets,
            case1_read_without_atom_cache_conflicts,
            case1_write_without_atom_cache_conflicts,
            case1_overwrite_without_atom_cache_conflicts,
            case2_overwrite_without_rewrite_with_rollback,
            case3_overwrite_with_rewrite_with_rollback,
            case3_overwrite_with_full_cache,
            case4_zero_packet_size
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

-spec case0_multiple_packets() -> erldist_filter_test:testcase_info().
case0_multiple_packets() ->
    [
        {doc, "Tests sending multiple packets to erldist_filter_nif"},
        {timetrap, {seconds, 600}}
    ].

-spec case0_multiple_packets(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
case0_multiple_packets(_Config) ->
    PacketSize = 4,
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    [A, B, C, D, E, F, G, H, I] = gen_non_conflicting_alpha_set([a, b, c, d, e, f, g, h, i]),
    Channel = erldist_filter_nif:channel_open(PacketSize, 'nonode@nohost', 0, 0, DFlags),
    C0 = vedf_channel:new(PacketSize, DFlags),
    {ControlMessageA, _} = vdist_entry:reg_send_noop(0, 0, 0),
    {ControlMessageB, _} = vdist_entry:reg_send_noop(1, 1, 1),
    {ControlMessageC, _} = vdist_entry:reg_send_noop(2, 2, 2),
    {ControlMessageD, _} = vdist_entry:reg_send_noop(3, 3, 3),
    {ControlMessageE, _} = vdist_entry:reg_send_noop(4, 4, 4),
    {ControlMessageF, _} = vdist_entry:reg_send_noop(5, 5, 5),
    {ControlMessageG, _} = vdist_entry:reg_send_noop(6, 6, 6),
    {ControlMessageH, _} = vdist_entry:reg_send_noop(7, 7, 7),
    {ControlMessageI, _} = vdist_entry:reg_send_noop(8, 8, 8),
    LargeBin = binary:copy(<<"a">>, 255),
    Payload = {LargeBin, A, B, C, D, E, F, G, H, I},
    {ok, P0, C0} = vedf_channel:send_encode(C0, ControlMessageA, vterm:expand(Payload), SendOptions),
    {ok, A1, C1} = vedf_channel:recv(C0, P0),
    {ok, P1, C1} = vedf_channel:send_encode(C1, ControlMessageB, vterm:expand(Payload), SendOptions),
    {ok, A2, C2} = vedf_channel:recv(C1, P1),
    {ok, P2, C2} = vedf_channel:send_encode(C2, ControlMessageC, vterm:expand(Payload), SendOptions),
    {ok, A3, C3} = vedf_channel:recv(C2, P2),
    {ok, P3, C3} = vedf_channel:send_encode(C3, ControlMessageD, vterm:expand(Payload), SendOptions),
    {ok, A4, C4} = vedf_channel:recv(C3, P3),
    {ok, P4, C4} = vedf_channel:send_encode(C4, ControlMessageE, vterm:expand(Payload), SendOptions),
    {ok, A5, C5} = vedf_channel:recv(C4, P4),
    {ok, P5, C5} = vedf_channel:send_encode(C5, ControlMessageF, vterm:expand(Payload), SendOptions),
    {ok, A6, C6} = vedf_channel:recv(C5, P5),
    {ok, P6, C6} = vedf_channel:send_encode(C6, ControlMessageG, vterm:expand(Payload), SendOptions),
    {ok, A7, C7} = vedf_channel:recv(C6, P6),
    {ok, P7, C7} = vedf_channel:send_encode(C7, ControlMessageH, vterm:expand(Payload), SendOptions),
    {ok, A8, C8} = vedf_channel:recv(C7, P7),
    {ok, P8, C8} = vedf_channel:send_encode(C8, ControlMessageI, vterm:expand(Payload), SendOptions),
    {ok, A9, C9} = vedf_channel:recv(C8, P8),
    AllPackets = lists:flatten([P0, P1, P2, P3, P4, P5, P6, P7, P8]),
    AllActions = lists:flatten([A1, A2, A3, A4, A5, A6, A7, A8, A9]),
    ?assertEqual(AllActions, erldist_filter_nif:channel_recv(Channel, AllPackets)),
    _ = C9,
    ok = erldist_filter_nif:channel_close(Channel).

-spec case1_read_without_atom_cache_conflicts() -> erldist_filter_test:testcase_info().
case1_read_without_atom_cache_conflicts() ->
    [
        {doc, "Tests reading without atom cache conflicts in erldist_filter_nif"},
        {timetrap, {seconds, 600}}
    ].

-spec case1_read_without_atom_cache_conflicts(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
case1_read_without_atom_cache_conflicts(_Config) ->
    PacketSize = 4,
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    [A, B, C, D] = gen_non_conflicting_alpha_set([a, b, c, d]),
    Channel = erldist_filter_nif:channel_open(PacketSize, 'nonode@nohost', 0, 0, DFlags),
    C0 = vedf_channel:new(PacketSize, DFlags),
    {ControlMessageA, _} = vdist_entry:reg_send_noop(),
    {ControlMessageB, _} = vdist_entry:reg_send_noop_alt(),
    LargeBin = binary:copy(<<"a">>, 255),
    {ok, P0, C0} = vedf_channel:send_encode(C0, ControlMessageA, vterm:expand({LargeBin, A, B, C, D}), SendOptions),
    {ok, A1, C1} = vedf_channel:recv(C0, P0),
    ?assertEqual(A1, erldist_filter_nif:channel_recv(Channel, P0)),
    {ok, [PHead | PTail], C1} = vedf_channel:send_encode(
        C1, ControlMessageA, vterm:expand({LargeBin, A, C}), SendOptions
    ),
    {ok, A2, C2} = vedf_channel:recv(C1, [PHead]),
    ?assertEqual(A2, erldist_filter_nif:channel_recv(Channel, [PHead])),
    {ok, P1, C2} = vedf_channel:send_encode(C2, ControlMessageB, vterm:expand({A, B, D}), SendOptions),
    {ok, A3, C3} = vedf_channel:recv(C2, P1),
    ?assertEqual(A3, erldist_filter_nif:channel_recv(Channel, P1)),
    {ok, A4, C4} = vedf_channel:recv(C3, PTail),
    ?assertEqual(A4, erldist_filter_nif:channel_recv(Channel, PTail)),
    _ = C4,
    ok = erldist_filter_nif:channel_close(Channel).

-spec case1_write_without_atom_cache_conflicts() -> erldist_filter_test:testcase_info().
case1_write_without_atom_cache_conflicts() ->
    [
        {doc, "Tests writing without atom cache conflicts in erldist_filter_nif"},
        {timetrap, {seconds, 600}}
    ].

-spec case1_write_without_atom_cache_conflicts(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
case1_write_without_atom_cache_conflicts(_Config) ->
    PacketSize = 4,
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    [A, B, C, D] = gen_non_conflicting_alpha_set([a, b, c, d]),
    Channel = erldist_filter_nif:channel_open(PacketSize, 'nonode@nohost', 0, 0, DFlags),
    C0 = vedf_channel:new(PacketSize, DFlags),
    {ControlMessageA, _} = vdist_entry:reg_send_noop(),
    {ControlMessageB, _} = vdist_entry:reg_send_noop_alt(),
    LargeBin = binary:copy(<<"a">>, 255),
    {ok, P0, C0} = vedf_channel:send_encode(C0, ControlMessageA, vterm:expand({LargeBin, A, B}), SendOptions),
    {ok, A1, C1} = vedf_channel:recv(C0, P0),
    ?assertEqual(A1, erldist_filter_nif:channel_recv(Channel, P0)),
    {ok, [PHead | PTail], C1} = vedf_channel:send_encode(
        C1, ControlMessageA, vterm:expand({LargeBin, A, C}), SendOptions
    ),
    {ok, A2, C2} = vedf_channel:recv(C1, [PHead]),
    ?assertEqual(A2, erldist_filter_nif:channel_recv(Channel, [PHead])),
    {ok, P1, C2} = vedf_channel:send_encode(C2, ControlMessageB, vterm:expand({A, B, D}), SendOptions),
    {ok, A3, C3} = vedf_channel:recv(C2, P1),
    ?assertEqual(A3, erldist_filter_nif:channel_recv(Channel, P1)),
    {ok, A4, C4} = vedf_channel:recv(C3, PTail),
    ?assertEqual(A4, erldist_filter_nif:channel_recv(Channel, PTail)),
    _ = C4,
    ok = erldist_filter_nif:channel_close(Channel).

-spec case1_overwrite_without_atom_cache_conflicts() -> erldist_filter_test:testcase_info().
case1_overwrite_without_atom_cache_conflicts() ->
    [
        {doc, "Tests overwriting without atom cache conflicts in erldist_filter_nif"},
        {timetrap, {seconds, 600}}
    ].

-spec case1_overwrite_without_atom_cache_conflicts(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
case1_overwrite_without_atom_cache_conflicts(_Config) ->
    PacketSize = 4,
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    [A, B, C, D] = gen_non_conflicting_alpha_set([a, b, c, d]),
    BConflict = find_hash_conflict(B),
    Channel = erldist_filter_nif:channel_open(PacketSize, 'nonode@nohost', 0, 0, DFlags),
    C0 = vedf_channel:new(PacketSize, DFlags),
    {ControlMessageA, _} = vdist_entry:reg_send_noop(),
    {ControlMessageB, _} = vdist_entry:reg_send_noop_alt(),
    LargeBin = binary:copy(<<"a">>, 255),
    {ok, P0, C0} = vedf_channel:send_encode(C0, ControlMessageA, vterm:expand({LargeBin, A, B}), SendOptions),
    {ok, A1, C1} = vedf_channel:recv(C0, P0),
    ?assertEqual(A1, erldist_filter_nif:channel_recv(Channel, P0)),
    {ok, [PHead | PTail], C1} = vedf_channel:send_encode(
        C1, ControlMessageA, vterm:expand({LargeBin, A, BConflict, C}), SendOptions
    ),
    {ok, A2, C2} = vedf_channel:recv(C1, [PHead]),
    ?assertEqual(A2, erldist_filter_nif:channel_recv(Channel, [PHead])),
    {ok, P1, C2} = vedf_channel:send_encode(C2, ControlMessageB, vterm:expand({A, BConflict, D}), SendOptions),
    {ok, A3, C3} = vedf_channel:recv(C2, P1),
    ?assertEqual(A3, erldist_filter_nif:channel_recv(Channel, P1)),
    {ok, A4, C4} = vedf_channel:recv(C3, PTail),
    ?assertEqual(A4, erldist_filter_nif:channel_recv(Channel, PTail)),
    _ = C4,
    ok = erldist_filter_nif:channel_close(Channel).

-spec case2_overwrite_without_rewrite_with_rollback() -> erldist_filter_test:testcase_info().
case2_overwrite_without_rewrite_with_rollback() ->
    [
        {doc, "Tests overwriting without rewrite, but with rollback in erldist_filter_nif"},
        {timetrap, {seconds, 600}}
    ].

-spec case2_overwrite_without_rewrite_with_rollback(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
case2_overwrite_without_rewrite_with_rollback(_Config) ->
    PacketSize = 4,
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    [A, B, C, D] = gen_non_conflicting_alpha_set([a, b, c, d]),
    CConflict = find_hash_conflict(C),
    Channel = erldist_filter_nif:channel_open(PacketSize, 'nonode@nohost', 0, 0, DFlags),
    C0 = vedf_channel:new(PacketSize, DFlags),
    {ControlMessageA, _} = vdist_entry:reg_send_noop(),
    {ControlMessageB, _} = vdist_entry:reg_send_noop_alt(),
    LargeBin = binary:copy(<<"a">>, 255),
    {ok, P0, C0} = vedf_channel:send_encode(C0, ControlMessageA, vterm:expand({LargeBin, A, B}), SendOptions),
    {ok, A1, C1} = vedf_channel:recv(C0, P0),
    ?assertEqual(A1, erldist_filter_nif:channel_recv(Channel, P0)),
    {ok, [PHead | PTail], C1} = vedf_channel:send_encode(
        C1, ControlMessageA, vterm:expand({LargeBin, A, C}), SendOptions
    ),
    {ok, A2, C2} = vedf_channel:recv(C1, [PHead]),
    ?assertEqual(A2, erldist_filter_nif:channel_recv(Channel, [PHead])),
    {ok, P1, C2} = vedf_channel:send_encode(C2, ControlMessageB, vterm:expand({A, CConflict, D}), SendOptions),
    {ok, A3, C3} = vedf_channel:recv(C2, P1),
    ?assertEqual(A3, erldist_filter_nif:channel_recv(Channel, P1)),
    {ok, A4, C4} = vedf_channel:recv(C3, PTail),
    ?assertEqual(A4, erldist_filter_nif:channel_recv(Channel, PTail)),
    _ = C4,
    ok = erldist_filter_nif:channel_close(Channel).

-spec case3_overwrite_with_rewrite_with_rollback() -> erldist_filter_test:testcase_info().
case3_overwrite_with_rewrite_with_rollback() ->
    [
        {doc, "Tests overwriting with both rewrite and rollback in erldist_filter_nif"},
        {timetrap, {seconds, 600}}
    ].

-spec case3_overwrite_with_rewrite_with_rollback(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
case3_overwrite_with_rewrite_with_rollback(_Config) ->
    PacketSize = 4,
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    [A, B] = gen_non_conflicting_alpha_set([a, b]),
    AConflict = find_hash_conflict(A),
    BConflict = find_hash_conflict(B),
    Channel = erldist_filter_nif:channel_open(PacketSize, 'nonode@nohost', 0, 0, DFlags),
    C0 = vedf_channel:new(PacketSize, DFlags),
    {ControlMessageA, _} = vdist_entry:reg_send_noop(),
    {ControlMessageB, _} = vdist_entry:reg_send_noop_alt(),
    LargeBin = binary:copy(<<"a">>, 255),
    {ok, P0, C0} = vedf_channel:send_encode(C0, ControlMessageA, vterm:expand({LargeBin, A, B}), SendOptions),
    {ok, A1, C1} = vedf_channel:recv(C0, P0),
    ?assertEqual(A1, erldist_filter_nif:channel_recv(Channel, P0)),
    {ok, [PHead | PTail], C1} = vedf_channel:send_encode(
        C1, ControlMessageA, vterm:expand({LargeBin, AConflict, B}), SendOptions
    ),
    {ok, A2, C2} = vedf_channel:recv(C1, [PHead]),
    ?assertEqual(A2, erldist_filter_nif:channel_recv(Channel, [PHead])),
    {ok, P1, C2} = vedf_channel:send_encode(C2, ControlMessageB, vterm:expand({A, BConflict}), SendOptions),
    {ok, A3, C3} = vedf_channel:recv(C2, P1),
    ?assertEqual(A3, erldist_filter_nif:channel_recv(Channel, P1)),
    {ok, A4, C4} = vedf_channel:recv(C3, PTail),
    ?assertEqual(A4, erldist_filter_nif:channel_recv(Channel, PTail)),
    _ = C4,
    ok = erldist_filter_nif:channel_close(Channel).

-spec case3_overwrite_with_full_cache() -> erldist_filter_test:testcase_info().
case3_overwrite_with_full_cache() ->
    [
        {doc, "Tests overwriting with a full atom cache in erldist_filter_nif"},
        {timetrap, {seconds, 600}}
    ].

-spec case3_overwrite_with_full_cache(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
case3_overwrite_with_full_cache(_Config) ->
    PacketSize = 4,
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    Atoms = [A, B, C | RestAtoms] = gen_non_conflicting_alpha_set({a, 2039}),
    AConflict = find_hash_conflict(A),
    BConflict = find_hash_conflict(B),
    CConflict = find_hash_conflict(C),
    RestConflictsA = [find_hash_conflict(Atom) || Atom <- RestAtoms],
    RestConflictsB = [find_hash_conflict(Atom) || Atom <- RestConflictsA],
    RestConflictsC = [find_hash_conflict(Atom) || Atom <- RestConflictsB],
    Channel = erldist_filter_nif:channel_open(PacketSize, 'nonode@nohost', 0, 0, DFlags),
    C0 = vedf_channel:new(PacketSize, DFlags),
    {ControlMessageA, _} = vdist_entry:reg_send_noop(0, 0, 0),
    {ControlMessageB, _} = vdist_entry:reg_send_noop(1, 1, 1),
    {ControlMessageC, _} = vdist_entry:reg_send_noop(2, 2, 2),
    LargeBin = binary:copy(<<"a">>, 255),
    C1 = lists:foldl(
        fun(Atom, Cacc0) ->
            {ok, P0, Cacc0} = vedf_channel:send_encode(
                Cacc0, ControlMessageA, vterm:expand({LargeBin, Atom}), SendOptions
            ),
            {ok, A1, Cacc1} = vedf_channel:recv(Cacc0, P0),
            begin
                ?assertEqual(A1, erldist_filter_nif:channel_recv(Channel, P0))
            end,
            Cacc1
        end,
        C0,
        Atoms
    ),
    {ok, [PHeadA | PTailA], C1} = vedf_channel:send_encode(
        C1, ControlMessageA, vterm:expand({LargeBin, AConflict, B, C, RestConflictsA}), SendOptions
    ),
    {ok, A2, C2} = vedf_channel:recv(C1, [PHeadA]),
    ?assertEqual(A2, erldist_filter_nif:channel_recv(Channel, [PHeadA])),
    {ok, [PHeadB | PTailB], C2} = vedf_channel:send_encode(
        C2, ControlMessageB, vterm:expand({LargeBin, A, BConflict, C, RestConflictsB}), SendOptions
    ),
    {ok, A3, C3} = vedf_channel:recv(C2, [PHeadB]),
    ?assertEqual(A3, erldist_filter_nif:channel_recv(Channel, [PHeadB])),
    {ok, [PHeadC | PTailC], C3} = vedf_channel:send_encode(
        C3, ControlMessageC, vterm:expand({LargeBin, A, B, CConflict, RestConflictsC}), SendOptions
    ),
    {ok, A4, C4} = vedf_channel:recv(C3, [PHeadC]),
    ?assertEqual(A4, erldist_filter_nif:channel_recv(Channel, [PHeadC])),
    {ok, A5, C5} = vedf_channel:recv(C4, PTailB),
    ?assertEqual(A5, erldist_filter_nif:channel_recv(Channel, PTailB)),
    {ok, A6, C6} = vedf_channel:recv(C5, PTailA),
    ?assertEqual(A6, erldist_filter_nif:channel_recv(Channel, PTailA)),
    {ok, A7, C7} = vedf_channel:recv(C6, PTailC),
    ?assertEqual(A7, erldist_filter_nif:channel_recv(Channel, PTailC)),
    _ = C7,
    ok = erldist_filter_nif:channel_close(Channel).

-spec case4_zero_packet_size() -> erldist_filter_test:testcase_info().
case4_zero_packet_size() ->
    [
        {doc, "Tests zero sized packets in erldist_filter_nif"},
        {timetrap, {seconds, 600}}
    ].

-spec case4_zero_packet_size(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
case4_zero_packet_size(_Config) ->
    PacketSize = 0,
    DFlags = maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()),
    SendOptions = #{header_mode => fragment, fragment_size => 16#7F},
    [A, B, C, D, E, F, G, H, I] = gen_non_conflicting_alpha_set([a, b, c, d, e, f, g, h, i]),
    Channel = erldist_filter_nif:channel_open(PacketSize, 'nonode@nohost', 0, 0, DFlags),
    C0 = vedf_channel:new(PacketSize, DFlags),
    {ControlMessageA, _} = vdist_entry:reg_send_noop(0, 0, 0),
    {ControlMessageB, _} = vdist_entry:reg_send_noop(1, 1, 1),
    {ControlMessageC, _} = vdist_entry:reg_send_noop(2, 2, 2),
    {ControlMessageD, _} = vdist_entry:reg_send_noop(3, 3, 3),
    {ControlMessageE, _} = vdist_entry:reg_send_noop(4, 4, 4),
    {ControlMessageF, _} = vdist_entry:reg_send_noop(5, 5, 5),
    {ControlMessageG, _} = vdist_entry:reg_send_noop(6, 6, 6),
    {ControlMessageH, _} = vdist_entry:reg_send_noop(7, 7, 7),
    {ControlMessageI, _} = vdist_entry:reg_send_noop(8, 8, 8),
    LargeBin = binary:copy(<<"a">>, 255),
    Payload = {LargeBin, A, B, C, D, E, F, G, H, I},
    {ok, P0, C0} = vedf_channel:send_encode(C0, ControlMessageA, vterm:expand(Payload), SendOptions),
    {ok, A1, C1} = vedf_channel:recv(C0, P0),
    {ok, P1, C1} = vedf_channel:send_encode(C1, ControlMessageB, vterm:expand(Payload), SendOptions),
    {ok, A2, C2} = vedf_channel:recv(C1, P1),
    {ok, P2, C2} = vedf_channel:send_encode(C2, ControlMessageC, vterm:expand(Payload), SendOptions),
    {ok, A3, C3} = vedf_channel:recv(C2, P2),
    {ok, P3, C3} = vedf_channel:send_encode(C3, ControlMessageD, vterm:expand(Payload), SendOptions),
    {ok, A4, C4} = vedf_channel:recv(C3, P3),
    {ok, P4, C4} = vedf_channel:send_encode(C4, ControlMessageE, vterm:expand(Payload), SendOptions),
    {ok, A5, C5} = vedf_channel:recv(C4, P4),
    {ok, P5, C5} = vedf_channel:send_encode(C5, ControlMessageF, vterm:expand(Payload), SendOptions),
    {ok, A6, C6} = vedf_channel:recv(C5, P5),
    {ok, P6, C6} = vedf_channel:send_encode(C6, ControlMessageG, vterm:expand(Payload), SendOptions),
    {ok, A7, C7} = vedf_channel:recv(C6, P6),
    {ok, P7, C7} = vedf_channel:send_encode(C7, ControlMessageH, vterm:expand(Payload), SendOptions),
    {ok, A8, C8} = vedf_channel:recv(C7, P7),
    {ok, P8, C8} = vedf_channel:send_encode(C8, ControlMessageI, vterm:expand(Payload), SendOptions),
    {ok, A9, C9} = vedf_channel:recv(C8, P8),
    AllPackets = lists:flatten([P0, P1, P2, P3, P4, P5, P6, P7, P8]),
    AllActions = lists:flatten([A1, A2, A3, A4, A5, A6, A7, A8, A9]),
    ?assertEqual(
        AllActions, lists:flatten([erldist_filter_nif:channel_recv(Channel, [Packet]) || Packet <- AllPackets])
    ),
    _ = C9,
    ok = erldist_filter_nif:channel_close(Channel).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec find_hash_conflict(Atom) -> Candidate when
    Atom :: atom(),
    Candidate :: atom().
find_hash_conflict(Atom) when is_atom(Atom) ->
    do_find_hash_conflict(Atom, vdist_atom_cache:atom_cache_index(Atom), 0).

%% @private
-spec do_find_hash_conflict(Atom, Target, I) -> Candidate when
    Atom :: atom(),
    Target :: vdist_atom_cache:index(),
    I :: non_neg_integer(),
    Candidate :: atom().
do_find_hash_conflict(Atom, Target, I) when is_atom(Atom) andalso is_integer(Target) andalso is_integer(I) ->
    Candidate = erlang:list_to_atom(lists:flatten(io_lib:format("~s~w", [Atom, I]))),
    case vdist_atom_cache:atom_cache_index(Candidate) of
        Target ->
            Candidate;
        _ ->
            do_find_hash_conflict(Atom, Target, I + 1)
    end.

%% @private
-spec gen_non_conflicting_alpha_set(AtomState) -> AtomList when
    AtomState :: {FirstAtom, Count} | AtomList,
    FirstAtom :: atom(),
    Count :: pos_integer(),
    AtomList :: [atom()].
gen_non_conflicting_alpha_set({FirstAtom, Count}) when is_atom(FirstAtom) andalso is_integer(Count) andalso Count > 0 ->
    do_gen_non_conflicting_alpha_set({FirstAtom, Count}, maps:new(), []);
gen_non_conflicting_alpha_set(Atoms = [_ | _]) ->
    do_gen_non_conflicting_alpha_set(Atoms, maps:new(), []).

%% @private
-spec do_gen_non_conflicting_alpha_set(AtomState, Seen, AtomList) -> AtomList when
    AtomState :: {FirstAtom, Count} | AtomList,
    FirstAtom :: atom(),
    Count :: pos_integer(),
    AtomList :: [atom()],
    Seen :: #{Index => []},
    Index :: vdist_atom_cache:index().
do_gen_non_conflicting_alpha_set({_Atom, 0}, _Seen, Acc) ->
    lists:reverse(Acc);
do_gen_non_conflicting_alpha_set({Atom0, Count}, Seen0, Acc0) when is_integer(Count) andalso Count > 0 ->
    Index = vdist_atom_cache:atom_cache_index(Atom0),
    case maps:is_key(Index, Seen0) of
        true ->
            Atom1 = next_lowercase_alpha_atom(Atom0),
            do_gen_non_conflicting_alpha_set({Atom1, Count}, Seen0, Acc0);
        false ->
            Seen1 = Seen0#{Index => []},
            Acc1 = [Atom0 | Acc0],
            do_gen_non_conflicting_alpha_set({Atom0, Count - 1}, Seen1, Acc1)
    end;
do_gen_non_conflicting_alpha_set([Atom0 | Atoms], Seen0, Acc0) ->
    Index = vdist_atom_cache:atom_cache_index(Atom0),
    case maps:is_key(Index, Seen0) of
        true ->
            Atom1 = next_lowercase_alpha_atom(Atom0),
            do_gen_non_conflicting_alpha_set([Atom1 | Atoms], Seen0, Acc0);
        false ->
            Seen1 = Seen0#{Index => []},
            Acc1 = [Atom0 | Acc0],
            do_gen_non_conflicting_alpha_set(Atoms, Seen1, Acc1)
    end;
do_gen_non_conflicting_alpha_set([], _Seen, Acc) ->
    lists:reverse(Acc).

%% @private
-spec base26_to_lowercase_alpha(In) -> Out when
    In :: binary(),
    Out :: binary().
base26_to_lowercase_alpha(B) when is_binary(B) ->
    <<
        <<
            (case C of
                _ when C >= $0 andalso C =< $9 ->
                    (C - $0) + $a;
                _ when C >= $A andalso C =< $P ->
                    (C - $A) + $a + 10;
                _ when C >= $a andalso C =< $p ->
                    C + 10
            end):8
        >>
     || <<C:8>> <= B
    >>.

%% @private
-spec integer_to_lowercase_alpha(In) -> Out when
    In :: non_neg_integer(),
    Out :: binary().
integer_to_lowercase_alpha(I) when is_integer(I) andalso I >= 0 ->
    base26_to_lowercase_alpha(erlang:integer_to_binary(I, 26)).

%% @private
-spec lowercase_alpha_to_base26(In) -> Out when
    In :: binary(),
    Out :: binary().
lowercase_alpha_to_base26(B) when is_binary(B) ->
    <<
        <<
            (case C of
                _ when C >= $a andalso C =< $j ->
                    (C - $a) + $0;
                _ when C >= $k andalso C =< $z ->
                    (C - $k) + $A
            end):8
        >>
     || <<C:8>> <= B
    >>.

%% @private
-spec lowercase_alpha_to_integer(In) -> Out when
    In :: binary(),
    Out :: non_neg_integer().
lowercase_alpha_to_integer(B) when is_binary(B) ->
    erlang:binary_to_integer(lowercase_alpha_to_base26(B), 26).

%% @private
-spec next_lowercase_alpha_atom(In) -> Out when
    In :: atom(),
    Out :: atom().
next_lowercase_alpha_atom(Atom) when is_atom(Atom) ->
    case erlang:atom_to_binary(Atom, utf8) of
        <<>> ->
            'a';
        AtomText ->
            erlang:binary_to_atom(next_lowercase_alpha_binary(AtomText), utf8)
    end.

%% @private
-spec next_lowercase_alpha_binary(In) -> Out when
    In :: binary() | non_neg_integer(),
    Out :: binary().
next_lowercase_alpha_binary(B) when is_binary(B) ->
    Len = byte_size(B),
    I = lowercase_alpha_to_integer(B),
    case integer_to_lowercase_alpha(I + 1) of
        Wrapped when byte_size(Wrapped) > Len ->
            binary:copy(<<$a:8>>, Len + 1);
        Short when byte_size(Short) < Len ->
            <<(binary:copy(<<$a:8>>, Len - byte_size(Short)))/bytes, Short/bytes>>;
        Next ->
            Next
    end;
next_lowercase_alpha_binary(I) when is_integer(I) andalso I >= 0 ->
    next_lowercase_alpha_binary(integer_to_lowercase_alpha(I)).
