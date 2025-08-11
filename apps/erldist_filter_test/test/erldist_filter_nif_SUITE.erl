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
%%% @doc A test suite designed to load the `erldist_filter_nif' and run tests.
%%%
%%% @end
%%% Created :  19 May 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
-module(erldist_filter_nif_SUITE).
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
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test Cases
-export([
    dist_ext_to_vdist_2/0,
    dist_ext_to_vdist_2/1,
    dist_ext_to_vterm_2/0,
    dist_ext_to_vterm_2/1,
    dist_ext_to_vterm_3/0,
    dist_ext_to_vterm_3/1,
    dist_int_to_vdist_2/0,
    dist_int_to_vdist_2/1,
    dist_int_to_vterm_2/0,
    dist_int_to_vterm_2/1,
    dist_int_to_vterm_3/0,
    dist_int_to_vterm_3/1,
    prop_dist_ext_to_vdist_2/0,
    prop_dist_ext_to_vdist_2/1,
    prop_dist_ext_to_vterm_2/0,
    prop_dist_ext_to_vterm_2/1,
    prop_dist_ext_to_vterm_3/0,
    prop_dist_ext_to_vterm_3/1,
    prop_dist_int_to_vdist_2/0,
    prop_dist_int_to_vdist_2/1,
    prop_dist_int_to_vterm_2/0,
    prop_dist_int_to_vterm_2/1,
    prop_dist_int_to_vterm_3/0,
    prop_dist_int_to_vterm_3/1
]).

%%%=============================================================================
%%% ct callbacks
%%%=============================================================================

all() ->
    [
        {group, vedf}
    ].

groups() ->
    [
        {vedf, [parallel], [
            dist_ext_to_vdist_2,
            dist_ext_to_vterm_2,
            dist_ext_to_vterm_3,
            dist_int_to_vdist_2,
            dist_int_to_vterm_2,
            dist_int_to_vterm_3,
            prop_dist_ext_to_vdist_2,
            prop_dist_ext_to_vterm_2,
            prop_dist_ext_to_vterm_3,
            prop_dist_int_to_vdist_2,
            prop_dist_int_to_vterm_2,
            prop_dist_int_to_vterm_3
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

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%=============================================================================
%%% Test Cases
%%%=============================================================================

dist_ext_to_vdist_2() ->
    [
        {doc, "Loads the `erldist_filter_nif' and verifies that `dist_ext_to_vdist/2' functions as expected."},
        {timetrap, {seconds, 60}}
    ].

dist_ext_to_vdist_2(Config) ->
    dist_ext_to_vdist_2(Config, vdist_test_vectors()).

%% @private
dist_ext_to_vdist_2(Config, [{VDistNoAtoms, {Atoms, VDistWithAtoms}} | TestVectors]) ->
    ExternalNoAtomsBinary = vterm_encode:external_vterm_to_binary(
        vdist_dop:dop_to_control_message_vterm(VDistNoAtoms), #{}
    ),
    ExternalWithAtomsBinary = vterm_encode:external_vterm_to_binary(
        vdist_dop:dop_to_control_message_vterm(VDistWithAtoms), #{allow_atom_cache_refs => true}
    ),
    ?assertEqual(VDistNoAtoms, vedf:dist_ext_to_vdist({}, ExternalNoAtomsBinary)),
    ?assertEqual(VDistNoAtoms, erldist_filter_nif:dist_ext_to_vdist({}, ExternalNoAtomsBinary)),
    ?assertEqual(VDistWithAtoms, vedf:dist_ext_to_vdist(Atoms, ExternalWithAtomsBinary)),
    ?assertEqual(VDistWithAtoms, erldist_filter_nif:dist_ext_to_vdist(Atoms, ExternalWithAtomsBinary)),
    dist_ext_to_vdist_2(Config, TestVectors);
dist_ext_to_vdist_2(_Config, []) ->
    ok.

dist_ext_to_vterm_2() ->
    [
        {doc, "Loads the `erldist_filter_nif' and verifies that `dist_ext_to_vterm/2' functions as expected."},
        {timetrap, {seconds, 60}}
    ].

dist_ext_to_vterm_2(Config) ->
    dist_ext_to_vterm_2(Config, vterm_test_vectors()).

%% @private
dist_ext_to_vterm_2(Config, [{Atoms, VTermInput, VTermOutput} | TestVectors]) ->
    ExternalBinary = vterm_encode:external_vterm_to_binary(VTermInput, #{allow_atom_cache_refs => true}),
    ?assertEqual(VTermOutput, erldist_filter_nif:dist_ext_to_vterm(Atoms, ExternalBinary)),
    dist_ext_to_vterm_2(Config, TestVectors);
dist_ext_to_vterm_2(_Config, []) ->
    ok.

dist_ext_to_vterm_3() ->
    [
        {doc, "Loads the `erldist_filter_nif' and verifies that `dist_ext_to_vterm/3' functions as expected."},
        {timetrap, {seconds, 60}}
    ].

dist_ext_to_vterm_3(Config) ->
    dist_ext_to_vterm_3(Config, vterm_test_vectors()).

%% @private
dist_ext_to_vterm_3(Config, [{Atoms, VTermInput, VTermOutput} | TestVectors]) ->
    ExternalBinary = vterm_encode:external_vterm_to_binary(VTermInput, #{allow_atom_cache_refs => true}),
    ExpectedLazy0 = vedf:dist_ext_to_vterm(Atoms, ExternalBinary, 0),
    ExpectedLazy1 = vedf:dist_ext_to_vterm(Atoms, ExternalBinary, 1),
    ExpectedLazy2 = vedf:dist_ext_to_vterm(Atoms, ExternalBinary, 2),
    ?assertEqual(VTermOutput, erldist_filter_nif:dist_ext_to_vterm(Atoms, ExternalBinary, -1)),
    ?assertEqual(ExpectedLazy0, erldist_filter_nif:dist_ext_to_vterm(Atoms, ExternalBinary, 0)),
    ?assertEqual(ExpectedLazy1, erldist_filter_nif:dist_ext_to_vterm(Atoms, ExternalBinary, 1)),
    ?assertEqual(ExpectedLazy2, erldist_filter_nif:dist_ext_to_vterm(Atoms, ExternalBinary, 2)),
    dist_ext_to_vterm_3(Config, TestVectors);
dist_ext_to_vterm_3(_Config, []) ->
    ok.

dist_int_to_vdist_2() ->
    [
        {doc, "Loads the `erldist_filter_nif' and verifies that `dist_int_to_vdist/2' functions as expected."},
        {timetrap, {seconds, 60}}
    ].

dist_int_to_vdist_2(Config) ->
    dist_int_to_vdist_2(Config, vdist_test_vectors()).

%% @private
dist_int_to_vdist_2(Config, [{VDistNoAtoms, {Atoms, VDistWithAtoms}} | TestVectors]) ->
    InternalNoAtomsBinary = vterm_encode:internal_vterm_to_binary(
        vdist_dop:dop_to_control_message_vterm(VDistNoAtoms), #{}
    ),
    InternalWithAtomsBinary = vterm_encode:internal_vterm_to_binary(
        vdist_dop:dop_to_control_message_vterm(VDistWithAtoms), #{allow_atom_cache_refs => true}
    ),
    ?assertEqual(VDistNoAtoms, vedf:dist_int_to_vdist({}, InternalNoAtomsBinary)),
    ?assertEqual(VDistNoAtoms, erldist_filter_nif:dist_int_to_vdist({}, InternalNoAtomsBinary)),
    ?assertEqual(VDistWithAtoms, vedf:dist_int_to_vdist(Atoms, InternalWithAtomsBinary)),
    ?assertEqual(VDistWithAtoms, erldist_filter_nif:dist_int_to_vdist(Atoms, InternalWithAtomsBinary)),
    dist_int_to_vdist_2(Config, TestVectors);
dist_int_to_vdist_2(_Config, []) ->
    ok.

dist_int_to_vterm_2() ->
    [
        {doc, "Loads the `erldist_filter_nif' and verifies that `dist_int_to_vterm/2' functions as expected."},
        {timetrap, {seconds, 60}}
    ].

dist_int_to_vterm_2(Config) ->
    dist_int_to_vterm_2(Config, vterm_test_vectors()).

%% @private
dist_int_to_vterm_2(Config, [{Atoms, VTermInput, VTermOutput} | TestVectors]) ->
    InternalBinary = vterm_encode:internal_vterm_to_binary(VTermInput, #{allow_atom_cache_refs => true}),
    ?assertEqual(VTermOutput, erldist_filter_nif:dist_int_to_vterm(Atoms, InternalBinary)),
    dist_int_to_vterm_2(Config, TestVectors);
dist_int_to_vterm_2(_Config, []) ->
    ok.

dist_int_to_vterm_3() ->
    [
        {doc, "Loads the `erldist_filter_nif' and verifies that `dist_int_to_vterm/3' functions as expected."},
        {timetrap, {seconds, 60}}
    ].

dist_int_to_vterm_3(Config) ->
    dist_int_to_vterm_3(Config, vterm_test_vectors()).

%% @private
dist_int_to_vterm_3(Config, [{Atoms, VTermInput, VTermOutput} | TestVectors]) ->
    InternalBinary = vterm_encode:internal_vterm_to_binary(VTermInput, #{allow_atom_cache_refs => true}),
    ExpectedLazy0 = vedf:dist_int_to_vterm(Atoms, InternalBinary, 0),
    ExpectedLazy1 = vedf:dist_int_to_vterm(Atoms, InternalBinary, 1),
    ExpectedLazy2 = vedf:dist_int_to_vterm(Atoms, InternalBinary, 2),
    ?assertEqual(VTermOutput, erldist_filter_nif:dist_int_to_vterm(Atoms, InternalBinary, -1)),
    ?assertEqual(ExpectedLazy0, erldist_filter_nif:dist_int_to_vterm(Atoms, InternalBinary, 0)),
    ?assertEqual(ExpectedLazy1, erldist_filter_nif:dist_int_to_vterm(Atoms, InternalBinary, 1)),
    ?assertEqual(ExpectedLazy2, erldist_filter_nif:dist_int_to_vterm(Atoms, InternalBinary, 2)),
    dist_int_to_vterm_3(Config, TestVectors);
dist_int_to_vterm_3(_Config, []) ->
    ok.

prop_dist_ext_to_vdist_2() ->
    [
        {doc, "Loads the `erldist_filter_nif' and runs property tests for `dist_ext_to_vdist/2'."},
        {timetrap, {seconds, 600}}
    ].

prop_dist_ext_to_vdist_2(Config) ->
    erldist_filter_proper:quickcheck(erldist_filter_nif_prop:prop_dist_ext_to_vdist_2(Config), [
        verbose,
        {max_shrinks, 100},
        {numtests, 1000}
    ]).

prop_dist_ext_to_vterm_2() ->
    [
        {doc, "Loads the `erldist_filter_nif' and runs property tests for `dist_ext_to_vterm/2'."},
        {timetrap, {seconds, 600}}
    ].

prop_dist_ext_to_vterm_2(Config) ->
    erldist_filter_proper:quickcheck(erldist_filter_nif_prop:prop_dist_ext_to_vterm_2(Config), [
        verbose,
        {max_shrinks, 100},
        {numtests, 1000}
    ]).

prop_dist_ext_to_vterm_3() ->
    [
        {doc, "Loads the `erldist_filter_nif' and runs property tests for `dist_ext_to_vterm/3'."},
        {timetrap, {seconds, 600}}
    ].

prop_dist_ext_to_vterm_3(Config) ->
    erldist_filter_proper:quickcheck(erldist_filter_nif_prop:prop_dist_ext_to_vterm_3(Config), [
        verbose,
        {max_shrinks, 100},
        {numtests, 1000}
    ]).

prop_dist_int_to_vdist_2() ->
    [
        {doc, "Loads the `erldist_filter_nif' and runs property tests for `dist_int_to_vdist/2'."},
        {timetrap, {seconds, 600}}
    ].

prop_dist_int_to_vdist_2(Config) ->
    erldist_filter_proper:quickcheck(erldist_filter_nif_prop:prop_dist_int_to_vdist_2(Config), [
        verbose,
        {max_shrinks, 100},
        {numtests, 1000}
    ]).

prop_dist_int_to_vterm_2() ->
    [
        {doc, "Loads the `erldist_filter_nif' and runs property tests for `dist_int_to_vterm/2'."},
        {timetrap, {seconds, 60}}
    ].

prop_dist_int_to_vterm_2(Config) ->
    erldist_filter_proper:quickcheck(erldist_filter_nif_prop:prop_dist_int_to_vterm_2(Config), [
        verbose,
        {max_shrinks, 100},
        {numtests, 1000}
    ]).

prop_dist_int_to_vterm_3() ->
    [
        {doc, "Loads the `erldist_filter_nif' and runs property tests for `dist_int_to_vterm/3'."},
        {timetrap, {seconds, 60}}
    ].

prop_dist_int_to_vterm_3(Config) ->
    erldist_filter_proper:quickcheck(erldist_filter_nif_prop:prop_dist_int_to_vterm_3(Config), [
        verbose,
        {max_shrinks, 100},
        {numtests, 1000}
    ]).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
make_atom_i(Prefix, I) ->
    erlang:binary_to_atom(
        <<_/binary>> = unicode:characters_to_binary(lists:flatten(io_lib:format("~ts~w", [Prefix, I]))), unicode
    ).

%% @private
make_large_tuple_test_vector(N) ->
    FirstN = min(N, 255),
    LastN = N - FirstN,
    Atoms = erlang:list_to_tuple([make_atom_i(<<"atom">>, I) || I <- lists:seq(0, FirstN - 1)]),
    FirstHalf = [vterm_atom_cache_ref:new(I) || I <- lists:seq(0, FirstN - 1)],
    LastHalf = [vterm:expand(make_atom_i(<<"atom">>, FirstN + I)) || I <- lists:seq(0, LastN - 1)],
    N = length(FirstHalf) + length(LastHalf),
    Elements = FirstHalf ++ LastHalf,
    TV = {Atoms, vterm_large_tuple_ext:new(N, Elements)},
    TV.

%% @private
vdist_test_vectors() ->
    Unused = vterm_nil_ext:new(),
    NodeA = vterm_small_atom_utf8_ext:new(3, <<"a@a">>),
    NodeB = vterm_small_atom_utf8_ext:new(3, <<"b@b">>),
    GroupLeaderA = vterm_new_pid_ext:new(NodeA, 1, 0, 0),
    PidA = vterm_new_pid_ext:new(NodeA, 0, 0, 0),
    PidB = vterm_new_pid_ext:new(NodeB, 0, 0, 0),
    NameB = vterm_small_atom_utf8_ext:new(1, <<"b">>),
    NormalReason = vterm_small_atom_utf8_ext:new(6, <<"normal">>),
    KillReason = vterm_small_atom_utf8_ext:new(4, <<"kill">>),
    TraceToken = vterm_binary_ext:new(9, <<"seq_token">>),
    MonitorRefA = vterm_newer_reference_ext:new(5, NodeA, 0, [0, 0, 0, 0, 0]),
    ReqIdA = vterm_newer_reference_ext:new(5, NodeA, 0, [1, 0, 0, 0, 0]),
    Module = vterm_small_atom_utf8_ext:new(4, <<"erpc">>),
    Function = vterm_small_atom_utf8_ext:new(12, <<"execute_call">>),
    Arity = vterm_small_integer_ext:new(4),
    OptList = [vterm_small_atom_utf8_ext:new(4, <<"link">>)],
    Flags = vterm_small_integer_ext:new(1),
    ResultPidB = vterm_new_pid_ext:new(NodeB, 1, 0, 0),
    ResultFailed = vterm_small_atom_utf8_ext:new(6, <<"failed">>),
    AliasB = vterm_newer_reference_ext:new(5, NodeB, 0, [1, 0, 0, 0, 0]),
    Id = vterm_integer_ext:new(123456),
    erldist_filter_nif_prop:vdist_test_vectors_derive_atoms([
        % DOP_LINK
        vdist_dop_link:new(PidA, PidB),
        % DOP_SEND
        vdist_dop_send:new(Unused, PidB),
        % DOP_EXIT
        vdist_dop_exit:new(PidA, PidB, NormalReason),
        % DOP_UNLINK
        vdist_dop_unlink:new(PidA, PidB),
        % DOP_REG_SEND
        vdist_dop_reg_send:new(PidA, Unused, NameB),
        % DOP_GROUP_LEADER
        vdist_dop_group_leader:new(GroupLeaderA, PidB),
        % DOP_EXIT2
        vdist_dop_exit2:new(PidA, PidB, KillReason),
        % DOP_SEND_TT
        vdist_dop_send_tt:new(Unused, PidB, TraceToken),
        % DOP_EXIT_TT
        vdist_dop_exit_tt:new(PidA, PidB, TraceToken, NormalReason),
        % DOP_REG_SEND_TT
        vdist_dop_reg_send_tt:new(PidA, Unused, NameB, TraceToken),
        % DOP_EXIT2_TT
        vdist_dop_exit2_tt:new(PidA, PidB, KillReason, TraceToken),
        % DOP_MONITOR_P
        vdist_dop_monitor_p:new(PidA, PidB, MonitorRefA),
        vdist_dop_monitor_p:new(PidA, NameB, MonitorRefA),
        % DOP_DEMONITOR_P
        vdist_dop_demonitor_p:new(PidA, PidB, MonitorRefA),
        vdist_dop_demonitor_p:new(PidA, NameB, MonitorRefA),
        % DOP_MONITOR_P_EXIT
        vdist_dop_monitor_p_exit:new(PidB, PidA, MonitorRefA, NormalReason),
        vdist_dop_monitor_p_exit:new(NameB, PidA, MonitorRefA, NormalReason),
        % DOP_SEND_SENDER
        vdist_dop_send_sender:new(PidA, PidB),
        % DOP_SEND_SENDER_TT
        vdist_dop_send_sender_tt:new(PidA, PidB, TraceToken),
        % DOP_PAYLOAD_EXIT
        vdist_dop_payload_exit:new(PidA, PidB),
        % DOP_PAYLOAD_EXIT_TT
        vdist_dop_payload_exit_tt:new(PidA, PidB, TraceToken),
        % DOP_PAYLOAD_EXIT2
        vdist_dop_payload_exit2:new(PidA, PidB),
        % DOP_PAYLOAD_EXIT2_TT
        vdist_dop_payload_exit2_tt:new(PidA, PidB, TraceToken),
        % DOP_PAYLOAD_MONITOR_P_EXIT
        vdist_dop_payload_monitor_p_exit:new(PidB, PidA, MonitorRefA),
        vdist_dop_payload_monitor_p_exit:new(NameB, PidA, MonitorRefA),
        % DOP_SPAWN_REQUEST
        vdist_dop_spawn_request:new(ReqIdA, PidA, GroupLeaderA, Module, Function, Arity, OptList),
        % DOP_SPAWN_REQUEST_TT
        vdist_dop_spawn_request_tt:new(ReqIdA, PidA, GroupLeaderA, Module, Function, Arity, OptList, TraceToken),
        % DOP_SPAWN_REPLY
        vdist_dop_spawn_reply:new(ReqIdA, PidA, Flags, ResultPidB),
        vdist_dop_spawn_reply:new(ReqIdA, PidA, Flags, ResultFailed),
        % DOP_SPAWN_REPLY_TT
        vdist_dop_spawn_reply_tt:new(ReqIdA, PidA, Flags, ResultPidB, TraceToken),
        vdist_dop_spawn_reply_tt:new(ReqIdA, PidA, Flags, ResultFailed, TraceToken),
        % DOP_ALIAS_SEND
        vdist_dop_alias_send:new(PidA, AliasB),
        % DOP_ALIAS_SEND_TT
        vdist_dop_alias_send_tt:new(PidA, AliasB, TraceToken),
        % DOP_UNLINK_ID
        vdist_dop_unlink_id:new(Id, PidA, PidB),
        % DOP_UNLINK_ID_ACK
        vdist_dop_unlink_id_ack:new(Id, PidA, PidB)
    ]).

%% @private
vterm_test_vectors() ->
    LongL1Byte = binary:copy(<<"a">>, 255),
    LongL1Size = 255,
    LongL1Atom =
        'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
    LongU8Byte = binary:copy(<<"🟦"/utf8>>, 255),
    LongU8Size = 1020,
    LongU8Atom =
        '🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦🟦',
    Node = vterm_small_atom_utf8_ext:new(3, <<"a@a">>),
    A0 = vterm_atom_ext:new(1, <<"a">>),
    A1 = vterm_atom_ext:new(1, <<"b">>),
    A2 = vterm_atom_ext:new(1, <<"c">>),
    R0 = vterm_atom_cache_ref:new(0),
    R1 = vterm_atom_cache_ref:new(1),
    R2 = vterm_atom_cache_ref:new(2),
    erldist_filter_nif_prop:vterm_test_vectors_resolve_atoms([
        % SMALL_INTEGER_EXT
        {{}, vterm_small_integer_ext:new(0)},
        % INTEGER_EXT
        {{}, vterm_integer_ext:new(-1)},
        % FLOAT_EXT
        {{}, vterm_float_ext:new(erlang:iolist_to_binary(io_lib:format("~-31.20.*e", [0, 1.5])))},
        % ATOM_EXT
        {{}, vterm_atom_ext:new(0, <<>>)},
        {{}, vterm_atom_ext:new(1, <<"a">>)},
        {{}, vterm_atom_ext:new(LongL1Size, LongL1Byte)},
        % SMALL_ATOM_EXT
        {{}, vterm_small_atom_ext:new(0, <<>>)},
        {{}, vterm_small_atom_ext:new(1, <<"a">>)},
        % REFERENCE_EXT
        {{}, vterm_reference_ext:new(Node, 0, 1)},
        {{'b@b'}, vterm_reference_ext:new(R0, 0, 1)},
        % NEW_REFERENCE_EXT
        {{}, vterm_new_reference_ext:new(1, Node, 1, [0])},
        {{'b@b'}, vterm_new_reference_ext:new(1, R0, 1, [0])},
        % NEWER_REFERENCE_EXT
        {{}, vterm_newer_reference_ext:new(1, Node, 1, [0])},
        {{'b@b'}, vterm_newer_reference_ext:new(1, R0, 1, [0])},
        % PORT_EXT
        {{}, vterm_port_ext:new(Node, 0, 0)},
        {{'b@b'}, vterm_port_ext:new(R0, 0, 0)},
        % NEW_PORT_EXT
        {{}, vterm_new_port_ext:new(Node, 0, 0)},
        {{'b@b'}, vterm_new_port_ext:new(R0, 0, 0)},
        % NEW_FLOAT_EXT
        {{}, vterm_new_float_ext:new(<<(1.5):64/float>>)},
        % PID_EXT
        {{}, vterm_pid_ext:new(Node, 0, 0, 0)},
        {{'b@b'}, vterm_pid_ext:new(R0, 0, 0, 0)},
        % NEW_PID_EXT
        {{}, vterm_new_pid_ext:new(Node, 0, 0, 0)},
        {{'b@b'}, vterm_new_pid_ext:new(R0, 0, 0, 0)},
        % SMALL_TUPLE_EXT
        {{}, vterm_small_tuple_ext:new(0, [])},
        {{a, b, c}, vterm_small_tuple_ext:new(3, [R0, R1, R2])},
        % LARGE_TUPLE_EXT
        {{}, vterm_large_tuple_ext:new(0, [])},
        {{a, b, c}, vterm_large_tuple_ext:new(3, [R0, R1, R2])},
        make_large_tuple_test_vector(300),
        % NIL_EXT
        {{}, vterm_nil_ext:new()},
        % STRING_EXT
        {{}, vterm_string_ext:new(0, <<>>)},
        {{}, vterm_string_ext:new(3, <<"abc">>)},
        % LIST_EXT
        {{}, vterm_list_ext:new(0, [], vterm_nil_ext:new())},
        {{a, b, c}, vterm_list_ext:new(3, [R0, R1, R2], vterm_nil_ext:new())},
        {{a, b, c}, vterm_list_ext:new(2, [R0, R1], R2)},
        % BINARY_EXT
        {{}, vterm_binary_ext:new(0, <<>>)},
        {{}, vterm_binary_ext:new(3, <<"abc">>)},
        % BIT_BINARY_EXT
        {{}, vterm_bit_binary_ext:new(1, 1, <<0>>)},
        {{}, vterm_bit_binary_ext:new(3, 3, <<"abc">>)},
        % SMALL_BIG_EXT
        {{}, vterm_small_big_ext:new(4, 0, <<128:32>>)},
        % LARGE_BIG_EXT
        {{}, vterm_large_big_ext:new(256, 0, <<1:2048>>)},
        % NEW_FUN_EXT
        % EXPORT_EXT
        {{}, vterm_export_ext:new(A0, A1, vterm_small_integer_ext:new(1))},
        {{a, b}, vterm_export_ext:new(R0, R1, vterm_small_integer_ext:new(1))},
        % MAP_EXT
        {{}, vterm_map_ext:new(0, [])},
        {{}, vterm_map_ext:new(2, [{A0, vterm_nil_ext:new()}, {A1, A2}])},
        {{a, b, c}, vterm_map_ext:new(2, [{R0, vterm_nil_ext:new()}, {R1, R2}])},
        % FUN_EXT (deprecated, removed)
        % ATOM_UTF8_EXT
        {{}, vterm_atom_utf8_ext:new(0, <<>>)},
        {{}, vterm_atom_utf8_ext:new(2, <<"Ω"/utf8>>)},
        {{}, vterm_atom_utf8_ext:new(LongL1Size, LongL1Byte)},
        {{}, vterm_atom_utf8_ext:new(LongU8Size, LongU8Byte)},
        % SMALL_ATOM_UTF8_EXT
        {{}, vterm_small_atom_utf8_ext:new(0, <<>>)},
        {{}, vterm_small_atom_utf8_ext:new(2, <<"Ω"/utf8>>)},
        % V4_PORT_EXT
        {{}, vterm_v4_port_ext:new(Node, 0, 1)},
        {{'b@b'}, vterm_v4_port_ext:new(R0, 0, 1)},
        % ATOM_CACHE_REF
        {{a}, R0},
        {{'Ω'}, R0},
        {{LongL1Atom}, R0},
        {{LongU8Atom}, R0}
    ]).
