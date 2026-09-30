%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(erldist_filter_channel_teardown_SUITE).
-moduledoc """
# Erlang Distribution Filter Channel Teardown Test Suite

Regression tests for ownership of receive-path externals when a channel is torn
down while a `channel_recv/2` receive trap is suspended, closed, or failed.

The receive trap runs in the channel owner process. When the owner dies, the
channel's resource monitor down callback destroys the channel synchronously in
the exiting process; the suspended receive trap is only destroyed later, when
ERTS releases the process's references. Any external that both the channel and
the trap believe they own is then destroyed twice (heap-use-after-free under
AddressSanitizer).

The cases use test-only NIF instrumentation (`erldist_filter_nif:test_hook_*`,
built with `EDF_TEST_HOOKS=1`) to make the ordering deterministic:

- A receive barrier makes the trap yield (releasing the channel lock) right
  after a given fragment has been received, so the owner can be killed while
  the trap is suspended. The barrier never blocks.
- Every teardown event (`recv_barrier`, `owner_down`, `channel_destroy`,
  `recv_trap_error`, `recv_trap_dtor`) is written to stderr and sent to the test
  process with a global sequence number, so the observed order is asserted.

The suite is skipped when the NIF is built without test hooks, unless
`ERLDIST_FILTER_REQUIRE_TEST_HOOKS=1` (set by `justfiles/sanitizers/run.sh`).
""".
-moduledoc #{created => "2026-09-30", modified => "2026-09-30"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("stdlib/include/assert.hrl").

-behaviour(ct_suite).

%% ct_suite callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test Cases
-export([
    receiver_death_during_fragment_header/0,
    receiver_death_during_fragment_header/1,
    receiver_death_during_fragment_continuation/0,
    receiver_death_during_fragment_continuation/1,
    receiver_death_during_unfragmented_message/0,
    receiver_death_during_unfragmented_message/1,
    receiver_death_with_parked_fragments/0,
    receiver_death_with_parked_fragments/1,
    channel_close_with_parked_fragments/0,
    channel_close_with_parked_fragments/1,
    normal_completion_through_barrier/0,
    normal_completion_through_barrier/1,
    error_cleanup_out_of_order_fragment/0,
    error_cleanup_out_of_order_fragment/1,
    error_cleanup_duplicate_fragment_header/0,
    error_cleanup_duplicate_fragment_header/1
]).

%% Macros
-define(PACKET_SIZE, 4).
-define(TIMEOUT, 10000).
-define(DIST_FRAG_HEADER, 69).

%%%=============================================================================
%%% ct_suite callbacks
%%%=============================================================================

-spec all() -> erldist_filter_test:all().
all() ->
    % Serial on purpose: the test hook is global to the NIF.
    [
        receiver_death_during_fragment_header,
        receiver_death_during_fragment_continuation,
        receiver_death_during_unfragmented_message,
        receiver_death_with_parked_fragments,
        channel_close_with_parked_fragments,
        normal_completion_through_barrier,
        error_cleanup_out_of_order_fragment,
        error_cleanup_duplicate_fragment_header
    ].

-spec init_per_suite(Config :: ct_suite:ct_config()) -> erldist_filter_test:init_per_suite().
init_per_suite(Config) ->
    case os:getenv("ERLDIST_FILTER_REQUIRE_ASAN") of
        "1" -> ?assertEqual(asan, erlang:system_info(emu_type));
        _ -> ok
    end,
    RequireTestHooks = (os:getenv("ERLDIST_FILTER_REQUIRE_TEST_HOOKS") =:= "1"),
    try erldist_filter_nif:test_hook_disarm() of
        ok ->
            ct:pal("emu_type=~0p, NIF test hooks are available", [erlang:system_info(emu_type)]),
            Config
    catch
        error:{nif_not_loaded, erldist_filter_nif} when RequireTestHooks =:= true ->
            ct:fail("erldist_filter_nif was built without EDF_TEST_HOOKS=1, but ERLDIST_FILTER_REQUIRE_TEST_HOOKS=1");
        error:{nif_not_loaded, erldist_filter_nif} ->
            {skip, "erldist_filter_nif was built without EDF_TEST_HOOKS=1"}
    end.

-spec end_per_suite(Config :: ct_suite:ct_config()) -> erldist_filter_test:end_per_suite().
end_per_suite(_Config) ->
    ok.

-spec init_per_testcase(TestCase :: ct_suite:ct_testname(), Config :: ct_suite:ct_config()) ->
    erldist_filter_test:init_per_testcase().
init_per_testcase(_TestCase, Config) ->
    ok = erldist_filter_nif:test_hook_disarm(),
    _ = flush_events(),
    Config.

-spec end_per_testcase(TestCase :: ct_suite:ct_testname(), Config :: ct_suite:ct_config()) ->
    erldist_filter_test:end_per_testcase().
end_per_testcase(_TestCase, _Config) ->
    % Disarming also releases a receive trap that is still spinning on the barrier.
    ok = erldist_filter_nif:test_hook_disarm(),
    _ = flush_events(),
    ok.

%%%=============================================================================
%%% Test Cases
%%%=============================================================================

-spec receiver_death_during_fragment_header() -> erldist_filter_test:testcase_info().
receiver_death_during_fragment_header() ->
    [
        {doc, "Owner dies while the receive trap is suspended right after linking a new fragmented external"},
        {timetrap, {seconds, 60}}
    ].

-spec receiver_death_during_fragment_header(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
receiver_death_during_fragment_header(_Config) ->
    {PHead, _PTail, FragmentCount, _Actions} = fragmented_message(),
    {Owner, MonitorRef, Channel} = start_owner(),
    ok = erldist_filter_nif:test_hook_arm(self(), Channel, FragmentCount),
    _Tag = owner_recv_async(Owner, split_packet(PHead)),
    {BarrierSeq, #{trap := Trap, fragment_id := FragmentCount, linked_in_rx_sequences := Linked}} = await_event(
        recv_barrier
    ),
    ct:pal("receive trap suspended after DIST_FRAG_HEADER (FragmentCount=~b, linked_in_rx_sequences=~0p)", [
        FragmentCount, Linked
    ]),
    kill_owner_and_await_teardown(Owner, MonitorRef, BarrierSeq, Trap).

-spec receiver_death_during_fragment_continuation() -> erldist_filter_test:testcase_info().
receiver_death_during_fragment_continuation() ->
    [
        {doc, "Owner dies while the receive trap is suspended right after adding a continuation fragment"},
        {timetrap, {seconds, 60}}
    ].

-spec receiver_death_during_fragment_continuation(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
receiver_death_during_fragment_continuation(_Config) ->
    {PHead, [PCont | _], FragmentCount, _Actions} = fragmented_message(),
    {Owner, MonitorRef, Channel} = start_owner(),
    ok = erldist_filter_nif:test_hook_arm(self(), Channel, FragmentCount - 1),
    % The header fragment is received and parked without hitting the barrier. Replies may carry the atom cache
    % commit emitted for the header fragment, so the parked-fragment receives only check the reply shape.
    ?assertMatch({ok, _}, owner_recv(Owner, split_packet(PHead))),
    _Tag = owner_recv_async(Owner, split_packet(PCont)),
    ExpectedFragmentId = FragmentCount - 1,
    {BarrierSeq, #{trap := Trap, fragment_id := ExpectedFragmentId, linked_in_rx_sequences := Linked}} = await_event(
        recv_barrier
    ),
    ct:pal("receive trap suspended after DIST_FRAG_CONT (FragmentId=~b, linked_in_rx_sequences=~0p)", [
        ExpectedFragmentId, Linked
    ]),
    kill_owner_and_await_teardown(Owner, MonitorRef, BarrierSeq, Trap).

-spec receiver_death_during_unfragmented_message() -> erldist_filter_test:testcase_info().
receiver_death_during_unfragmented_message() ->
    [
        {doc, "Owner dies while the receive trap is suspended on an unfragmented (DIST_HEADER) external"},
        {timetrap, {seconds, 60}}
    ].

-spec receiver_death_during_unfragmented_message(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
receiver_death_during_unfragmented_message(_Config) ->
    {[Packet], _Actions} = unfragmented_message(),
    {Owner, MonitorRef, Channel} = start_owner(),
    ok = erldist_filter_nif:test_hook_arm(self(), Channel, 1),
    % Splitting the packet forces the receive trap instead of the single-binary fast path.
    _Tag = owner_recv_async(Owner, split_packet(Packet)),
    {BarrierSeq, #{trap := Trap, fragment_id := 1, fragment_count := 1, linked_in_rx_sequences := false}} = await_event(
        recv_barrier
    ),
    kill_owner_and_await_teardown(Owner, MonitorRef, BarrierSeq, Trap).

-spec receiver_death_with_parked_fragments() -> erldist_filter_test:testcase_info().
receiver_death_with_parked_fragments() ->
    [
        {doc, "Owner dies with an incomplete fragmented message parked on the channel and no receive in flight"},
        {timetrap, {seconds, 60}}
    ].

-spec receiver_death_with_parked_fragments(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
receiver_death_with_parked_fragments(_Config) ->
    {PHead, [PCont | _], _FragmentCount, _Actions} = fragmented_message(),
    {Owner, MonitorRef, Channel} = start_owner(),
    ok = erldist_filter_nif:test_hook_arm(self(), Channel, 0),
    ?assertMatch({ok, _}, owner_recv(Owner, [PHead])),
    ?assertMatch({ok, _}, owner_recv(Owner, [PCont])),
    true = exit(Owner, kill),
    ok = await_down(Owner, MonitorRef, killed),
    {OwnerDownSeq, _} = await_event(owner_down),
    {ChannelDestroySeq, _} = await_event(channel_destroy),
    ?assert(OwnerDownSeq < ChannelDestroySeq),
    ok.

-spec channel_close_with_parked_fragments() -> erldist_filter_test:testcase_info().
channel_close_with_parked_fragments() ->
    [
        {doc, "channel_close/1 with an incomplete fragmented message parked on the channel"},
        {timetrap, {seconds, 60}}
    ].

-spec channel_close_with_parked_fragments(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
channel_close_with_parked_fragments(_Config) ->
    {PHead, [PCont | _], _FragmentCount, _Actions} = fragmented_message(),
    {Owner, MonitorRef, Channel} = start_owner(),
    ok = erldist_filter_nif:test_hook_arm(self(), Channel, 0),
    ?assertMatch({ok, _}, owner_recv(Owner, split_packet(PHead))),
    ?assertMatch({ok, _}, owner_recv(Owner, split_packet(PCont))),
    ?assertEqual(ok, owner_close(Owner)),
    {_, _} = await_event(channel_destroy),
    ?assertMatch({error, _}, owner_recv(Owner, [PCont])),
    ok = stop_owner(Owner, MonitorRef).

-spec normal_completion_through_barrier() -> erldist_filter_test:testcase_info().
normal_completion_through_barrier() ->
    [
        {doc, "A fragmented message suspended at the barrier after its last fragment completes normally once opened"},
        {timetrap, {seconds, 60}}
    ].

-spec normal_completion_through_barrier(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
normal_completion_through_barrier(_Config) ->
    {PHead, PTail, FragmentCount, Actions} = fragmented_message(),
    {Owner, MonitorRef, Channel} = start_owner(),
    ok = erldist_filter_nif:test_hook_arm(self(), Channel, 1),
    Tag = owner_recv_async(Owner, [PHead | PTail]),
    {_, #{fragment_id := 1, fragment_count := FragmentCount}} = await_event(recv_barrier),
    ok = erldist_filter_nif:test_hook_open(),
    ?assertEqual({ok, Actions}, await_reply(Tag)),
    % The channel is still usable afterwards (the atom cache may now differ, so only the shape is checked).
    ok = erldist_filter_nif:test_hook_disarm(),
    ?assertMatch({ok, [_ | _]}, owner_recv(Owner, [PHead | PTail])),
    ?assertEqual(ok, owner_close(Owner)),
    ok = stop_owner(Owner, MonitorRef).

-spec error_cleanup_out_of_order_fragment() -> erldist_filter_test:testcase_info().
error_cleanup_out_of_order_fragment() ->
    [
        {doc, "An out-of-order continuation fragment raises and closes the channel with the sequence parked"},
        {timetrap, {seconds, 60}}
    ].

-spec error_cleanup_out_of_order_fragment(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
error_cleanup_out_of_order_fragment(_Config) ->
    {PHead, [PCont, PSkipped | _], _FragmentCount, _Actions} = fragmented_message(),
    {Owner, MonitorRef, Channel} = start_owner(),
    ok = erldist_filter_nif:test_hook_arm(self(), Channel, 0),
    ?assertMatch({ok, _}, owner_recv(Owner, split_packet(PHead))),
    ?assertMatch({error, _}, owner_recv(Owner, split_packet(PSkipped))),
    {ErrorSeq, _} = await_event(recv_trap_error),
    {ChannelDestroySeq, _} = await_event(channel_destroy),
    ?assert(ErrorSeq < ChannelDestroySeq),
    ?assertMatch({error, _}, owner_recv(Owner, split_packet(PCont))),
    ok = stop_owner(Owner, MonitorRef).

-spec error_cleanup_duplicate_fragment_header() -> erldist_filter_test:testcase_info().
error_cleanup_duplicate_fragment_header() ->
    [
        {doc, "A duplicate DIST_FRAG_HEADER for a parked sequence raises and closes the channel"},
        {timetrap, {seconds, 60}}
    ].

-spec error_cleanup_duplicate_fragment_header(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
error_cleanup_duplicate_fragment_header(_Config) ->
    {PHead, _PTail, _FragmentCount, _Actions} = fragmented_message(),
    {Owner, MonitorRef, Channel} = start_owner(),
    ok = erldist_filter_nif:test_hook_arm(self(), Channel, 0),
    ?assertMatch({ok, _}, owner_recv(Owner, split_packet(PHead))),
    ?assertMatch({error, _}, owner_recv(Owner, split_packet(PHead))),
    {ErrorSeq, _} = await_event(recv_trap_error),
    {ChannelDestroySeq, _} = await_event(channel_destroy),
    ?assert(ErrorSeq < ChannelDestroySeq),
    ok = stop_owner(Owner, MonitorRef).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-spec dflags() -> vterm:u64().
dflags() ->
    maps:get('DFLAG_DIST_DEFAULT', erldist_filter_nif:distribution_flags()).

-spec fragmented_message() -> {PHead, PTail, FragmentCount, Actions} when
    PHead :: binary(),
    PTail :: [binary()],
    FragmentCount :: pos_integer(),
    Actions :: [erldist_filter_nif:action()].
fragmented_message() ->
    C0 = vedf_channel:new(?PACKET_SIZE, dflags()),
    {ControlMessage, _} = vdist_entry:reg_send_noop(0, 0, 0),
    Payload = vterm:expand({binary:copy(<<"a">>, 1024)}),
    {ok, Packets = [PHead | PTail], C0} = vedf_channel:send_encode(
        C0, ControlMessage, Payload, #{header_mode => fragment, fragment_size => 16#7F}
    ),
    <<_:?PACKET_SIZE/unit:8, 131, ?DIST_FRAG_HEADER, _SequenceId:64, FragmentCount:64, _/binary>> = PHead,
    ?assert(FragmentCount >= 3),
    ?assertEqual(FragmentCount, length(Packets)),
    {ok, Actions, _C1} = vedf_channel:recv(C0, Packets),
    {PHead, PTail, FragmentCount, Actions}.

-spec unfragmented_message() -> {Packets, Actions} when
    Packets :: [binary()],
    Actions :: [erldist_filter_nif:action()].
unfragmented_message() ->
    C0 = vedf_channel:new(?PACKET_SIZE, dflags()),
    {ControlMessage, _} = vdist_entry:reg_send_noop(0, 0, 0),
    Payload = vterm:expand({binary:copy(<<"a">>, 1024)}),
    {ok, Packets, C0} = vedf_channel:send_encode(C0, ControlMessage, Payload, #{header_mode => normal}),
    {ok, Actions, _C1} = vedf_channel:recv(C0, Packets),
    {Packets, Actions}.

-spec split_packet(Packet :: binary()) -> [binary()].
split_packet(Packet) when byte_size(Packet) >= 2 ->
    [binary:part(Packet, 0, 1), binary:part(Packet, 1, byte_size(Packet) - 1)].

-spec start_owner() -> {Owner, MonitorRef, Channel} when
    Owner :: pid(), MonitorRef :: reference(), Channel :: erldist_filter_nif:channel().
start_owner() ->
    Parent = self(),
    {Owner, MonitorRef} = spawn_monitor(fun() -> owner_init(Parent) end),
    receive
        {Owner, channel, Channel} ->
            {Owner, MonitorRef, Channel};
        {'DOWN', MonitorRef, process, Owner, Reason} ->
            ct:fail({owner_start_failed, Reason})
    after ?TIMEOUT ->
        ct:fail(owner_start_timeout)
    end.

-spec owner_init(Parent :: pid()) -> ok.
owner_init(Parent) ->
    Channel = erldist_filter_nif:channel_open(?PACKET_SIZE, 'nonode@nohost', 0, 0, dflags()),
    Parent ! {self(), channel, Channel},
    owner_loop(Channel).

-spec owner_loop(Channel :: erldist_filter_nif:channel()) -> ok.
owner_loop(Channel) ->
    receive
        {recv, From, Tag, Packets} ->
            Reply =
                try erldist_filter_nif:channel_recv(Channel, Packets) of
                    Actions -> {ok, Actions}
                catch
                    Class:Reason -> {Class, Reason}
                end,
            From ! {Tag, Reply},
            owner_loop(Channel);
        {close, From, Tag} ->
            Reply =
                try
                    erldist_filter_nif:channel_close(Channel)
                catch
                    Class:Reason -> {Class, Reason}
                end,
            From ! {Tag, Reply},
            owner_loop(Channel);
        stop ->
            ok
    end.

-spec owner_recv_async(Owner :: pid(), Packets :: [binary()]) -> Tag :: reference().
owner_recv_async(Owner, Packets) ->
    Tag = make_ref(),
    Owner ! {recv, self(), Tag, Packets},
    Tag.

-spec owner_recv(Owner :: pid(), Packets :: [binary()]) -> term().
owner_recv(Owner, Packets) ->
    await_reply(owner_recv_async(Owner, Packets)).

-spec owner_close(Owner :: pid()) -> term().
owner_close(Owner) ->
    Tag = make_ref(),
    Owner ! {close, self(), Tag},
    await_reply(Tag).

-spec await_reply(Tag :: reference()) -> term().
await_reply(Tag) ->
    receive
        {Tag, Reply} -> Reply
    after ?TIMEOUT ->
        ct:fail({reply_timeout, flush_events()})
    end.

-spec stop_owner(Owner :: pid(), MonitorRef :: reference()) -> ok.
stop_owner(Owner, MonitorRef) ->
    Owner ! stop,
    await_down(Owner, MonitorRef, normal).

-spec await_down(Owner :: pid(), MonitorRef :: reference(), ExpectedReason :: term()) -> ok.
await_down(Owner, MonitorRef, ExpectedReason) ->
    receive
        {'DOWN', MonitorRef, process, Owner, Reason} ->
            ?assertEqual(ExpectedReason, Reason),
            ok
    after ?TIMEOUT ->
        ct:fail({owner_down_timeout, Owner})
    end.

-doc """
Kills the owner while its receive trap is suspended at the barrier and asserts the
teardown order: the monitor down callback destroys the channel, and only later is
the suspended receive trap (the one that hit the barrier) destroyed.
""".
-spec kill_owner_and_await_teardown(Owner, MonitorRef, BarrierSeq, Trap) -> ok when
    Owner :: pid(), MonitorRef :: reference(), BarrierSeq :: pos_integer(), Trap :: non_neg_integer().
kill_owner_and_await_teardown(Owner, MonitorRef, BarrierSeq, Trap) ->
    true = exit(Owner, kill),
    ok = await_down(Owner, MonitorRef, killed),
    {OwnerDownSeq, _} = await_event(owner_down),
    {ChannelDestroySeq, _} = await_event(channel_destroy),
    {TrapDtorSeq, TrapDtorDetails} = await_event(recv_trap_dtor, fun(#{trap := T}) -> T =:= Trap end),
    ct:pal("teardown order: recv_barrier=~b < owner_down=~b < channel_destroy=~b < recv_trap_dtor=~b (~0p)", [
        BarrierSeq, OwnerDownSeq, ChannelDestroySeq, TrapDtorSeq, TrapDtorDetails
    ]),
    ?assert(BarrierSeq < OwnerDownSeq),
    ?assert(OwnerDownSeq < ChannelDestroySeq),
    ?assert(ChannelDestroySeq < TrapDtorSeq),
    ok.

-spec await_event(Name :: atom()) -> {Seq :: pos_integer(), Details :: map()}.
await_event(Name) ->
    await_event(Name, fun(_) -> true end).

-spec await_event(Name :: atom(), Filter :: fun((map()) -> boolean())) -> {Seq :: pos_integer(), Details :: map()}.
await_event(Name, Filter) ->
    receive
        {erldist_filter_test_hook, Seq, Name, Details} when is_integer(Seq) andalso is_map(Details) ->
            case Filter(Details) of
                true ->
                    ct:pal("test hook event ~b: ~ts ~0p", [Seq, Name, Details]),
                    {Seq, Details};
                false ->
                    ct:pal("test hook event ~b: ~ts ~0p (ignored)", [Seq, Name, Details]),
                    await_event(Name, Filter)
            end
    after ?TIMEOUT ->
        ct:fail({test_hook_event_timeout, Name, flush_events()})
    end.

-spec flush_events() -> [term()].
flush_events() ->
    receive
        Event = {erldist_filter_test_hook, _, _, _} -> [Event | flush_events()]
    after 0 ->
        []
    end.
