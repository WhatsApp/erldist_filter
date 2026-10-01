%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(erldist_filter_peer_priority_SUITE).
-moduledoc """
## Priority Signal Regression Tests for Peers Connected Through erldist_filter

### Test Cases

- **alias_priority_send_fragmented**: sends a priority message larger than the
  distribution fragment size to an `erlang:alias([priority])` alias, in both
  directions between `peer` nodes connected through `erldist_filter`. ERTS
  releases without OTP-20286 (fixed in ERTS 17.0.6/OTP 29.0.6 and ERTS
  16.4.0.6/OTP 28.5.0.6) read past a 40-byte message reference when they
  receive such a message; under ASan the receiving node aborts.
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2026-10-01", modified => "2026-10-01"}.
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
    alias_priority_send_fragmented/0,
    alias_priority_send_fragmented/1
]).

%% Macros
-define(PEER_SPBT_SHIM, erldist_filter_peer_spbt_shim).

%%%=============================================================================
%%% ct_suite callbacks
%%%=============================================================================

-spec all() -> erldist_filter_test:all().
all() ->
    [
        alias_priority_send_fragmented
    ].

-spec init_per_suite(Config :: ct_suite:ct_config()) -> erldist_filter_test:init_per_suite().
init_per_suite(Config) ->
    case erlang:list_to_integer(erlang:system_info(otp_release)) >= 28 of
        true ->
            {ok, _} = application:ensure_all_started(erldist_filter_test),
            Config;
        false ->
            {skip, "Priority messages require OTP 28 or later"}
    end.

-spec end_per_suite(Config :: ct_suite:ct_config()) -> erldist_filter_test:end_per_suite().
end_per_suite(_Config) ->
    ok.

-spec init_per_testcase(TestCase :: ct_suite:ct_testname(), Config :: ct_suite:ct_config()) ->
    erldist_filter_test:init_per_testcase().
init_per_testcase(alias_priority_send_fragmented, Config) ->
    P2P = erldist_filter_test_p2p:open(<<"edf-priority-fragmented">>),
    [{p2p, P2P} | Config].

-spec end_per_testcase(TestCase :: ct_suite:ct_testname(), Config :: ct_suite:ct_config()) ->
    erldist_filter_test:end_per_testcase().
end_per_testcase(_TestCase, Config) ->
    case lists:keyfind(p2p, 1, Config) of
        {p2p, P2P} when is_pid(P2P) ->
            ok = erldist_filter_test_p2p:close(P2P),
            ok
    end.

%%%=============================================================================
%%% Test Cases
%%%=============================================================================

-spec alias_priority_send_fragmented() -> erldist_filter_test:testcase_info().
alias_priority_send_fragmented() ->
    [
        {doc, "Fragmented priority message to a priority alias in both directions (OTP-20286)"},
        {timetrap, {seconds, 120}}
    ].

-spec alias_priority_send_fragmented(Config :: ct_suite:ct_config()) -> erldist_filter_test:testcase().
alias_priority_send_fragmented(Config) ->
    {p2p, P2P} = lists:keyfind(p2p, 1, Config),
    true = is_pid(P2P),
    ok = ?PEER_SPBT_SHIM:ensure_connected(P2P),
    % 1 MiB always spans several distribution fragments.
    Term = binary:copy(<<0>>, 1 bsl 20),
    ?assertEqual(pong, ?PEER_SPBT_SHIM:alias_priority_send(P2P, Term)),
    ?assert(?PEER_SPBT_SHIM:is_connected(P2P)),
    ok.
