%%%-----------------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@meta.com>
%%% @copyright (C) 2022, WhatsApp, Inc
%%% @doc A test suite designed to load the `erldist_filter_nif' and do nothing
%%% else.
%%%
%%% @end
%%% Created :  16 Dec 2022 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(erldist_filter_noop_SUITE).
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
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test Cases
-export([
    test_that_the_nif_loads/0,
    test_that_the_nif_loads/1
]).

%%%=============================================================================
%%% ct callbacks
%%%=============================================================================

all() ->
    [
        {group, noop}
    ].

groups() ->
    [
        {noop, [parallel], [
            test_that_the_nif_loads
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

test_that_the_nif_loads() ->
    [
        {doc, "Loads the `erldist_filter_nif' and verifies that `distribution_flags/0' returns a map."},
        {timetrap, {seconds, 60}}
    ].

test_that_the_nif_loads(_Config) ->
    ?assert(is_map(erldist_filter_nif:distribution_flags())).
