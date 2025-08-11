%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% Created :  10 Aug 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
-module(erldist_filter_routers_sup).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(supervisor).

%% OTP callbacks
-export([
    child_spec/0,
    child_spec/1,
    child_specs/1,
    start_link/0,
    start_link/1,
    sup_flags/0
]).

%% supervisor callbacks
-export([
    init/1
]).

%% Macros
-define(is_router_count(X), (is_integer(X) andalso (X) >= 0)).

%% Types
-type router_count() :: non_neg_integer().

-export_type([
    router_count/0
]).

%%%=============================================================================
%%% OTP callbacks
%%%=============================================================================

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    #{count := RouterCount} = erldist_filter_nif:router_info(),
    child_spec(RouterCount).

-spec child_spec(RouterCount) -> supervisor:child_spec() when
    RouterCount :: router_count().
child_spec(RouterCount) when ?is_router_count(RouterCount) ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, [RouterCount]},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [?MODULE]
    }.

-spec child_specs(RouterCount) -> ChildSpecs when
    RouterCount :: router_count(),
    ChildSpecs :: [supervisor:child_spec()].
child_specs(RouterCount) when ?is_router_count(RouterCount) ->
    make_child_specs(RouterCount, []).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    #{count := RouterCount} = erldist_filter_nif:router_info(),
    start_link(RouterCount).

-spec start_link(RouterCount) -> supervisor:startlink_ret() when
    RouterCount :: router_count().
start_link(RouterCount) when ?is_router_count(RouterCount) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {RouterCount}).

-spec sup_flags() -> supervisor:sup_flags().
sup_flags() ->
    #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    }.

%%%=============================================================================
%%% supervisor callbacks
%%%=============================================================================

-spec init({RouterCount}) -> InitResult when
    RouterCount :: router_count(),
    InitResult :: {ok, {SupFlags, [ChildSpec]}} | ignore,
    SupFlags :: supervisor:sup_flags(),
    ChildSpec :: supervisor:child_spec().
init({RouterCount}) when ?is_router_count(RouterCount) ->
    SupFlags = sup_flags(),
    ChildSpecs = child_specs(RouterCount),
    {ok, {SupFlags, ChildSpecs}}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-spec make_child_specs(RouterCount, ChildSpecs) -> ChildSpecs when
    RouterCount :: router_count(),
    ChildSpecs :: [supervisor:child_spec()].
make_child_specs(0, ChildSpecs) ->
    ChildSpecs;
make_child_specs(RouterCount, ChildSpecs) when ?is_router_count(RouterCount) ->
    ChildSpec = erldist_filter_router_sup:child_spec(RouterCount),
    make_child_specs(RouterCount - 1, [ChildSpec | ChildSpecs]).
