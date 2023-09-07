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
%%% Created :  10 Aug 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(erldist_filter_sup).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(supervisor).

%% OTP callbacks
-export([
    child_spec/0,
    child_specs/0,
    start_link/0,
    sup_flags/0
]).

%% supervisor callbacks
-export([
    init/1
]).

%%%=============================================================================
%%% OTP callbacks
%%%=============================================================================

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [?MODULE]
    }.

-spec child_specs() -> ChildSpecs when
    ChildSpecs :: [supervisor:child_spec()].
child_specs() ->
    [
        erldist_filter_routers_sup:child_spec()
    ].

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

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

-spec init({}) -> InitResult when
    InitResult :: {ok, {SupFlags, [ChildSpec]}} | ignore,
    SupFlags :: supervisor:sup_flags(),
    ChildSpec :: supervisor:child_spec().
init({}) ->
    SupFlags = sup_flags(),
    ChildSpecs = child_specs(),
    {ok, {SupFlags, ChildSpecs}}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
