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
-module(erldist_filter_handler_sup).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(supervisor).

%% OTP callbacks
-export([
    child_name/1,
    child_spec/1,
    child_specs/1,
    start_link/2,
    sup_flags/0
]).

%% supervisor callbacks
-export([
    init/1
]).

%% Macros
-define(is_router_number(X), (is_integer(X) andalso (X) >= 1)).

%% Types
-type router_number() :: pos_integer().

-export_type([
    router_number/0
]).

%%%=============================================================================
%%% OTP callbacks
%%%=============================================================================

-spec child_name(RouterNumber) -> atom() when RouterNumber :: router_number().
child_name(RouterNumber) when ?is_router_number(RouterNumber) ->
    erlang:binary_to_atom(
        <<"erldist_filter_handler_", (erlang:integer_to_binary(RouterNumber))/bytes, "_sup">>,
        utf8
    ).

-spec child_spec(RouterNumber) -> supervisor:child_spec() when
    RouterNumber :: router_number().
child_spec(RouterNumber) when ?is_router_number(RouterNumber) ->
    ChildName = child_name(RouterNumber),
    #{
        id => ChildName,
        start => {?MODULE, start_link, [{local, ChildName}, RouterNumber]},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [?MODULE]
    }.

-spec child_specs(RouterNumber) -> ChildSpecs when
    RouterNumber :: router_number(),
    ChildSpecs :: [supervisor:child_spec()].
child_specs(RouterNumber) when ?is_router_number(RouterNumber) ->
    [
        erldist_filter_handler:child_spec(RouterNumber)
    ].

-spec start_link(ServerName, RouterNumber) -> supervisor:startlink_ret() when
    ServerName :: gen_statem:server_name(),
    RouterNumber :: router_number().
start_link(ServerName, RouterNumber) when ?is_router_number(RouterNumber) ->
    supervisor:start_link(ServerName, ?MODULE, {RouterNumber}).

-spec sup_flags() -> supervisor:sup_flags().
sup_flags() ->
    #{
        strategy => simple_one_for_one,
        intensity => 0,
        period => 1
    }.

%%%=============================================================================
%%% supervisor callbacks
%%%=============================================================================

-spec init({RouterNumber}) -> InitResult when
    RouterNumber :: router_number(),
    InitResult :: {ok, {SupFlags, [ChildSpec]}} | ignore,
    SupFlags :: supervisor:sup_flags(),
    ChildSpec :: supervisor:child_spec().
init({RouterNumber}) when ?is_router_number(RouterNumber) ->
    SupFlags = sup_flags(),
    ChildSpecs = child_specs(RouterNumber),
    {ok, {SupFlags, ChildSpecs}}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
