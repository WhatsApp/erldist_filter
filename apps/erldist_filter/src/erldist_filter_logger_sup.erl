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
%%% Created :  10 May 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(erldist_filter_logger_sup).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(supervisor).

%% OTP callbacks
-export([
    child_spec/3,
    child_specs/3,
    start_link/3
]).

%% supervisor callbacks
-export([
    init/1
]).

%% Macros
-define(is_handler(X), is_atom(X)).
-define(is_worker_count(X), (is_integer(X) andalso (X) >= 0)).

%% Types
-type worker_count() :: non_neg_integer().

-export_type([
    worker_count/0
]).

%%%=============================================================================
%%% OTP callbacks
%%%=============================================================================

-spec child_spec(Handler, HandlerOptions, WorkerCount) -> supervisor:child_spec() when
    Handler :: erldist_filter_logger:handler(),
    HandlerOptions :: erldist_filter_logger:handler_options(),
    WorkerCount :: worker_count().
child_spec(Handler, HandlerOptions, WorkerCount) when ?is_handler(Handler) andalso ?is_worker_count(WorkerCount) ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, [Handler, HandlerOptions, WorkerCount]},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [?MODULE]
    }.

-spec child_specs(Handler, HandlerOptions, WorkerCount) -> ChildSpecs when
    Handler :: erldist_filter_logger:handler(),
    HandlerOptions :: erldist_filter_logger:handler_options(),
    WorkerCount :: worker_count(),
    ChildSpecs :: [supervisor:child_spec()].
child_specs(Handler, HandlerOptions, WorkerCount) when ?is_handler(Handler) andalso ?is_worker_count(WorkerCount) ->
    make_child_specs(Handler, HandlerOptions, WorkerCount, []).

-spec start_link(Handler, HandlerOptions, WorkerCount) -> supervisor:startlink_ret() when
    Handler :: erldist_filter_logger:handler(),
    HandlerOptions :: erldist_filter_logger:handler_options(),
    WorkerCount :: worker_count().
start_link(Handler, HandlerOptions, WorkerCount) when ?is_handler(Handler) andalso ?is_worker_count(WorkerCount) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {Handler, HandlerOptions, WorkerCount}).

%%%=============================================================================
%%% supervisor callbacks
%%%=============================================================================

-spec init({Handler, HandlerOptions, WorkerCount}) -> InitResult when
    Handler :: erldist_filter_logger:handler(),
    HandlerOptions :: erldist_filter_logger:handler_options(),
    WorkerCount :: worker_count(),
    InitResult :: {ok, {SupFlags, [ChildSpec]}} | ignore,
    SupFlags :: supervisor:sup_flags(),
    ChildSpec :: supervisor:child_spec().
init({Handler, HandlerOptions, WorkerCount}) when ?is_handler(Handler) andalso ?is_worker_count(WorkerCount) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },
    ChildSpecs = child_specs(Handler, HandlerOptions, WorkerCount),
    {ok, {SupFlags, ChildSpecs}}.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec make_child_specs(Handler, HandlerOptions, WorkerCount, ChildSpecs) -> ChildSpecs when
    Handler :: erldist_filter_logger:handler(),
    HandlerOptions :: erldist_filter_logger:handler_options(),
    WorkerCount :: worker_count(),
    ChildSpecs :: [supervisor:child_spec()].
make_child_specs(Handler, _HandlerOptions, 0, ChildSpecs) when ?is_handler(Handler) ->
    ChildSpecs;
make_child_specs(Handler, HandlerOptions, WorkerCount, ChildSpecs) when
    ?is_handler(Handler) andalso ?is_worker_count(WorkerCount)
->
    ChildSpec = erldist_filter_logger:child_spec(Handler, HandlerOptions, WorkerCount),
    make_child_specs(Handler, HandlerOptions, WorkerCount - 1, [ChildSpec | ChildSpecs]).
