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
%%% Created :  22 Sep 2022 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(erldist_filter_peer_spbt_sup).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(warn_missing_spec).

-behaviour(supervisor).

%% Public API
-export([
    child_spec/0,
    start_child/1,
    stop_child/1,
    stop_all_children/0,
    start_link/0,
    stop/0
]).
%% supervisor callbacks
-export([init/1]).

%%%=============================================================================
%%% Public API
%%%=============================================================================

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => supervisor,
        modules => [?MODULE]
    }.

-spec start_child(supervisor:child_spec()) -> supervisor:startchild_ret().
start_child(ChildSpec) ->
    supervisor:start_child(?MODULE, ChildSpec).

-spec stop_child(term()) -> ok.
stop_child(ChildId) ->
    _ = catch supervisor:terminate_child(?MODULE, ChildId),
    _ = catch supervisor:delete_child(?MODULE, ChildId),
    ok.

-spec stop_all_children() -> ok.
stop_all_children() ->
    _ = [
        stop_child(ChildId)
     || {ChildId, _ChildPid, _Type, _Modules} <- supervisor:which_children(?MODULE)
    ],
    ok.

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop() -> ok.
stop() ->
    case erlang:whereis(?MODULE) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            Monitor = erlang:monitor(process, Pid),
            true = erlang:unlink(Pid),
            true = erlang:exit(Pid, shutdown),
            receive
                {'DOWN', Monitor, process, Pid, _Reason} ->
                    ok
            after 1000 ->
                true = erlang:exit(Pid, kill),
                receive
                    {'DOWN', Monitor, process, Pid, _Reason} ->
                        ok
                end
            end
    end.

%%%=============================================================================
%%% supervisor callbacks
%%%=============================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    ChildSpecs = [],
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1
    },
    {ok, {SupFlags, ChildSpecs}}.
