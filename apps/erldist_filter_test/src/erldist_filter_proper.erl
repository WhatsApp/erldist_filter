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
%%% Created :  12 Oct 2022 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(erldist_filter_proper).
% proper
-eqwalizer(ignore).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(nowarn_missing_spec).

%% PropEr Helpers
-export([
    quickcheck/4,
    quickcheck/1
]).

%% Macros
-define(PROPER_QUICKCHECK_OPTIONS, '$proper_quickcheck_options').

%%%=============================================================================
%%% PropEr Helpers
%%%=============================================================================

quickcheck(Module, Function, Config0, Options) ->
    _ = erlang:put(?PROPER_QUICKCHECK_OPTIONS, {Module, Function, Config0, Options}),
    Config1 = lists:keystore(property_test_tool, 1, Config0, {property_test_tool, ?MODULE}),
    ct_property_test:quickcheck(undefined, Config1).

quickcheck(undefined) ->
    {Module, Function, Config, Options0} = erlang:erase(?PROPER_QUICKCHECK_OPTIONS),
    {Store, Options1} =
        case lists:keytake(store, 1, Options0) of
            {value, StoreTuple = {store, _}, Opts1} ->
                {StoreTuple, Opts1};
            false ->
                {false, Options0}
        end,
    OuterTest = Module:Function(Config),
    Result =
        try proper:quickcheck(OuterTest, Options1) of
            true ->
                true;
            FailureResult ->
                #{failure => FailureResult, counterexample => proper:counterexample()}
        catch
            Class:Reason:Stacktrace ->
                #{error => {Class, Reason, Stacktrace}, counterexample => proper:counterexample()}
        end,
    case Result of
        true ->
            true;
        _ ->
            ok =
                case Store of
                    {store, StoreFilename} ->
                        {ok, IoDevice} = file:open(StoreFilename, [write, {encoding, utf8}]),
                        io:format(IoDevice, "~p.~n", [{Module, Function, Result}]),
                        _ = file:close(IoDevice),
                        ok;
                    false ->
                        ok
                end,
            case Result of
                #{error := {C, R, S}} ->
                    erlang:raise(C, R, S);
                #{failure := Failure} ->
                    Failure
            end
    end.
