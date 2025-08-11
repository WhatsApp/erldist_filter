%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% Created :  12 Oct 2022 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
-module(erldist_filter_proper).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(warn_missing_spec_all).

%% PropEr Helpers
-export([
    quickcheck/2,
    present_result/4
]).

-define(UNKNOWN, "unknown_property").

%%%=============================================================================
%%% PropEr Helpers
%%%=============================================================================

-spec present_result(module(), list(), tuple(), proplists:proplist()) -> boolean().
present_result(Module, Cmds, Triple, Config0) ->
    Config = [{property_test_tool, proper}] ++ Config0,
    ct_property_test:present_result(Module, Cmds, Triple, Config).

-spec quickcheck(proper:outer_test(), Options) -> true | {fail, Reason} when
    Options :: dynamic(),
    Reason :: dynamic().
quickcheck(OuterTest, Options0) ->
    {Store, Options1} =
        case lists:keytake(store, 1, Options0) of
            {value, StoreTuple = {store, _}, Opts1} ->
                {StoreTuple, Opts1};
            false ->
                {false, Options0}
        end,
    Options = [long_result, {to_file, user}] ++ Options1,
    Result =
        try proper:quickcheck(OuterTest, Options) of
            true -> true;
            {error, Reason} -> #{failure => Reason};
            [CounterExample] -> #{counterexample => CounterExample};
            CounterExamples -> #{counterexamples => CounterExamples}
        catch
            Class:Reason:Stacktrace -> #{error => {Class, Reason, Stacktrace}}
        end,
    case Result of
        true ->
            true;
        _ ->
            case Store of
                {store, StoreFilename} ->
                    {ok, IoDevice} = file:open(StoreFilename, [write, {encoding, utf8}]),
                    io:format(IoDevice, "~p.~n", [Result]),
                    _ = file:close(IoDevice),
                    ok;
                false ->
                    ok
            end,
            case Result of
                #{error := {C, R, S}} ->
                    erlang:raise(C, R, S);
                Other ->
                    {fail, {property_name(OuterTest), Other}}
            end
    end.

-spec property_name(dynamic()) -> string().
property_name({forall, _Generator, PropFun}) ->
    extract_name_from_fun(PropFun);
property_name({setup, _SetupFun, Prop}) ->
    property_name(Prop);
property_name({exists, _Generator, PropFun, _Not}) ->
    extract_name_from_fun(PropFun);
property_name(_) ->
    ?UNKNOWN.

-spec extract_name_from_fun(fun()) -> string().
extract_name_from_fun(Fun) ->
    FInfo = erlang:fun_info(Fun),
    case proplists:get_value(name, FInfo) of
        undefined ->
            ?UNKNOWN;
        FullName ->
            L = erlang:atom_to_list(FullName),
            case string:split(L, "/") of
                [L] -> L;
                [[$- | EncfunName], _] -> EncfunName;
                [EncfunName, _] -> EncfunName
            end
    end.
