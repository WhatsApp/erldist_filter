%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% Created :  17 Jun 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
-module(edf).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

%% Internal API
-export([
    req/3
]).

%%%=============================================================================
%%% Internal API functions
%%%=============================================================================

-spec req(Sysname, {Module, FunctionName, Arity}, Arguments) -> no_return() | dynamic() when
    Sysname :: node(),
    Module :: module(),
    FunctionName :: atom(),
    Arity :: non_neg_integer(),
    Arguments :: [dynamic()].
req(Sysname, {Module, FunctionName, Arity}, Arguments) when length(Arguments) =:= Arity ->
    case erldist_filter:handler_get() of
        undefined ->
            erlang:apply(Module, FunctionName, Arguments);
        Handler when is_atom(Handler) ->
            Handler:spawn_request_init(Sysname, Module, FunctionName, Arguments)
    end.
