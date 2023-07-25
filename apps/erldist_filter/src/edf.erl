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
%%% Created :  17 Jun 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(edf).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("erldist_filter/include/udist.hrl").

%% Internal API
-export([
    dop/3,
    req/3
]).

%%%=============================================================================
%%% Internal API functions
%%%=============================================================================

-spec dop(Node, Control, Payload) -> no_return() when
    Node :: node(),
    Control :: eqwalizer:dynamic(),
    Payload :: undefined | eqwalizer:dynamic().
dop(Node, Control0, Payload) ->
    _Control = udist:cast_to_dop(Control0),
    _ = Node,
    _ = Payload,
    exit(normal).

-spec req(Node, {Module, FunctionName, Arity}, Arguments) -> no_return() | eqwalizer:dynamic() when
    Node :: node(),
    Module :: module(),
    FunctionName :: atom(),
    Arity :: non_neg_integer(),
    Arguments :: [eqwalizer:dynamic()].
req(_Node, {Module, FunctionName, Arity}, Arguments) when length(Arguments) =:= Arity ->
    erlang:apply(Module, FunctionName, Arguments).
