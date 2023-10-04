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
%%% Created :  27 Mar 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(vterm_export_ext).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(vterm_encode).
-behaviour(vterm_simplify).

-include("erldist_filter.hrl").
-include("erldist_filter_erts_external.hrl").

%% API
-export([
    new/3,
    internal_vterm_to_binary/2,
    simplify/1
]).

%% Types
-type t() :: #vterm_export_ext{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Module, Function, Arity) -> T when
    Module :: vterm:atom_t(), Function :: vterm:atom_t(), Arity :: vterm:small_integer_t(), T :: t().
new(Module, Function, Arity) when
    ?is_vterm_atom_t(Module) andalso ?is_vterm_atom_t(Function) andalso ?is_vterm_small_integer_t(Arity)
->
    #vterm_export_ext{module = Module, function = Function, arity = Arity}.

-spec internal_vterm_to_binary(T, Opts) -> binary() when T :: t(), Opts :: term().
internal_vterm_to_binary(#vterm_export_ext{module = Module0, function = Function0, arity = Arity0}, Opts) ->
    Module = vterm_encode:internal_vterm_to_binary(Module0, Opts),
    Function = vterm_encode:internal_vterm_to_binary(Function0, Opts),
    Arity = vterm_encode:internal_vterm_to_binary(Arity0, Opts),
    <<?EXPORT_EXT, Module/bytes, Function/bytes, Arity/bytes>>.

-spec simplify(t()) -> fun().
simplify(#vterm_export_ext{module = Module0, function = Function0, arity = Arity0}) ->
    Module = vterm:simplify(Module0),
    Function = vterm:simplify(Function0),
    Arity = vterm:simplify(Arity0),
    fun Module:Function/Arity.
