%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% Created :  27 Mar 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
-module(vterm_encode).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("erldist_filter/include/erldist_filter.hrl").
-include_lib("erldist_filter/include/erldist_filter_erts_external.hrl").

%% Callbacks
-callback internal_vterm_to_binary(T :: vterm:t(), Options :: term()) -> binary().

%% API
-export([
    external_term_to_binary/1,
    external_vterm_to_binary/2,
    internal_term_to_binary/1,
    internal_vterm_to_binary/2,
    internal_vterm_elements_to_binary/2,
    internal_vterm_pairs_to_binary/2
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec external_term_to_binary(Term) -> binary() when Term :: term().
external_term_to_binary(Term) ->
    erlang:term_to_binary(Term, [{minor_version, 2}]).

-spec external_vterm_to_binary(VTerm, Opts) -> binary() when VTerm :: vterm:t(), Opts :: term().
external_vterm_to_binary(T, Opts) when ?is_vterm_t(T) ->
    InternalEncodedTerm = internal_vterm_to_binary(T, Opts),
    <<?VERSION_MAGIC:8, InternalEncodedTerm/bytes>>.

-spec internal_term_to_binary(Term) -> binary() when Term :: term().
internal_term_to_binary(Term) ->
    <<?VERSION_MAGIC:8, InternalEncodedTerm/binary>> = external_term_to_binary(Term),
    InternalEncodedTerm.

-spec internal_vterm_to_binary(VTerm, Opts) -> binary() when VTerm :: vterm:t(), Opts :: term().
internal_vterm_to_binary(T, Opts) when ?is_vterm_t(T) ->
    Module = element(1, T),
    Module:internal_vterm_to_binary(T, Opts).

-spec internal_vterm_elements_to_binary(Elements, Options) -> binary() when Elements :: [vterm:t()], Options :: term().
internal_vterm_elements_to_binary([], _Opts) ->
    <<>>;
internal_vterm_elements_to_binary(Elements, Opts) when is_list(Elements) ->
    internal_vterm_elements_to_binary(Elements, Opts, []).

-spec internal_vterm_elements_to_binary(Elements, Options, Acc) -> binary() when
    Elements :: [vterm:t()], Options :: term(), Acc :: iodata().
internal_vterm_elements_to_binary([], _Opts, Acc) ->
    erlang:iolist_to_binary(Acc);
internal_vterm_elements_to_binary([Element | Elements], Opts, Acc) ->
    internal_vterm_elements_to_binary(Elements, Opts, [Acc, internal_vterm_to_binary(Element, Opts)]).

-spec internal_vterm_pairs_to_binary(Pairs, Options) -> binary() when
    Pairs :: [{vterm:t(), vterm:t()}], Options :: term().
internal_vterm_pairs_to_binary([], _Opts) ->
    <<>>;
internal_vterm_pairs_to_binary(Pairs, Opts) when is_list(Pairs) ->
    internal_vterm_pairs_to_binary(Pairs, Opts, []).

-spec internal_vterm_pairs_to_binary(Pairs, Options, Acc) -> binary() when
    Pairs :: [{vterm:t(), vterm:t()}], Options :: term(), Acc :: iodata().
internal_vterm_pairs_to_binary([], _Opts, Acc) ->
    erlang:iolist_to_binary(Acc);
internal_vterm_pairs_to_binary([{Key, Value} | Pairs], Opts, Acc) ->
    internal_vterm_pairs_to_binary(Pairs, Opts, [
        Acc, internal_vterm_to_binary(Key, Opts), internal_vterm_to_binary(Value, Opts)
    ]).
