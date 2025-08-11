%%% % @format
%%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% Created :  27 Mar 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
-module(vterm_atom).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("erldist_filter/include/erldist_filter.hrl").

-export([
    sequence_id/1
]).

-spec sequence_id(T) -> non_neg_integer() when T :: vterm:atom_t().
sequence_id(#vterm_atom_cache_ref_resolved{term = Term}) ->
    sequence_id(vterm:expand_atom(Term));
sequence_id(#vterm_atom_ext{len = Len, name = Name}) ->
    hash_u64(Len, Name);
sequence_id(#vterm_atom_utf8_ext{len = Len, name = Name}) ->
    hash_u64(Len, Name);
sequence_id(#vterm_small_atom_ext{len = Len, name = Name}) ->
    hash_u64(Len, Name);
sequence_id(#vterm_small_atom_utf8_ext{len = Len, name = Name}) ->
    hash_u64(Len, Name).

-spec hash_u64(Len, Name) -> Hash when Len :: non_neg_integer(), Name :: binary(), Hash :: non_neg_integer().
hash_u64(Len, Name) ->
    ((erlang:phash2({Name, Len}, 16#FFFFFFFF) bsl 32) bor erlang:phash2({Len, Name}, 16#FFFFFFFF)) band
        16#FFFFFFFFFFFFFFFF.
