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
-module(vterm_reference).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("erldist_filter/include/erldist_filter.hrl").

-export([
    sequence_id/1
]).

-spec sequence_id(vterm:reference_t()) -> non_neg_integer().
sequence_id(#vterm_newer_reference_ext{creation = Creation, ids = Ids}) ->
    hash_u64(Ids, Creation);
sequence_id(#vterm_new_reference_ext{creation = Creation, ids = Ids}) ->
    hash_u64(Ids, Creation);
sequence_id(#vterm_reference_ext{id = Id, creation = Creation}) ->
    hash_u64([Id], Creation).

-spec hash_u64(Ids, Creation) -> Hash when
    Ids :: [non_neg_integer()], Creation :: non_neg_integer(), Hash :: non_neg_integer().
hash_u64(Ids, Creation) ->
    ((erlang:phash2({Creation, Ids}, 16#FFFFFFFF) bsl 32) bor erlang:phash2({Ids, Creation}, 16#FFFFFFFF)) band
        16#FFFFFFFFFFFFFFFF.
