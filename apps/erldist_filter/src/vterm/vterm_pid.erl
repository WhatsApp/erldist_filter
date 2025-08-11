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
-module(vterm_pid).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("erldist_filter/include/erldist_filter.hrl").

-export([
    sequence_id/1
]).

-spec sequence_id(vterm:pid_t()) -> non_neg_integer().
sequence_id(#vterm_new_pid_ext{id = Id, serial = Serial, creation = Creation}) ->
    hash_u64(Id, Serial, Creation);
sequence_id(#vterm_pid_ext{id = Id, serial = Serial, creation = Creation}) ->
    hash_u64(Id, Serial, Creation).

-spec hash_u64(non_neg_integer(), non_neg_integer(), non_neg_integer()) -> non_neg_integer().
hash_u64(Id, Serial, Creation) ->
    ((erlang:phash2({Creation, Serial, Id}, 16#FFFFFFFF) bsl 32) bor erlang:phash2({Id, Serial, Creation}, 16#FFFFFFFF)) band
        16#FFFFFFFFFFFFFFFF.
