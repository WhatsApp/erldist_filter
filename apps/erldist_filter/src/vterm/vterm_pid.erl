%%%%-----------------------------------------------------------------------------
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
-module(vterm_pid).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include("erldist_filter.hrl").

-export([
    sequence_id/1
]).

-spec sequence_id(vterm:pid_t()) -> non_neg_integer().
sequence_id(#vterm_new_pid_ext{id = Id, serial = Serial, creation = Creation}) ->
    hash_u64(Id, Serial, Creation);
sequence_id(#vterm_pid_ext{id = Id, serial = Serial, creation = Creation}) ->
    hash_u64(Id, Serial, Creation).

hash_u64(Id, Serial, Creation) ->
    ((erlang:phash2({Creation, Serial, Id}, 16#FFFFFFFF) bsl 32) bor erlang:phash2({Id, Serial, Creation}, 16#FFFFFFFF)) band
        16#FFFFFFFFFFFFFFFF.
