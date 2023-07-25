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
-module(vdist_external).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include("erldist_filter.hrl").

%% API
-export([
    new/3,
    append_next_fragment/3,
    decode_next_fragment/3
]).

%% Types
-type t() :: #vdist_external{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Table, SequenceId, FragmentCount) -> t() when
    Table :: vdist_atom_translation_table:t(),
    SequenceId :: vdist:sequence_id(),
    FragmentCount :: vdist:fragment_id().
new(AtomTable = #vdist_atom_translation_table{}, SequenceId, FragmentCount) when
    ?is_u64(SequenceId) andalso ?is_u64(FragmentCount) andalso FragmentCount > 0
->
    #vdist_external{
        atom_table = AtomTable,
        sequence_id = SequenceId,
        fragment_id_next = FragmentCount,
        fragment_count = FragmentCount,
        fragments = queue:new()
    }.

-spec append_next_fragment(t(), FragmentCont, Fragment) ->
    {ok, queue:queue(Fragment), vdist_atom_translation_table:t()} | {cont, t()}
when
    FragmentCont :: vdist_fragment_cont:t(),
    Fragment :: binary().
append_next_fragment(
    External0 = #vdist_external{
        atom_table = AtomTable, sequence_id = SequenceId, fragment_id_next = FragmentId, fragments = Fragments0
    },
    #vdist_fragment_cont{sequence_id = SequenceId, fragment_id = FragmentId},
    Fragment
) when is_binary(Fragment) ->
    FragmentIdNext = FragmentId - 1,
    Fragments1 = queue:in(Fragment, Fragments0),
    case FragmentIdNext of
        0 ->
            {ok, Fragments1, AtomTable};
        _ ->
            External1 = External0#vdist_external{fragment_id_next = FragmentIdNext, fragments = Fragments1},
            {cont, External1}
    end.

-spec decode_next_fragment(t(), Header, Fragment) ->
    {ok, binary(), vdist_atom_translation_table:t()} | {cont, t()}
when
    Header :: vdist_fragment_cont:t(),
    Fragment :: binary().
decode_next_fragment(External0 = #vdist_external{}, Header = #vdist_fragment_cont{}, Fragment) when
    is_binary(Fragment)
->
    case append_next_fragment(External0, Header, Fragment) of
        {ok, FragmentsQueue, AtomTable} ->
            Fragments = erlang:iolist_to_binary(queue:to_list(FragmentsQueue)),
            {ok, Fragments, AtomTable};
        {cont, External1} ->
            {cont, External1}
    end.
