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
-module(vdist_old_atom_cache_ref_entry).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include("erldist_filter.hrl").
-include("erldist_filter_erts_external.hrl").

%% API
-export([
    new/1,
    encode_header_flag/1,
    encode_header_reference/2
]).

%% Types
-type t() :: #vdist_old_atom_cache_ref_entry{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(AtomCacheIndex) -> T when AtomCacheIndex :: vdist:atom_cache_index(), T :: t().
new(AtomCacheIndex) when ?is_atom_cache_index(AtomCacheIndex) ->
    #vdist_old_atom_cache_ref_entry{atom_cache_index = AtomCacheIndex}.

-spec encode_header_flag(T) -> bitstring() when T :: t().
encode_header_flag(#vdist_old_atom_cache_ref_entry{atom_cache_index = AtomCacheIndex}) ->
    SegmentIndex = ((AtomCacheIndex bsr 8) band 7),
    <<0:1, SegmentIndex:3>>.

-spec encode_header_reference(T, LongAtoms) -> bitstring() when T :: t(), LongAtoms :: boolean().
encode_header_reference(#vdist_old_atom_cache_ref_entry{atom_cache_index = AtomCacheIndex}, _LongAtoms) ->
    InternalSegmentIndex = (AtomCacheIndex band 16#FF),
    <<InternalSegmentIndex:8>>.
