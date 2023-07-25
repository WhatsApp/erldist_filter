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
%%% Created :  09 Jun 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(vedf).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

%% API
-export([
    dist_ext_to_vdist/2,
    dist_ext_to_vterm/2,
    dist_ext_to_vterm/3,
    dist_int_to_vdist/2,
    dist_int_to_vterm/2,
    dist_int_to_vterm/3
]).

%% Types
-type atoms_tuple() :: erldist_filter_nif:logger_event_atoms().

-export_type([
    atoms_tuple/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec dist_ext_to_vdist(AtomsTuple, InputBinary) -> VDist when
    AtomsTuple :: atoms_tuple(), InputBinary :: binary(), VDist :: vdist:dop_t().
dist_ext_to_vdist(AtomsTuple, InputBinary) ->
    VT = dist_ext_to_vterm(AtomsTuple, InputBinary),
    VDist = vdist_dop:control_message_vterm_to_dop(VT),
    VDist.

-spec dist_ext_to_vterm(AtomsTuple, InputBinary) -> VTerm when
    AtomsTuple :: atoms_tuple(), InputBinary :: binary(), VTerm :: vterm:t().
dist_ext_to_vterm(AtomsTuple, InputBinary) when is_tuple(AtomsTuple) ->
    {ok, VT0, <<>>} = vterm_decode:external_binary_to_vterm(InputBinary),
    VT1 = vterm:resolve_atoms(AtomsTuple, VT0),
    VT1.

-spec dist_ext_to_vterm(AtomsTuple, InputBinary, Limit) -> VTerm when
    AtomsTuple :: atoms_tuple(), InputBinary :: binary(), Limit :: integer(), VTerm :: vterm:t().
dist_ext_to_vterm(AtomsTuple, InputBinary, Limit) when is_tuple(AtomsTuple) ->
    {ok, VT0, <<>>} = vterm_decode:external_binary_to_vterm_lazy(InputBinary, Limit),
    VT1 = vterm:resolve_atoms(AtomsTuple, VT0),
    VT1.

-spec dist_int_to_vdist(AtomsTuple, InputBinary) -> VDist when
    AtomsTuple :: atoms_tuple(), InputBinary :: binary(), VDist :: vdist:dop_t().
dist_int_to_vdist(AtomsTuple, InputBinary) ->
    VT = dist_int_to_vterm(AtomsTuple, InputBinary),
    VDist = vdist_dop:control_message_vterm_to_dop(VT),
    VDist.

-spec dist_int_to_vterm(AtomsTuple, InputBinary) -> VTerm when
    AtomsTuple :: atoms_tuple(), InputBinary :: binary(), VTerm :: vterm:t().
dist_int_to_vterm(AtomsTuple, InputBinary) when is_tuple(AtomsTuple) ->
    {ok, VT0, <<>>} = vterm_decode:internal_binary_to_vterm(InputBinary),
    VT1 = vterm:resolve_atoms(AtomsTuple, VT0),
    VT1.

-spec dist_int_to_vterm(AtomsTuple, InputBinary, Limit) -> VTerm when
    AtomsTuple :: atoms_tuple(), InputBinary :: binary(), Limit :: integer(), VTerm :: vterm:t().
dist_int_to_vterm(AtomsTuple, InputBinary, Limit) when is_tuple(AtomsTuple) ->
    {ok, VT0, <<>>} = vterm_decode:internal_binary_to_vterm_lazy(InputBinary, Limit),
    VT1 = vterm:resolve_atoms(AtomsTuple, VT0),
    VT1.
