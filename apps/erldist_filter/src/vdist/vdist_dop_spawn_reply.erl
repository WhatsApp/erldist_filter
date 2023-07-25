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
-module(vdist_dop_spawn_reply).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include("erldist_filter.hrl").
-include("erldist_filter_erts_dist.hrl").

%% API
-export([
    new/4,
    has_payload/1,
    into_control_message_vterm/1,
    sequence_id/1
]).

%% Types
-type t() :: #vdist_dop_spawn_reply{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(ReqId, To, Flags, Result) -> T when
    ReqId :: vterm:reference_t(),
    To :: vterm:pid_t(),
    Flags :: vterm:fixed_integer_t(),
    Result :: vterm:atom_t() | vterm:pid_t(),
    T :: t().
new(ReqId, To, Flags, Result) when
    ?is_vterm_reference_t(ReqId) andalso ?is_vterm_pid_t(To) andalso ?is_vterm_fixed_integer_t(Flags) andalso
        (?is_vterm_atom_t(Result) orelse ?is_vterm_pid_t(Result))
->
    #vdist_dop_spawn_reply{req_id = ReqId, to = To, flags = Flags, result = Result}.

-spec has_payload(T) -> boolean() when T :: t().
has_payload(#vdist_dop_spawn_reply{}) ->
    false.

-spec into_control_message_vterm(T) -> vterm:tuple_t() when T :: t().
into_control_message_vterm(#vdist_dop_spawn_reply{
    req_id = ReqId, to = To, flags = Flags, result = Result
}) ->
    vterm_small_tuple_ext:new(5, [vterm_small_integer_ext:new(?DOP_SPAWN_REPLY), ReqId, To, Flags, Result]).

-spec sequence_id(T) -> vdist:sequence_id() when T :: t().
sequence_id(#vdist_dop_spawn_reply{req_id = ReqId}) when ?is_vterm_reference_t(ReqId) ->
    vterm_reference:sequence_id(ReqId).
