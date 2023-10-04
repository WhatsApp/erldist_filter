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
-module(vdist_dop_spawn_request).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include("erldist_filter.hrl").
-include("erldist_filter_erts_dist.hrl").

%% API
-export([
    new/7,
    has_payload/1,
    into_control_message_vterm/1,
    sequence_id/1
]).

%% Types
-type t() :: #vdist_dop_spawn_request{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(ReqId, From, GroupLeader, Module, Function, Arity, OptList) -> T when
    ReqId :: vterm:reference_t(),
    From :: vterm:pid_t(),
    GroupLeader :: vterm:pid_t(),
    Module :: vterm:atom_t(),
    Function :: vterm:atom_t(),
    Arity :: vterm:fixed_integer_t(),
    OptList :: [vterm:t()],
    T :: t().
new(ReqId, From, GroupLeader, Module, Function, Arity, OptList) when
    ?is_vterm_reference_t(ReqId) andalso ?is_vterm_pid_t(From) andalso ?is_vterm_pid_t(GroupLeader) andalso
        ?is_vterm_atom_t(Module) andalso ?is_vterm_atom_t(Function) andalso ?is_vterm_fixed_integer_t(Arity) andalso
        is_list(OptList) andalso length(OptList) >= 0
->
    #vdist_dop_spawn_request{
        req_id = ReqId,
        from = From,
        group_leader = GroupLeader,
        module = Module,
        function = Function,
        arity = Arity,
        opt_list = OptList
    }.

-spec has_payload(T) -> boolean() when T :: t().
has_payload(#vdist_dop_spawn_request{}) ->
    true.

-spec into_control_message_vterm(T) -> vterm:tuple_t() when T :: t().
into_control_message_vterm(#vdist_dop_spawn_request{
    req_id = ReqId,
    from = From,
    group_leader = GroupLeader,
    module = Module,
    function = Function,
    arity = Arity,
    opt_list = OptList
}) ->
    MFAVTerm = vterm_small_tuple_ext:new(3, [Module, Function, Arity]),
    OptListVTerm =
        case OptList of
            [] ->
                vterm_nil_ext:new();
            [_ | _] ->
                vterm_list_ext:new(length(OptList), OptList, vterm_nil_ext:new())
        end,
    vterm_small_tuple_ext:new(6, [
        vterm_small_integer_ext:new(?DOP_SPAWN_REQUEST), ReqId, From, GroupLeader, MFAVTerm, OptListVTerm
    ]).

-spec sequence_id(T) -> vdist:sequence_id() when T :: t().
sequence_id(#vdist_dop_spawn_request{from = From}) when ?is_vterm_pid_t(From) ->
    vterm_pid:sequence_id(From).
