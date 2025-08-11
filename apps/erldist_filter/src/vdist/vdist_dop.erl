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
-module(vdist_dop).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("erldist_filter/include/erldist_filter.hrl").
-include_lib("erldist_filter/include/erldist_filter_erts_dist.hrl").

-export([
    control_message_vterm_to_dop/1,
    dop_to_control_message_vterm/1,
    dop_has_payload/1,
    dop_sequence_id/1
]).

-spec control_message_vterm_to_dop(vterm:t()) -> vdist:dop_t().
control_message_vterm_to_dop(#vterm_large_tuple_ext{arity = Arity, elements = [VTermDOP | Elements]}) ->
    IntegerDOP = vterm:simplify(VTermDOP),
    control_message_vterm_to_dop(Arity, IntegerDOP, Elements);
control_message_vterm_to_dop(#vterm_small_tuple_ext{arity = Arity, elements = [VTermDOP | Elements]}) ->
    IntegerDOP = vterm:simplify(VTermDOP),
    control_message_vterm_to_dop(Arity, IntegerDOP, Elements).

-spec dop_to_control_message_vterm(vdist:dop_t()) -> vterm:tuple_t().
dop_to_control_message_vterm(T) when ?is_vdist_dop_t(T) ->
    Module = element(1, T),
    Module:into_control_message_vterm(T).

-spec dop_has_payload(vdist:dop_t()) -> boolean().
dop_has_payload(T) when ?is_vdist_dop_t(T) ->
    Module = element(1, T),
    Module:has_payload(T).

-spec dop_sequence_id(vdist:dop_t()) -> vdist:sequence_id().
dop_sequence_id(T) when ?is_vdist_dop_t(T) ->
    Module = element(1, T),
    Module:sequence_id(T).

-spec control_message_vterm_to_dop(vterm:u32(), OpCode :: pos_integer(), [vterm:t()]) -> vdist:dop_t().
control_message_vterm_to_dop(_Arity = 3, IntegerDOP, [A, B]) ->
    case IntegerDOP of
        ?DOP_ALIAS_SEND when ?is_vterm_pid_t(A) andalso ?is_vterm_reference_t(B) ->
            FromPid = A,
            Alias = B,
            vdist_dop_alias_send:new(FromPid, Alias);
        ?DOP_GROUP_LEADER when ?is_vterm_pid_t(A) andalso ?is_vterm_pid_t(B) ->
            FromPid = A,
            ToPid = B,
            vdist_dop_group_leader:new(FromPid, ToPid);
        ?DOP_LINK when ?is_vterm_pid_t(A) andalso ?is_vterm_pid_t(B) ->
            FromPid = A,
            ToPid = B,
            vdist_dop_link:new(FromPid, ToPid);
        ?DOP_PAYLOAD_EXIT when ?is_vterm_pid_t(A) andalso ?is_vterm_pid_t(B) ->
            FromPid = A,
            ToPid = B,
            vdist_dop_payload_exit:new(FromPid, ToPid);
        ?DOP_PAYLOAD_EXIT2 when ?is_vterm_pid_t(A) andalso ?is_vterm_pid_t(B) ->
            FromPid = A,
            ToPid = B,
            vdist_dop_payload_exit2:new(FromPid, ToPid);
        ?DOP_SEND when ?is_vterm_t(A) andalso ?is_vterm_pid_t(B) ->
            Unused = A,
            ToPid = B,
            vdist_dop_send:new(Unused, ToPid);
        ?DOP_SEND_SENDER when ?is_vterm_pid_t(A) andalso ?is_vterm_pid_t(B) ->
            FromPid = A,
            ToPid = B,
            vdist_dop_send_sender:new(FromPid, ToPid);
        ?DOP_UNLINK when ?is_vterm_pid_t(A) andalso ?is_vterm_pid_t(B) ->
            FromPid = A,
            ToPid = B,
            vdist_dop_unlink:new(FromPid, ToPid)
    end;
control_message_vterm_to_dop(_Arity = 4, IntegerDOP, [A, B, C]) ->
    case IntegerDOP of
        ?DOP_ALIAS_SEND_TT when ?is_vterm_pid_t(A) andalso ?is_vterm_reference_t(B) andalso ?is_vterm_t(C) ->
            FromPid = A,
            Alias = B,
            TraceToken = C,
            vdist_dop_alias_send_tt:new(FromPid, Alias, TraceToken);
        ?DOP_DEMONITOR_P when
            ?is_vterm_pid_t(A) andalso (?is_vterm_pid_t(B) orelse ?is_vterm_atom_t(B)) andalso ?is_vterm_reference_t(C)
        ->
            FromPid = A,
            ToProc = B,
            Ref = C,
            vdist_dop_demonitor_p:new(FromPid, ToProc, Ref);
        ?DOP_EXIT when ?is_vterm_pid_t(A) andalso ?is_vterm_pid_t(B) andalso ?is_vterm_t(C) ->
            FromPid = A,
            ToPid = B,
            Reason = C,
            vdist_dop_exit:new(FromPid, ToPid, Reason);
        ?DOP_EXIT2 when ?is_vterm_pid_t(A) andalso ?is_vterm_pid_t(B) andalso ?is_vterm_t(C) ->
            FromPid = A,
            ToPid = B,
            Reason = C,
            vdist_dop_exit2:new(FromPid, ToPid, Reason);
        ?DOP_MONITOR_P when
            ?is_vterm_pid_t(A) andalso (?is_vterm_pid_t(B) orelse ?is_vterm_atom_t(B)) andalso ?is_vterm_reference_t(C)
        ->
            FromPid = A,
            ToProc = B,
            Ref = C,
            vdist_dop_monitor_p:new(FromPid, ToProc, Ref);
        ?DOP_PAYLOAD_EXIT_TT when ?is_vterm_pid_t(A) andalso ?is_vterm_pid_t(B) andalso ?is_vterm_t(C) ->
            FromPid = A,
            ToPid = B,
            TraceToken = C,
            vdist_dop_payload_exit_tt:new(FromPid, ToPid, TraceToken);
        ?DOP_PAYLOAD_EXIT2_TT when ?is_vterm_pid_t(A) andalso ?is_vterm_pid_t(B) andalso ?is_vterm_t(C) ->
            FromPid = A,
            ToPid = B,
            TraceToken = C,
            vdist_dop_payload_exit2_tt:new(FromPid, ToPid, TraceToken);
        ?DOP_PAYLOAD_MONITOR_P_EXIT when
            (?is_vterm_pid_t(A) orelse ?is_vterm_atom_t(A)) andalso ?is_vterm_pid_t(B) andalso ?is_vterm_reference_t(C)
        ->
            FromProc = A,
            ToPid = B,
            Ref = C,
            vdist_dop_payload_monitor_p_exit:new(FromProc, ToPid, Ref);
        ?DOP_REG_SEND when ?is_vterm_pid_t(A) andalso ?is_vterm_t(B) andalso ?is_vterm_atom_t(C) ->
            FromPid = A,
            Unused = B,
            ToName = C,
            vdist_dop_reg_send:new(FromPid, Unused, ToName);
        ?DOP_SEND_TT when ?is_vterm_t(A) andalso ?is_vterm_pid_t(B) andalso ?is_vterm_t(C) ->
            Unused = A,
            ToPid = B,
            TraceToken = C,
            vdist_dop_send_tt:new(Unused, ToPid, TraceToken);
        ?DOP_SEND_SENDER_TT when ?is_vterm_pid_t(A) andalso ?is_vterm_pid_t(B) andalso ?is_vterm_t(C) ->
            FromPid = A,
            ToPid = B,
            TraceToken = C,
            vdist_dop_send_sender_tt:new(FromPid, ToPid, TraceToken);
        ?DOP_UNLINK_ID when ?is_vterm_integer_t(A) andalso ?is_vterm_pid_t(B) andalso ?is_vterm_pid_t(C) ->
            Id = A,
            FromPid = B,
            ToPid = C,
            vdist_dop_unlink_id:new(Id, FromPid, ToPid);
        ?DOP_UNLINK_ID_ACK when ?is_vterm_integer_t(A) andalso ?is_vterm_pid_t(B) andalso ?is_vterm_pid_t(C) ->
            Id = A,
            FromPid = B,
            ToPid = C,
            vdist_dop_unlink_id_ack:new(Id, FromPid, ToPid)
    end;
control_message_vterm_to_dop(_Arity = 5, IntegerDOP, [A, B, C, D]) ->
    case IntegerDOP of
        ?DOP_EXIT_TT when
            ?is_vterm_pid_t(A) andalso ?is_vterm_pid_t(B) andalso ?is_vterm_t(C) andalso ?is_vterm_t(D)
        ->
            FromPid = A,
            ToPid = B,
            TraceToken = C,
            Reason = D,
            vdist_dop_exit_tt:new(FromPid, ToPid, TraceToken, Reason);
        ?DOP_EXIT2_TT when
            ?is_vterm_pid_t(A) andalso ?is_vterm_pid_t(B) andalso ?is_vterm_t(C) andalso ?is_vterm_t(D)
        ->
            FromPid = A,
            ToPid = B,
            TraceToken = C,
            Reason = D,
            vdist_dop_exit2_tt:new(FromPid, ToPid, TraceToken, Reason);
        ?DOP_MONITOR_P_EXIT when
            (?is_vterm_pid_t(A) orelse ?is_vterm_atom_t(A)) andalso ?is_vterm_pid_t(B) andalso ?is_vterm_reference_t(C) andalso
                ?is_vterm_t(D)
        ->
            FromProc = A,
            ToPid = B,
            Ref = C,
            Reason = D,
            vdist_dop_monitor_p_exit:new(FromProc, ToPid, Ref, Reason);
        ?DOP_REG_SEND_TT when
            ?is_vterm_pid_t(A) andalso ?is_vterm_t(B) andalso ?is_vterm_atom_t(C) andalso ?is_vterm_t(D)
        ->
            FromPid = A,
            Unused = B,
            ToName = C,
            TraceToken = D,
            vdist_dop_reg_send_tt:new(FromPid, Unused, ToName, TraceToken);
        ?DOP_SPAWN_REPLY when
            ?is_vterm_reference_t(A) andalso ?is_vterm_pid_t(B) andalso ?is_vterm_fixed_integer_t(C) andalso
                (?is_vterm_atom_t(D) orelse ?is_vterm_pid_t(D))
        ->
            ReqId = A,
            To = B,
            Flags = C,
            Result = D,
            vdist_dop_spawn_reply:new(ReqId, To, Flags, Result)
    end;
control_message_vterm_to_dop(_Arity = 6, IntegerDOP, [A, B, C, D, E]) ->
    case IntegerDOP of
        ?DOP_SPAWN_REPLY_TT when
            ?is_vterm_reference_t(A) andalso ?is_vterm_pid_t(B) andalso ?is_vterm_fixed_integer_t(C) andalso
                (?is_vterm_atom_t(D) orelse ?is_vterm_pid_t(D)) andalso ?is_vterm_t(E)
        ->
            ReqId = A,
            To = B,
            Flags = C,
            Result = D,
            Token = E,
            vdist_dop_spawn_reply_tt:new(ReqId, To, Flags, Result, Token);
        ?DOP_SPAWN_REQUEST when
            ?is_vterm_reference_t(A) andalso ?is_vterm_pid_t(B) andalso ?is_vterm_pid_t(C) andalso ?is_vterm_tuple_t(D) andalso
                ?is_vterm_list_t(E)
        ->
            ReqId = A,
            From = B,
            GroupLeader = C,
            {Module, Function, Arity} =
                case D of
                    #vterm_large_tuple_ext{arity = 3, elements = [Mv, Fv, Av]} when
                        ?is_vterm_atom_t(Mv) andalso ?is_vterm_atom_t(Fv) andalso ?is_vterm_fixed_integer_t(Av)
                    ->
                        {Mv, Fv, Av};
                    #vterm_small_tuple_ext{arity = 3, elements = [Mv, Fv, Av]} when
                        ?is_vterm_atom_t(Mv) andalso ?is_vterm_atom_t(Fv) andalso ?is_vterm_fixed_integer_t(Av)
                    ->
                        {Mv, Fv, Av}
                end,
            OptList =
                case E of
                    #vterm_nil_ext{} ->
                        [];
                    #vterm_list_ext{elements = OptListElements, tail = #vterm_nil_ext{}} when
                        is_list(OptListElements)
                    ->
                        OptListElements
                end,
            vdist_dop_spawn_request:new(ReqId, From, GroupLeader, Module, Function, Arity, OptList)
    end;
control_message_vterm_to_dop(_Arity = 7, IntegerDOP, [A, B, C, D, E, F]) ->
    case IntegerDOP of
        ?DOP_SPAWN_REQUEST_TT when
            ?is_vterm_reference_t(A) andalso ?is_vterm_pid_t(B) andalso ?is_vterm_pid_t(C) andalso ?is_vterm_tuple_t(D) andalso
                ?is_vterm_list_t(E) andalso ?is_vterm_t(F)
        ->
            ReqId = A,
            From = B,
            GroupLeader = C,
            {Module, Function, Arity} =
                case D of
                    #vterm_large_tuple_ext{arity = 3, elements = [Mv, Fv, Av]} when
                        ?is_vterm_atom_t(Mv) andalso ?is_vterm_atom_t(Fv) andalso ?is_vterm_fixed_integer_t(Av)
                    ->
                        {Mv, Fv, Av};
                    #vterm_small_tuple_ext{arity = 3, elements = [Mv, Fv, Av]} when
                        ?is_vterm_atom_t(Mv) andalso ?is_vterm_atom_t(Fv) andalso ?is_vterm_fixed_integer_t(Av)
                    ->
                        {Mv, Fv, Av}
                end,
            OptList =
                case E of
                    #vterm_nil_ext{} ->
                        [];
                    #vterm_list_ext{elements = OptListElements, tail = #vterm_nil_ext{}} when
                        is_list(OptListElements)
                    ->
                        OptListElements
                end,
            Token = F,
            vdist_dop_spawn_request_tt:new(ReqId, From, GroupLeader, Module, Function, Arity, OptList, Token)
    end.
