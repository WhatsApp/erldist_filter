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
%%% Created :  12 Oct 2022 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(proper_vdist).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(warn_missing_spec).
-wacov(ignore).

-include_lib("erldist_filter_test/include/proper_erldist_filter_test.hrl").
-include_lib("erldist_filter/include/erldist_filter.hrl").
-include_lib("erldist_filter/include/erldist_filter_erts_dist.hrl").

-import(proper_vterm, [
    mostly/2,
    u8/0,
    u64/0,
    vterm/0,
    vterm_atom/0,
    vterm_fixed_integer/0,
    vterm_fixed_integer/2,
    vterm_integer/0,
    vterm_integer/2,
    vterm_pid/0,
    vterm_reference/0
]).

-export([
    vdist_any_header/0,
    vdist_distribution_flags/0,
    vdist_any_dop_with_payload/0,
    vdist_any_dop_without_payload/0,
    vdist_dop_alias_send/0,
    vdist_dop_alias_send_tt/0,
    vdist_dop_demonitor_p/0,
    vdist_dop_exit/0,
    vdist_dop_exit_tt/0,
    vdist_dop_exit2/0,
    vdist_dop_exit2_tt/0,
    vdist_dop_group_leader/0,
    vdist_dop_link/0,
    vdist_dop_monitor_p/0,
    vdist_dop_monitor_p_exit/0,
    vdist_dop_payload_exit/0,
    vdist_dop_payload_exit_tt/0,
    vdist_dop_payload_exit2/0,
    vdist_dop_payload_exit2_tt/0,
    vdist_dop_payload_monitor_p_exit/0,
    vdist_dop_reg_send/0,
    vdist_dop_reg_send_tt/0,
    vdist_dop_send/0,
    vdist_dop_send_sender/0,
    vdist_dop_send_sender_tt/0,
    vdist_dop_send_tt/0,
    vdist_dop_spawn_reply/0,
    vdist_dop_spawn_reply_tt/0,
    vdist_dop_spawn_request/0,
    vdist_dop_spawn_request_tt/0,
    vdist_dop_unlink/0,
    vdist_dop_unlink_id/0,
    vdist_dop_unlink_id_ack/0,
    vdist_entry/0,
    vdist_fragment_cont/0,
    vdist_fragment_header/0,
    vdist_normal_header/0,
    vdist_new_atom_cache_ref_entry/0,
    vdist_old_atom_cache_ref_entry/0,
    vdist_pass_through_header/0,
    vdist_payload/2
]).

-spec vdist_any_header() -> proper_types:type().
vdist_any_header() ->
    oneof([
        vdist_fragment_cont(),
        vdist_fragment_header(),
        vdist_normal_header(),
        vdist_pass_through_header()
    ]).

-spec vdist_distribution_flags() -> proper_types:type().
vdist_distribution_flags() ->
    oneof([
        ?DFLAG_DIST_MANDATORY,
        ?DFLAG_DIST_DEFAULT,
        % No fragment header support
        ?DFLAG_DIST_DEFAULT band (bnot ?DFLAG_FRAGMENTS),
        % No normal header support
        ?DFLAG_DIST_DEFAULT band (bnot ?DFLAG_DIST_HDR_ATOM_CACHE)
    ]).

-spec vdist_any_dop_with_payload() -> proper_types:type().
vdist_any_dop_with_payload() ->
    oneof([
        vdist_dop_alias_send(),
        vdist_dop_alias_send_tt(),
        vdist_dop_payload_exit(),
        vdist_dop_payload_exit_tt(),
        vdist_dop_payload_exit2(),
        vdist_dop_payload_exit2_tt(),
        vdist_dop_payload_monitor_p_exit(),
        vdist_dop_reg_send(),
        vdist_dop_reg_send_tt(),
        vdist_dop_send(),
        vdist_dop_send_sender(),
        vdist_dop_send_sender_tt(),
        vdist_dop_send_tt(),
        vdist_dop_spawn_request(),
        vdist_dop_spawn_request_tt()
    ]).

-spec vdist_any_dop_without_payload() -> proper_types:type().
vdist_any_dop_without_payload() ->
    oneof([
        vdist_dop_demonitor_p(),
        vdist_dop_exit(),
        vdist_dop_exit_tt(),
        vdist_dop_exit2(),
        vdist_dop_exit2_tt(),
        vdist_dop_group_leader(),
        vdist_dop_link(),
        vdist_dop_monitor_p(),
        vdist_dop_monitor_p_exit(),
        vdist_dop_spawn_reply(),
        vdist_dop_spawn_reply_tt(),
        vdist_dop_unlink(),
        vdist_dop_unlink_id(),
        vdist_dop_unlink_id_ack()
    ]).

-spec vdist_dop_alias_send() -> proper_types:type().
vdist_dop_alias_send() ->
    ?LET(
        {FromPid, Alias},
        {vterm_pid(), vterm_reference()},
        begin
            vdist_dop_alias_send:new(FromPid, Alias)
        end
    ).

-spec vdist_dop_alias_send_tt() -> proper_types:type().
vdist_dop_alias_send_tt() ->
    ?LET(
        {FromPid, Alias, TraceToken},
        {vterm_pid(), vterm_reference(), vterm()},
        begin
            vdist_dop_alias_send_tt:new(FromPid, Alias, TraceToken)
        end
    ).

-spec vdist_dop_demonitor_p() -> proper_types:type().
vdist_dop_demonitor_p() ->
    ?LET(
        {FromPid, ToProc, Ref},
        {vterm_pid(), oneof([vterm_pid(), vterm_atom()]), vterm_reference()},
        vdist_dop_demonitor_p:new(FromPid, ToProc, Ref)
    ).

-spec vdist_dop_exit() -> proper_types:type().
vdist_dop_exit() ->
    ?LET(
        {FromPid, ToPid, Reason},
        {vterm_pid(), vterm_pid(), vterm()},
        vdist_dop_exit:new(FromPid, ToPid, Reason)
    ).

-spec vdist_dop_exit_tt() -> proper_types:type().
vdist_dop_exit_tt() ->
    ?LET(
        {FromPid, ToPid, TraceToken, Reason},
        {vterm_pid(), vterm_pid(), vterm(), vterm()},
        vdist_dop_exit_tt:new(FromPid, ToPid, TraceToken, Reason)
    ).

-spec vdist_dop_exit2() -> proper_types:type().
vdist_dop_exit2() ->
    ?LET(
        {FromPid, ToPid, Reason},
        {vterm_pid(), vterm_pid(), vterm()},
        vdist_dop_exit2:new(FromPid, ToPid, Reason)
    ).

-spec vdist_dop_exit2_tt() -> proper_types:type().
vdist_dop_exit2_tt() ->
    ?LET(
        {FromPid, ToPid, TraceToken, Reason},
        {vterm_pid(), vterm_pid(), vterm(), vterm()},
        vdist_dop_exit2_tt:new(FromPid, ToPid, TraceToken, Reason)
    ).

-spec vdist_dop_group_leader() -> proper_types:type().
vdist_dop_group_leader() ->
    ?LET(
        {FromPid, ToPid},
        {vterm_pid(), vterm_pid()},
        vdist_dop_group_leader:new(FromPid, ToPid)
    ).

-spec vdist_dop_link() -> proper_types:type().
vdist_dop_link() ->
    ?LET(
        {FromPid, ToPid},
        {vterm_pid(), vterm_pid()},
        vdist_dop_link:new(FromPid, ToPid)
    ).

-spec vdist_dop_monitor_p() -> proper_types:type().
vdist_dop_monitor_p() ->
    ?LET(
        {FromPid, ToProc, Ref},
        {vterm_pid(), oneof([vterm_pid(), vterm_atom()]), vterm_reference()},
        vdist_dop_monitor_p:new(FromPid, ToProc, Ref)
    ).

-spec vdist_dop_monitor_p_exit() -> proper_types:type().
vdist_dop_monitor_p_exit() ->
    ?LET(
        {FromProc, ToPid, Ref, Reason},
        {oneof([vterm_pid(), vterm_atom()]), vterm_pid(), vterm_reference(), vterm()},
        vdist_dop_monitor_p_exit:new(FromProc, ToPid, Ref, Reason)
    ).

-spec vdist_dop_payload_exit() -> proper_types:type().
vdist_dop_payload_exit() ->
    ?LET(
        {FromPid, ToPid},
        {vterm_pid(), vterm_pid()},
        begin
            vdist_dop_payload_exit:new(FromPid, ToPid)
        end
    ).

-spec vdist_dop_payload_exit_tt() -> proper_types:type().
vdist_dop_payload_exit_tt() ->
    ?LET(
        {FromPid, ToPid, TraceToken},
        {vterm_pid(), vterm_pid(), vterm()},
        begin
            vdist_dop_payload_exit_tt:new(FromPid, ToPid, TraceToken)
        end
    ).

-spec vdist_dop_payload_exit2() -> proper_types:type().
vdist_dop_payload_exit2() ->
    ?LET(
        {FromPid, ToPid},
        {vterm_pid(), vterm_pid()},
        begin
            vdist_dop_payload_exit2:new(FromPid, ToPid)
        end
    ).

-spec vdist_dop_payload_exit2_tt() -> proper_types:type().
vdist_dop_payload_exit2_tt() ->
    ?LET(
        {FromPid, ToPid, TraceToken},
        {vterm_pid(), vterm_pid(), vterm()},
        begin
            vdist_dop_payload_exit2_tt:new(FromPid, ToPid, TraceToken)
        end
    ).

-spec vdist_dop_payload_monitor_p_exit() -> proper_types:type().
vdist_dop_payload_monitor_p_exit() ->
    ?LET(
        {FromProc, ToPid, Ref},
        {oneof([vterm_pid(), vterm_atom()]), vterm_pid(), vterm_reference()},
        vdist_dop_payload_monitor_p_exit:new(FromProc, ToPid, Ref)
    ).

-spec vdist_dop_reg_send() -> proper_types:type().
vdist_dop_reg_send() ->
    ?LET(
        {FromPid, ToName},
        {vterm_pid(), vterm_atom()},
        begin
            Unused = vterm_small_atom_utf8_ext:new(0, <<>>),
            vdist_dop_reg_send:new(FromPid, Unused, ToName)
        end
    ).

-spec vdist_dop_reg_send_tt() -> proper_types:type().
vdist_dop_reg_send_tt() ->
    ?LET(
        {FromPid, ToName, TraceToken},
        {vterm_pid(), vterm_atom(), vterm()},
        begin
            Unused = vterm_small_atom_utf8_ext:new(0, <<>>),
            vdist_dop_reg_send_tt:new(FromPid, Unused, ToName, TraceToken)
        end
    ).

-spec vdist_dop_send() -> proper_types:type().
vdist_dop_send() ->
    ?LET(
        {ToPid},
        {vterm_pid()},
        begin
            Unused = vterm_small_atom_utf8_ext:new(0, <<>>),
            vdist_dop_send:new(Unused, ToPid)
        end
    ).

-spec vdist_dop_send_sender() -> proper_types:type().
vdist_dop_send_sender() ->
    ?LET(
        {FromPid, ToPid},
        {vterm_pid(), vterm_pid()},
        begin
            vdist_dop_send_sender:new(FromPid, ToPid)
        end
    ).

-spec vdist_dop_send_sender_tt() -> proper_types:type().
vdist_dop_send_sender_tt() ->
    ?LET(
        {FromPid, ToPid, TraceToken},
        {vterm_pid(), vterm_pid(), vterm()},
        begin
            vdist_dop_send_sender_tt:new(FromPid, ToPid, TraceToken)
        end
    ).

-spec vdist_dop_send_tt() -> proper_types:type().
vdist_dop_send_tt() ->
    ?LET(
        {ToPid, TraceToken},
        {vterm_pid(), vterm()},
        begin
            Unused = vterm_small_atom_utf8_ext:new(0, <<>>),
            vdist_dop_send_tt:new(Unused, ToPid, TraceToken)
        end
    ).

-spec vdist_dop_spawn_reply() -> proper_types:type().
vdist_dop_spawn_reply() ->
    ?LET(
        {ReqId, To, Flags, Result},
        {vterm_reference(), vterm_pid(), vterm_fixed_integer(0, 2), oneof([vterm_atom(), vterm_pid()])},
        vdist_dop_spawn_reply:new(ReqId, To, Flags, Result)
    ).

-spec vdist_dop_spawn_reply_tt() -> proper_types:type().
vdist_dop_spawn_reply_tt() ->
    ?LET(
        {ReqId, To, Flags, Result, Token},
        {vterm_reference(), vterm_pid(), vterm_fixed_integer(), oneof([vterm_atom(), vterm_pid()]), vterm()},
        vdist_dop_spawn_reply_tt:new(ReqId, To, Flags, Result, Token)
    ).

-spec vdist_dop_spawn_request() -> proper_types:type().
vdist_dop_spawn_request() ->
    ?LET(
        {ReqId, From, GroupLeader, Module, Function, Arity, OptList},
        {
            vterm_reference(),
            vterm_pid(),
            vterm_pid(),
            vterm_atom(),
            vterm_atom(),
            mostly(vterm_fixed_integer(0, 4), vterm_fixed_integer(0, 255)),
            list(vterm())
        },
        vdist_dop_spawn_request:new(ReqId, From, GroupLeader, Module, Function, Arity, OptList)
    ).

-spec vdist_dop_spawn_request_tt() -> proper_types:type().
vdist_dop_spawn_request_tt() ->
    ?LET(
        {ReqId, From, GroupLeader, Module, Function, Arity, OptList, Token},
        {
            vterm_reference(),
            vterm_pid(),
            vterm_pid(),
            vterm_atom(),
            vterm_atom(),
            mostly(vterm_fixed_integer(0, 4), vterm_fixed_integer(0, 255)),
            list(vterm()),
            vterm()
        },
        vdist_dop_spawn_request_tt:new(ReqId, From, GroupLeader, Module, Function, Arity, OptList, Token)
    ).

-spec vdist_dop_unlink() -> proper_types:type().
vdist_dop_unlink() ->
    ?LET(
        {FromPid, ToPid},
        {vterm_pid(), vterm_pid()},
        vdist_dop_unlink:new(FromPid, ToPid)
    ).

-spec vdist_dop_unlink_id() -> proper_types:type().
vdist_dop_unlink_id() ->
    ?LET(
        {Id, FromPid, ToPid},
        {vterm_integer(1, (1 bsl 64) - 1), vterm_pid(), vterm_pid()},
        vdist_dop_unlink_id:new(Id, FromPid, ToPid)
    ).

-spec vdist_dop_unlink_id_ack() -> proper_types:type().
vdist_dop_unlink_id_ack() ->
    ?LET(
        {Id, FromPid, ToPid},
        {vterm_integer(1, (1 bsl 64) - 1), vterm_pid(), vterm_pid()},
        vdist_dop_unlink_id_ack:new(Id, FromPid, ToPid)
    ).

-spec vdist_entry() -> proper_types:type().
vdist_entry() ->
    ?LET(DFlags, vdist_distribution_flags(), vdist_entry:new(DFlags)).

-spec vdist_fragment_cont() -> proper_types:type().
vdist_fragment_cont() ->
    ?LET({SequenceId, FragmentId}, {u64(), u64()}, vdist_fragment_cont:new(SequenceId, FragmentId)).

-spec vdist_fragment_header() -> proper_types:type().
vdist_fragment_header() ->
    ?LET(
        {SequenceId, FragmentId, NumberOfAtomCacheRefs},
        {u64(), u64(), mostly(integer(0, 4), u8())},
        case NumberOfAtomCacheRefs of
            0 ->
                vdist_fragment_header:new(SequenceId, FragmentId, NumberOfAtomCacheRefs, [], false);
            _ ->
                ?LET(
                    AtomCacheRefEntries,
                    vector(
                        NumberOfAtomCacheRefs,
                        union([vdist_new_atom_cache_ref_entry(), vdist_old_atom_cache_ref_entry()])
                    ),
                    begin
                        LongAtoms = needs_long_atoms(AtomCacheRefEntries),
                        vdist_fragment_header:new(
                            SequenceId, FragmentId, NumberOfAtomCacheRefs, AtomCacheRefEntries, LongAtoms
                        )
                    end
                )
        end
    ).

-spec vdist_normal_header() -> proper_types:type().
vdist_normal_header() ->
    ?LET(
        NumberOfAtomCacheRefs,
        mostly(integer(0, 4), u8()),
        case NumberOfAtomCacheRefs of
            0 ->
                vdist_normal_header:new(NumberOfAtomCacheRefs, [], false);
            _ ->
                ?LET(
                    AtomCacheRefEntries,
                    vector(
                        NumberOfAtomCacheRefs,
                        union([vdist_new_atom_cache_ref_entry(), vdist_old_atom_cache_ref_entry()])
                    ),
                    begin
                        LongAtoms = needs_long_atoms(AtomCacheRefEntries),
                        vdist_normal_header:new(NumberOfAtomCacheRefs, AtomCacheRefEntries, LongAtoms)
                    end
                )
        end
    ).

-spec vdist_new_atom_cache_ref_entry() -> proper_types:type().
vdist_new_atom_cache_ref_entry() ->
    ?LET(
        {AtomCacheIndex, AtomText},
        {integer(0, 2038), ?LET(AtomLength, integer(0, 255), binary(AtomLength))},
        vdist_new_atom_cache_ref_entry:new(AtomCacheIndex, AtomText)
    ).

-spec vdist_old_atom_cache_ref_entry() -> proper_types:type().
vdist_old_atom_cache_ref_entry() ->
    ?LET(AtomCacheIndex, integer(0, 2038), vdist_old_atom_cache_ref_entry:new(AtomCacheIndex)).

-spec vdist_pass_through_header() -> proper_types:type().
vdist_pass_through_header() ->
    exactly(vdist_pass_through_header:new()).

-spec vdist_payload(vdist:control_message(), proper_vterm:options()) -> proper_types:type().
vdist_payload(#vdist_dop_spawn_request{arity = ArityVTerm}, Options) ->
    Arity = vterm:simplify(ArityVTerm),
    proper_vterm:vterm_proper_list(Arity, Options);
vdist_payload(#vdist_dop_spawn_request_tt{arity = ArityVTerm}, Options) ->
    Arity = vterm:simplify(ArityVTerm),
    proper_vterm:vterm_proper_list(Arity, Options);
vdist_payload(_ControlMessage, Options) ->
    proper_vterm:vterm(Options).

%% @private
needs_long_atoms([#vdist_new_atom_cache_ref_entry{atom_text = AtomText} | _]) when byte_size(AtomText) > 255 ->
    true;
needs_long_atoms([_H | T]) ->
    needs_long_atoms(T);
needs_long_atoms([]) ->
    false.
