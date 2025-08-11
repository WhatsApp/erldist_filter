%%% % @format
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
%%% Created :  19 Jun 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%% @oncall whatsapp_clr
-ifndef(VDIST_HRL).

-define(VDIST_HRL, 1).

-record(vdist_atom_cache, {
    entries = orddict:new() :: orddict:orddict(vdist_atom_cache:index(), atom())
}).

-record(vdist_atom_cache_map, {
    long_atoms = false :: boolean(),
    cache = vdist_atom_cache:new() :: vdist_atom_cache:t(),
    entries = maps:new() :: #{
        vdist_atom_cache:index() => {vdist_atom_cache_map:index(), atom(), vdist:atom_cache_ref_entry()}
    }
}).

-record(vdist_atom_translation_table, {
    entries = maps:new() :: #{vdist_atom_translation_table:index() => {vdist_atom_cache:index(), atom()}}
}).

-record(vdist_external, {
    atom_table = undefined :: vdist_atom_translation_table:t(),
    sequence_id = undefined :: vdist:sequence_id(),
    fragment_id_next = undefined :: vdist:fragment_id(),
    fragment_count = undefined :: vdist:fragment_id(),
    fragments = queue:new() :: queue:queue(binary())
}).

-record(vdist_entry, {
    dflags = 0 :: vterm:u64(),
    rx_sequences = maps:new() :: #{vdist:sequence_id() => vdist_external:t()},
    rx_atom_cache = undefined :: undefined | vdist_atom_cache:t(),
    tx_atom_cache = undefined :: undefined | vdist_atom_cache:t()
}).

-record(vdist_new_atom_cache_ref_entry, {
    atom_cache_index = 0 :: vdist:atom_cache_index(),
    atom_text = <<>> :: binary()
}).

-record(vdist_old_atom_cache_ref_entry, {
    atom_cache_index = 0 :: vdist:atom_cache_index()
}).

-record(vdist_normal_header, {
    number_of_atom_cache_refs = 0 :: vterm:u8(),
    atom_cache_ref_entries = [] :: [vdist:atom_cache_ref_entry()],
    long_atoms = false :: boolean()
}).

-record(vdist_fragment_header, {
    sequence_id = 0 :: vdist:sequence_id(),
    fragment_id = 0 :: vdist:fragment_id(),
    number_of_atom_cache_refs = 0 :: vterm:u8(),
    atom_cache_ref_entries = [] :: [vdist:atom_cache_ref_entry()],
    long_atoms = false :: boolean()
}).

-record(vdist_fragment_cont, {
    sequence_id = 0 :: vdist:sequence_id(),
    fragment_id = 0 :: vdist:fragment_id()
}).

-record(vdist_pass_through_header, {}).

-record(vdist_dop_alias_send, {
    from_pid = undefined :: vterm:pid_t(),
    alias = undefined :: vterm:reference_t()
}).

-record(vdist_dop_alias_send_tt, {
    from_pid = undefined :: vterm:pid_t(),
    alias = undefined :: vterm:reference_t(),
    token = undefined :: vterm:t()
}).

-record(vdist_dop_demonitor_p, {
    from_pid = undefined :: vterm:pid_t(),
    to_proc = undefined :: vterm:atom_t() | vterm:pid_t(),
    ref = undefined :: vterm:reference_t()
}).

-record(vdist_dop_exit, {
    from_pid = undefined :: vterm:pid_t(),
    to_pid = undefined :: vterm:pid_t(),
    reason = undefined :: vterm:t()
}).

-record(vdist_dop_exit2, {
    from_pid = undefined :: vterm:pid_t(),
    to_pid = undefined :: vterm:pid_t(),
    reason = undefined :: vterm:t()
}).

-record(vdist_dop_exit2_tt, {
    from_pid = undefined :: vterm:pid_t(),
    to_pid = undefined :: vterm:pid_t(),
    trace_token = undefined :: vterm:t(),
    reason = undefined :: vterm:t()
}).

-record(vdist_dop_exit_tt, {
    from_pid = undefined :: vterm:pid_t(),
    to_pid = undefined :: vterm:pid_t(),
    trace_token = undefined :: vterm:t(),
    reason = undefined :: vterm:t()
}).

-record(vdist_dop_group_leader, {
    from_pid = undefined :: vterm:pid_t(),
    to_pid = undefined :: vterm:pid_t()
}).

-record(vdist_dop_link, {
    from_pid = undefined :: vterm:pid_t(),
    to_pid = undefined :: vterm:pid_t()
}).

-record(vdist_dop_monitor_p, {
    from_pid = undefined :: vterm:pid_t(),
    to_proc = undefined :: vterm:atom_t() | vterm:pid_t(),
    ref = undefined :: vterm:reference_t()
}).

-record(vdist_dop_monitor_p_exit, {
    from_proc = undefined :: vterm:atom_t() | vterm:pid_t(),
    to_pid = undefined :: vterm:pid_t(),
    ref = undefined :: vterm:reference_t(),
    reason = undefined :: vterm:t()
}).

-record(vdist_dop_payload_exit, {
    from_pid = undefined :: vterm:pid_t(),
    to_pid = undefined :: vterm:pid_t()
}).

-record(vdist_dop_payload_exit2, {
    from_pid = undefined :: vterm:pid_t(),
    to_pid = undefined :: vterm:pid_t()
}).

-record(vdist_dop_payload_exit2_tt, {
    from_pid = undefined :: vterm:pid_t(),
    to_pid = undefined :: vterm:pid_t(),
    trace_token = undefined :: vterm:t()
}).

-record(vdist_dop_payload_exit_tt, {
    from_pid = undefined :: vterm:pid_t(),
    to_pid = undefined :: vterm:pid_t(),
    trace_token = undefined :: vterm:t()
}).

-record(vdist_dop_payload_monitor_p_exit, {
    from_proc = undefined :: vterm:atom_t() | vterm:pid_t(),
    to_pid = undefined :: vterm:pid_t(),
    ref = undefined :: vterm:reference_t()
}).

-record(vdist_dop_reg_send, {
    from_pid = undefined :: vterm:pid_t(),
    unused = undefined :: vterm:t(),
    to_name = undefined :: vterm:atom_t()
}).

-record(vdist_dop_reg_send_tt, {
    from_pid = undefined :: vterm:pid_t(),
    unused = undefined :: vterm:t(),
    to_name = undefined :: vterm:atom_t(),
    trace_token = undefined :: vterm:t()
}).

-record(vdist_dop_send, {
    unused = undefined :: vterm:t(),
    to_pid = undefined :: vterm:pid_t()
}).

-record(vdist_dop_send_sender, {
    from_pid = undefined :: vterm:pid_t(),
    to_pid = undefined :: vterm:pid_t()
}).

-record(vdist_dop_send_sender_tt, {
    from_pid = undefined :: vterm:pid_t(),
    to_pid = undefined :: vterm:pid_t(),
    trace_token = undefined :: vterm:t()
}).

-record(vdist_dop_send_tt, {
    unused = undefined :: vterm:t(),
    to_pid = undefined :: vterm:pid_t(),
    trace_token = undefined :: vterm:t()
}).

-record(vdist_dop_spawn_reply, {
    req_id = undefined :: vterm:reference_t(),
    to = undefined :: vterm:pid_t(),
    flags = undefined :: vterm:fixed_integer_t(),
    result = undefined :: vterm:atom_t() | vterm:pid_t()
}).

-record(vdist_dop_spawn_reply_tt, {
    req_id = undefined :: vterm:reference_t(),
    to = undefined :: vterm:pid_t(),
    flags = undefined :: vterm:fixed_integer_t(),
    result = undefined :: vterm:atom_t() | vterm:pid_t(),
    token = undefined :: vterm:t()
}).

-record(vdist_dop_spawn_request, {
    req_id = undefined :: vterm:reference_t(),
    from = undefined :: vterm:pid_t(),
    group_leader = undefined :: vterm:pid_t(),
    module = undefined :: vterm:atom_t(),
    function = undefined :: vterm:atom_t(),
    arity = undefined :: vterm:fixed_integer_t(),
    opt_list = undefined :: [vterm:t()]
}).

-record(vdist_dop_spawn_request_tt, {
    req_id = undefined :: vterm:reference_t(),
    from = undefined :: vterm:pid_t(),
    group_leader = undefined :: vterm:pid_t(),
    module = undefined :: vterm:atom_t(),
    function = undefined :: vterm:atom_t(),
    arity = undefined :: vterm:fixed_integer_t(),
    opt_list = undefined :: [vterm:t()],
    token = undefined :: vterm:t()
}).

-record(vdist_dop_unlink, {
    from_pid = undefined :: vterm:pid_t(),
    to_pid = undefined :: vterm:pid_t()
}).

-record(vdist_dop_unlink_id, {
    id = undefined :: vterm:integer_t(),
    from_pid = undefined :: vterm:pid_t(),
    to_pid = undefined :: vterm:pid_t()
}).

-record(vdist_dop_unlink_id_ack, {
    id = undefined :: vterm:integer_t(),
    from_pid = undefined :: vterm:pid_t(),
    to_pid = undefined :: vterm:pid_t()
}).

-define(is_vdist_dop_without_payload_t(T),
    (is_record(T, vdist_dop_demonitor_p) orelse
        is_record(T, vdist_dop_exit) orelse
        is_record(T, vdist_dop_exit2) orelse
        is_record(T, vdist_dop_exit2_tt) orelse
        is_record(T, vdist_dop_exit_tt) orelse
        is_record(T, vdist_dop_group_leader) orelse
        is_record(T, vdist_dop_link) orelse
        is_record(T, vdist_dop_monitor_p) orelse
        is_record(T, vdist_dop_monitor_p_exit) orelse
        is_record(T, vdist_dop_spawn_reply) orelse
        is_record(T, vdist_dop_spawn_reply_tt) orelse
        is_record(T, vdist_dop_unlink) orelse
        is_record(T, vdist_dop_unlink_id) orelse
        is_record(T, vdist_dop_unlink_id_ack))
).

-define(is_vdist_dop_with_payload_t(T),
    (is_record(T, vdist_dop_alias_send) orelse
        is_record(T, vdist_dop_alias_send_tt) orelse
        is_record(T, vdist_dop_payload_exit) orelse
        is_record(T, vdist_dop_payload_exit2) orelse
        is_record(T, vdist_dop_payload_exit2_tt) orelse
        is_record(T, vdist_dop_payload_exit_tt) orelse
        is_record(T, vdist_dop_payload_monitor_p_exit) orelse
        is_record(T, vdist_dop_reg_send) orelse
        is_record(T, vdist_dop_reg_send_tt) orelse
        is_record(T, vdist_dop_send) orelse
        is_record(T, vdist_dop_send_sender) orelse
        is_record(T, vdist_dop_send_sender_tt) orelse
        is_record(T, vdist_dop_send_tt) orelse
        is_record(T, vdist_dop_spawn_request) orelse
        is_record(T, vdist_dop_spawn_request_tt))
).

-define(is_vdist_dop_t(T),
    (?is_vdist_dop_without_payload_t(T) orelse ?is_vdist_dop_with_payload_t(T))
).

-endif.
