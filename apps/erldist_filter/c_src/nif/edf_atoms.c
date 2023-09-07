/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "edf_atoms.h"

/* Global Variables */

static erldist_filter_nif_atom_table_t erldist_filter_nif_atom_table_internal;
erldist_filter_nif_atom_table_t *erldist_filter_nif_atom_table = &erldist_filter_nif_atom_table_internal;

/* Function Definitions */

void
erldist_filter_nif_make_atoms(ErlNifEnv *env)
{
#define MAKE_ATOM(Id, Value)                                                                                                       \
    {                                                                                                                              \
        erldist_filter_nif_atom_table->ATOM_##Id = enif_make_atom(env, Value);                                                     \
    }
    MAKE_ATOM($gen_call, "$gen_call");
    MAKE_ATOM($gen_cast, "$gen_cast");
    MAKE_ATOM(EXIT, "EXIT");
    MAKE_ATOM(alias, "alias");
    MAKE_ATOM(atom_cache, "atom_cache");
    MAKE_ATOM(atom_cache_overwrite_count, "atom_cache_overwrite_count");
    MAKE_ATOM(atom_cache_read_count, "atom_cache_read_count");
    MAKE_ATOM(atom_cache_write_count, "atom_cache_write_count");
    MAKE_ATOM(badarg, "badarg");
    MAKE_ATOM(channel, "channel");
    MAKE_ATOM(closed, "closed");
    MAKE_ATOM(compact_external_count, "compact_external_count");
    MAKE_ATOM(compact_fragment_count, "compact_fragment_count");
    MAKE_ATOM(compact_fragments, "compact_fragments");
    MAKE_ATOM(connection_id, "connection_id");
    MAKE_ATOM(control_has_export_ext, "control_has_export_ext");
    MAKE_ATOM(control_has_new_fun_ext, "control_has_new_fun_ext");
    MAKE_ATOM(controlling_process, "controlling_process");
    MAKE_ATOM(count, "count");
    MAKE_ATOM(create, "create");
    MAKE_ATOM(creation, "creation");
    MAKE_ATOM(deep_packet_inspection, "deep_packet_inspection");
    MAKE_ATOM(delete_child, "delete_child");
    MAKE_ATOM(destroy, "destroy");
    MAKE_ATOM(dflags, "dflags");
    MAKE_ATOM(dist_frag_cont_count, "dist_frag_cont_count");
    MAKE_ATOM(dist_frag_header_count, "dist_frag_header_count");
    MAKE_ATOM(dist_header_count, "dist_header_count");
    MAKE_ATOM(dist_pass_through_count, "dist_pass_through_count");
    MAKE_ATOM(dop_alias_send, "dop_alias_send");
    MAKE_ATOM(dop_alias_send_tt, "dop_alias_send_tt");
    MAKE_ATOM(dop_demonitor_p, "dop_demonitor_p");
    MAKE_ATOM(dop_exit, "dop_exit");
    MAKE_ATOM(dop_exit2, "dop_exit2");
    MAKE_ATOM(dop_exit2_tt, "dop_exit2_tt");
    MAKE_ATOM(dop_exit_tt, "dop_exit_tt");
    MAKE_ATOM(dop_group_leader, "dop_group_leader");
    MAKE_ATOM(dop_link, "dop_link");
    MAKE_ATOM(dop_monitor_p, "dop_monitor_p");
    MAKE_ATOM(dop_monitor_p_exit, "dop_monitor_p_exit");
    MAKE_ATOM(dop_payload_exit, "dop_payload_exit");
    MAKE_ATOM(dop_payload_exit2, "dop_payload_exit2");
    MAKE_ATOM(dop_payload_exit2_tt, "dop_payload_exit2_tt");
    MAKE_ATOM(dop_payload_exit_tt, "dop_payload_exit_tt");
    MAKE_ATOM(dop_payload_monitor_p_exit, "dop_payload_monitor_p_exit");
    MAKE_ATOM(dop_reg_send, "dop_reg_send");
    MAKE_ATOM(dop_reg_send_tt, "dop_reg_send_tt");
    MAKE_ATOM(dop_send, "dop_send");
    MAKE_ATOM(dop_send_sender, "dop_send_sender");
    MAKE_ATOM(dop_send_sender_tt, "dop_send_sender_tt");
    MAKE_ATOM(dop_send_tt, "dop_send_tt");
    MAKE_ATOM(dop_spawn_reply, "dop_spawn_reply");
    MAKE_ATOM(dop_spawn_reply_tt, "dop_spawn_reply_tt");
    MAKE_ATOM(dop_spawn_request, "dop_spawn_request");
    MAKE_ATOM(dop_spawn_request_tt, "dop_spawn_request_tt");
    MAKE_ATOM(dop_unlink, "dop_unlink");
    MAKE_ATOM(dop_unlink_id, "dop_unlink_id");
    MAKE_ATOM(dop_unlink_id_ack, "dop_unlink_id_ack");
    MAKE_ATOM(drop, "drop");
    MAKE_ATOM(drop_count, "drop_count");
    MAKE_ATOM(emit, "emit");
    MAKE_ATOM(emit_count, "emit_count");
    MAKE_ATOM(entry, "entry");
    MAKE_ATOM(error, "error");
    MAKE_ATOM(false, "false");
    MAKE_ATOM(features_reply, "features_reply");
    MAKE_ATOM(features_request, "features_request");
    MAKE_ATOM(io_reply, "io_reply");
    MAKE_ATOM(io_request, "io_request");
    MAKE_ATOM(ioq_size, "ioq_size");
    MAKE_ATOM(is_auth, "is_auth");
    MAKE_ATOM(latin1, "latin1");
    MAKE_ATOM(limit, "limit");
    MAKE_ATOM(log, "log");
    MAKE_ATOM(logging, "logging");
    MAKE_ATOM(memory, "memory");
    MAKE_ATOM(names, "names");
    MAKE_ATOM(net_kernel, "net_kernel");
    MAKE_ATOM(not_owner, "not_owner");
    MAKE_ATOM(notsup, "notsup");
    MAKE_ATOM(ok, "ok");
    MAKE_ATOM(packet_count, "packet_count");
    MAKE_ATOM(packet_size, "packet_size");
    MAKE_ATOM(payload_has_export_ext, "payload_has_export_ext");
    MAKE_ATOM(payload_has_new_fun_ext, "payload_has_new_fun_ext");
    MAKE_ATOM(redirect_dist_operations, "redirect_dist_operations");
    MAKE_ATOM(restart_child, "restart_child");
    MAKE_ATOM(rewrite_fragment_header_count, "rewrite_fragment_header_count");
    MAKE_ATOM(rex, "rex");
    MAKE_ATOM(rollback_atom_cache_count, "rollback_atom_cache_count");
    MAKE_ATOM(rx, "rx");
    MAKE_ATOM(rx_stats, "rx_stats");
    MAKE_ATOM(seen, "seen");
    MAKE_ATOM(slots, "slots");
    MAKE_ATOM(start_child, "start_child");
    MAKE_ATOM(stats, "stats");
    MAKE_ATOM(sysname, "sysname");
    MAKE_ATOM(system, "system");
    MAKE_ATOM(terminate_child, "terminate_child");
    MAKE_ATOM(tracing_process, "tracing_process");
    MAKE_ATOM(true, "true");
    MAKE_ATOM(try_again_restart, "try_again_restart");
    MAKE_ATOM(undefined, "undefined");
    MAKE_ATOM(untrusted, "untrusted");
    MAKE_ATOM(utf8, "utf8");
    MAKE_ATOM(vdist_dop_alias_send, "vdist_dop_alias_send");
    MAKE_ATOM(vdist_dop_alias_send_tt, "vdist_dop_alias_send_tt");
    MAKE_ATOM(vdist_dop_demonitor_p, "vdist_dop_demonitor_p");
    MAKE_ATOM(vdist_dop_exit, "vdist_dop_exit");
    MAKE_ATOM(vdist_dop_exit2, "vdist_dop_exit2");
    MAKE_ATOM(vdist_dop_exit2_tt, "vdist_dop_exit2_tt");
    MAKE_ATOM(vdist_dop_exit_tt, "vdist_dop_exit_tt");
    MAKE_ATOM(vdist_dop_group_leader, "vdist_dop_group_leader");
    MAKE_ATOM(vdist_dop_link, "vdist_dop_link");
    MAKE_ATOM(vdist_dop_monitor_p, "vdist_dop_monitor_p");
    MAKE_ATOM(vdist_dop_monitor_p_exit, "vdist_dop_monitor_p_exit");
    MAKE_ATOM(vdist_dop_payload_exit, "vdist_dop_payload_exit");
    MAKE_ATOM(vdist_dop_payload_exit2, "vdist_dop_payload_exit2");
    MAKE_ATOM(vdist_dop_payload_exit2_tt, "vdist_dop_payload_exit2_tt");
    MAKE_ATOM(vdist_dop_payload_exit_tt, "vdist_dop_payload_exit_tt");
    MAKE_ATOM(vdist_dop_payload_monitor_p_exit, "vdist_dop_payload_monitor_p_exit");
    MAKE_ATOM(vdist_dop_reg_send, "vdist_dop_reg_send");
    MAKE_ATOM(vdist_dop_reg_send_tt, "vdist_dop_reg_send_tt");
    MAKE_ATOM(vdist_dop_send, "vdist_dop_send");
    MAKE_ATOM(vdist_dop_send_sender, "vdist_dop_send_sender");
    MAKE_ATOM(vdist_dop_send_sender_tt, "vdist_dop_send_sender_tt");
    MAKE_ATOM(vdist_dop_send_tt, "vdist_dop_send_tt");
    MAKE_ATOM(vdist_dop_spawn_reply, "vdist_dop_spawn_reply");
    MAKE_ATOM(vdist_dop_spawn_reply_tt, "vdist_dop_spawn_reply_tt");
    MAKE_ATOM(vdist_dop_spawn_request, "vdist_dop_spawn_request");
    MAKE_ATOM(vdist_dop_spawn_request_tt, "vdist_dop_spawn_request_tt");
    MAKE_ATOM(vdist_dop_unlink, "vdist_dop_unlink");
    MAKE_ATOM(vdist_dop_unlink_id, "vdist_dop_unlink_id");
    MAKE_ATOM(vdist_dop_unlink_id_ack, "vdist_dop_unlink_id_ack");
    MAKE_ATOM(vec_capacity, "vec_capacity");
    MAKE_ATOM(vec_len, "vec_len");
    MAKE_ATOM(vec_own_bin_create, "vec_own_bin_create");
    MAKE_ATOM(vec_own_bin_create_capacity, "vec_own_bin_create_capacity");
    MAKE_ATOM(vec_own_bin_destroy, "vec_own_bin_destroy");
    MAKE_ATOM(vec_own_bin_realloc, "vec_own_bin_realloc");
    MAKE_ATOM(vec_own_bin_realloc_capacity, "vec_own_bin_realloc_capacity");
    MAKE_ATOM(vec_own_mem_create, "vec_own_mem_create");
    MAKE_ATOM(vec_own_mem_create_capacity, "vec_own_mem_create_capacity");
    MAKE_ATOM(vec_own_mem_destroy, "vec_own_mem_destroy");
    MAKE_ATOM(vec_own_mem_realloc, "vec_own_mem_realloc");
    MAKE_ATOM(vec_own_mem_realloc_capacity, "vec_own_mem_realloc_capacity");
    MAKE_ATOM(vec_ref_bin_create, "vec_ref_bin_create");
    MAKE_ATOM(vec_ref_bin_destroy, "vec_ref_bin_destroy");
    MAKE_ATOM(vec_ref_ioq_create, "vec_ref_ioq_create");
    MAKE_ATOM(vec_ref_ioq_destroy, "vec_ref_ioq_destroy");
    MAKE_ATOM(vterm_atom_cache_ref, "vterm_atom_cache_ref");
    MAKE_ATOM(vterm_atom_cache_ref_resolved, "vterm_atom_cache_ref_resolved");
    MAKE_ATOM(vterm_atom_ext, "vterm_atom_ext");
    MAKE_ATOM(vterm_atom_utf8_ext, "vterm_atom_utf8_ext");
    MAKE_ATOM(vterm_binary_ext, "vterm_binary_ext");
    MAKE_ATOM(vterm_bit_binary_ext, "vterm_bit_binary_ext");
    MAKE_ATOM(vterm_export_ext, "vterm_export_ext");
    MAKE_ATOM(vterm_float_ext, "vterm_float_ext");
    MAKE_ATOM(vterm_integer_ext, "vterm_integer_ext");
    MAKE_ATOM(vterm_large_big_ext, "vterm_large_big_ext");
    MAKE_ATOM(vterm_large_tuple_ext, "vterm_large_tuple_ext");
    MAKE_ATOM(vterm_lazy_term, "vterm_lazy_term");
    MAKE_ATOM(vterm_list_ext, "vterm_list_ext");
    MAKE_ATOM(vterm_map_ext, "vterm_map_ext");
    MAKE_ATOM(vterm_new_float_ext, "vterm_new_float_ext");
    MAKE_ATOM(vterm_new_fun_ext, "vterm_new_fun_ext");
    MAKE_ATOM(vterm_new_pid_ext, "vterm_new_pid_ext");
    MAKE_ATOM(vterm_new_port_ext, "vterm_new_port_ext");
    MAKE_ATOM(vterm_new_reference_ext, "vterm_new_reference_ext");
    MAKE_ATOM(vterm_newer_reference_ext, "vterm_newer_reference_ext");
    MAKE_ATOM(vterm_nif_term, "vterm_nif_term");
    MAKE_ATOM(vterm_nil_ext, "vterm_nil_ext");
    MAKE_ATOM(vterm_pid_ext, "vterm_pid_ext");
    MAKE_ATOM(vterm_port_ext, "vterm_port_ext");
    MAKE_ATOM(vterm_reference_ext, "vterm_reference_ext");
    MAKE_ATOM(vterm_small_atom_ext, "vterm_small_atom_ext");
    MAKE_ATOM(vterm_small_atom_utf8_ext, "vterm_small_atom_utf8_ext");
    MAKE_ATOM(vterm_small_big_ext, "vterm_small_big_ext");
    MAKE_ATOM(vterm_small_integer_ext, "vterm_small_integer_ext");
    MAKE_ATOM(vterm_small_tuple_ext, "vterm_small_tuple_ext");
    MAKE_ATOM(vterm_string_ext, "vterm_string_ext");
    MAKE_ATOM(vterm_the_non_value, "vterm_the_non_value");
    MAKE_ATOM(vterm_v4_port_ext, "vterm_v4_port_ext");
#undef MAKE_ATOM
}
