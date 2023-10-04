/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_ATOMS_H
#define EDF_ATOMS_H

#ifdef __cplusplus
extern "C" {
#endif

#include <erl_nif.h>

/* Type Definitions */

typedef struct erldist_filter_nif_atom_table_s erldist_filter_nif_atom_table_t;

struct erldist_filter_nif_atom_table_s {
    ERL_NIF_TERM ATOM_$gen_call;
    ERL_NIF_TERM ATOM_$gen_cast;
    ERL_NIF_TERM ATOM_EXIT;
    ERL_NIF_TERM ATOM_alias;
    ERL_NIF_TERM ATOM_atom_cache;
    ERL_NIF_TERM ATOM_atom_cache_overwrite_count;
    ERL_NIF_TERM ATOM_atom_cache_read_count;
    ERL_NIF_TERM ATOM_atom_cache_write_count;
    ERL_NIF_TERM ATOM_badarg;
    ERL_NIF_TERM ATOM_channel;
    ERL_NIF_TERM ATOM_closed;
    ERL_NIF_TERM ATOM_compact_external_count;
    ERL_NIF_TERM ATOM_compact_fragment_count;
    ERL_NIF_TERM ATOM_compact_fragments;
    ERL_NIF_TERM ATOM_connection_id;
    ERL_NIF_TERM ATOM_control_has_export_ext;
    ERL_NIF_TERM ATOM_control_has_new_fun_ext;
    ERL_NIF_TERM ATOM_controlling_process;
    ERL_NIF_TERM ATOM_count;
    ERL_NIF_TERM ATOM_create;
    ERL_NIF_TERM ATOM_creation;
    ERL_NIF_TERM ATOM_deep_packet_inspection;
    ERL_NIF_TERM ATOM_delete_child;
    ERL_NIF_TERM ATOM_destroy;
    ERL_NIF_TERM ATOM_dflags;
    ERL_NIF_TERM ATOM_dist_frag_cont_count;
    ERL_NIF_TERM ATOM_dist_frag_header_count;
    ERL_NIF_TERM ATOM_dist_header_count;
    ERL_NIF_TERM ATOM_dist_pass_through_count;
    ERL_NIF_TERM ATOM_dop_alias_send;
    ERL_NIF_TERM ATOM_dop_alias_send_tt;
    ERL_NIF_TERM ATOM_dop_demonitor_p;
    ERL_NIF_TERM ATOM_dop_exit;
    ERL_NIF_TERM ATOM_dop_exit2;
    ERL_NIF_TERM ATOM_dop_exit2_tt;
    ERL_NIF_TERM ATOM_dop_exit_tt;
    ERL_NIF_TERM ATOM_dop_group_leader;
    ERL_NIF_TERM ATOM_dop_link;
    ERL_NIF_TERM ATOM_dop_monitor_p;
    ERL_NIF_TERM ATOM_dop_monitor_p_exit;
    ERL_NIF_TERM ATOM_dop_payload_exit;
    ERL_NIF_TERM ATOM_dop_payload_exit2;
    ERL_NIF_TERM ATOM_dop_payload_exit2_tt;
    ERL_NIF_TERM ATOM_dop_payload_exit_tt;
    ERL_NIF_TERM ATOM_dop_payload_monitor_p_exit;
    ERL_NIF_TERM ATOM_dop_reg_send;
    ERL_NIF_TERM ATOM_dop_reg_send_tt;
    ERL_NIF_TERM ATOM_dop_send;
    ERL_NIF_TERM ATOM_dop_send_sender;
    ERL_NIF_TERM ATOM_dop_send_sender_tt;
    ERL_NIF_TERM ATOM_dop_send_tt;
    ERL_NIF_TERM ATOM_dop_spawn_reply;
    ERL_NIF_TERM ATOM_dop_spawn_reply_tt;
    ERL_NIF_TERM ATOM_dop_spawn_request;
    ERL_NIF_TERM ATOM_dop_spawn_request_tt;
    ERL_NIF_TERM ATOM_dop_unlink;
    ERL_NIF_TERM ATOM_dop_unlink_id;
    ERL_NIF_TERM ATOM_dop_unlink_id_ack;
    ERL_NIF_TERM ATOM_drop;
    ERL_NIF_TERM ATOM_drop_count;
    ERL_NIF_TERM ATOM_emit;
    ERL_NIF_TERM ATOM_emit_count;
    ERL_NIF_TERM ATOM_entry;
    ERL_NIF_TERM ATOM_error;
    ERL_NIF_TERM ATOM_false;
    ERL_NIF_TERM ATOM_fastpath;
    ERL_NIF_TERM ATOM_features_reply;
    ERL_NIF_TERM ATOM_features_request;
    ERL_NIF_TERM ATOM_io_reply;
    ERL_NIF_TERM ATOM_io_request;
    ERL_NIF_TERM ATOM_ioq_size;
    ERL_NIF_TERM ATOM_is_auth;
    ERL_NIF_TERM ATOM_latin1;
    ERL_NIF_TERM ATOM_limit;
    ERL_NIF_TERM ATOM_log;
    ERL_NIF_TERM ATOM_logging;
    ERL_NIF_TERM ATOM_memory;
    ERL_NIF_TERM ATOM_names;
    ERL_NIF_TERM ATOM_net_kernel;
    ERL_NIF_TERM ATOM_not_owner;
    ERL_NIF_TERM ATOM_notsup;
    ERL_NIF_TERM ATOM_ok;
    ERL_NIF_TERM ATOM_packet_count;
    ERL_NIF_TERM ATOM_packet_size;
    ERL_NIF_TERM ATOM_payload_has_export_ext;
    ERL_NIF_TERM ATOM_payload_has_new_fun_ext;
    ERL_NIF_TERM ATOM_redirect_dist_operations;
    ERL_NIF_TERM ATOM_restart_child;
    ERL_NIF_TERM ATOM_rewrite_fragment_header_count;
    ERL_NIF_TERM ATOM_rex;
    ERL_NIF_TERM ATOM_rollback_atom_cache_count;
    ERL_NIF_TERM ATOM_rx;
    ERL_NIF_TERM ATOM_rx_stats;
    ERL_NIF_TERM ATOM_seen;
    ERL_NIF_TERM ATOM_slots;
    ERL_NIF_TERM ATOM_slowpath;
    ERL_NIF_TERM ATOM_start_child;
    ERL_NIF_TERM ATOM_stats;
    ERL_NIF_TERM ATOM_sysname;
    ERL_NIF_TERM ATOM_system;
    ERL_NIF_TERM ATOM_terminate_child;
    ERL_NIF_TERM ATOM_tracing_process;
    ERL_NIF_TERM ATOM_true;
    ERL_NIF_TERM ATOM_try_again_restart;
    ERL_NIF_TERM ATOM_undefined;
    ERL_NIF_TERM ATOM_untrusted;
    ERL_NIF_TERM ATOM_utf8;
    ERL_NIF_TERM ATOM_vdist_dop_alias_send;
    ERL_NIF_TERM ATOM_vdist_dop_alias_send_tt;
    ERL_NIF_TERM ATOM_vdist_dop_demonitor_p;
    ERL_NIF_TERM ATOM_vdist_dop_exit;
    ERL_NIF_TERM ATOM_vdist_dop_exit2;
    ERL_NIF_TERM ATOM_vdist_dop_exit2_tt;
    ERL_NIF_TERM ATOM_vdist_dop_exit_tt;
    ERL_NIF_TERM ATOM_vdist_dop_group_leader;
    ERL_NIF_TERM ATOM_vdist_dop_link;
    ERL_NIF_TERM ATOM_vdist_dop_monitor_p;
    ERL_NIF_TERM ATOM_vdist_dop_monitor_p_exit;
    ERL_NIF_TERM ATOM_vdist_dop_payload_exit;
    ERL_NIF_TERM ATOM_vdist_dop_payload_exit2;
    ERL_NIF_TERM ATOM_vdist_dop_payload_exit2_tt;
    ERL_NIF_TERM ATOM_vdist_dop_payload_exit_tt;
    ERL_NIF_TERM ATOM_vdist_dop_payload_monitor_p_exit;
    ERL_NIF_TERM ATOM_vdist_dop_reg_send;
    ERL_NIF_TERM ATOM_vdist_dop_reg_send_tt;
    ERL_NIF_TERM ATOM_vdist_dop_send;
    ERL_NIF_TERM ATOM_vdist_dop_send_sender;
    ERL_NIF_TERM ATOM_vdist_dop_send_sender_tt;
    ERL_NIF_TERM ATOM_vdist_dop_send_tt;
    ERL_NIF_TERM ATOM_vdist_dop_spawn_reply;
    ERL_NIF_TERM ATOM_vdist_dop_spawn_reply_tt;
    ERL_NIF_TERM ATOM_vdist_dop_spawn_request;
    ERL_NIF_TERM ATOM_vdist_dop_spawn_request_tt;
    ERL_NIF_TERM ATOM_vdist_dop_unlink;
    ERL_NIF_TERM ATOM_vdist_dop_unlink_id;
    ERL_NIF_TERM ATOM_vdist_dop_unlink_id_ack;
    ERL_NIF_TERM ATOM_vec_capacity;
    ERL_NIF_TERM ATOM_vec_len;
    ERL_NIF_TERM ATOM_vec_own_bin_create;
    ERL_NIF_TERM ATOM_vec_own_bin_create_capacity;
    ERL_NIF_TERM ATOM_vec_own_bin_destroy;
    ERL_NIF_TERM ATOM_vec_own_bin_realloc;
    ERL_NIF_TERM ATOM_vec_own_bin_realloc_capacity;
    ERL_NIF_TERM ATOM_vec_own_mem_create;
    ERL_NIF_TERM ATOM_vec_own_mem_create_capacity;
    ERL_NIF_TERM ATOM_vec_own_mem_destroy;
    ERL_NIF_TERM ATOM_vec_own_mem_realloc;
    ERL_NIF_TERM ATOM_vec_own_mem_realloc_capacity;
    ERL_NIF_TERM ATOM_vec_ref_bin_create;
    ERL_NIF_TERM ATOM_vec_ref_bin_destroy;
    ERL_NIF_TERM ATOM_vec_ref_ioq_create;
    ERL_NIF_TERM ATOM_vec_ref_ioq_destroy;
    ERL_NIF_TERM ATOM_vterm_atom_cache_ref;
    ERL_NIF_TERM ATOM_vterm_atom_cache_ref_resolved;
    ERL_NIF_TERM ATOM_vterm_atom_ext;
    ERL_NIF_TERM ATOM_vterm_atom_utf8_ext;
    ERL_NIF_TERM ATOM_vterm_binary_ext;
    ERL_NIF_TERM ATOM_vterm_bit_binary_ext;
    ERL_NIF_TERM ATOM_vterm_export_ext;
    ERL_NIF_TERM ATOM_vterm_float_ext;
    ERL_NIF_TERM ATOM_vterm_integer_ext;
    ERL_NIF_TERM ATOM_vterm_large_big_ext;
    ERL_NIF_TERM ATOM_vterm_large_tuple_ext;
    ERL_NIF_TERM ATOM_vterm_lazy_term;
    ERL_NIF_TERM ATOM_vterm_list_ext;
    ERL_NIF_TERM ATOM_vterm_map_ext;
    ERL_NIF_TERM ATOM_vterm_new_float_ext;
    ERL_NIF_TERM ATOM_vterm_new_fun_ext;
    ERL_NIF_TERM ATOM_vterm_new_pid_ext;
    ERL_NIF_TERM ATOM_vterm_new_port_ext;
    ERL_NIF_TERM ATOM_vterm_new_reference_ext;
    ERL_NIF_TERM ATOM_vterm_newer_reference_ext;
    ERL_NIF_TERM ATOM_vterm_nif_term;
    ERL_NIF_TERM ATOM_vterm_nil_ext;
    ERL_NIF_TERM ATOM_vterm_pid_ext;
    ERL_NIF_TERM ATOM_vterm_port_ext;
    ERL_NIF_TERM ATOM_vterm_reference_ext;
    ERL_NIF_TERM ATOM_vterm_small_atom_ext;
    ERL_NIF_TERM ATOM_vterm_small_atom_utf8_ext;
    ERL_NIF_TERM ATOM_vterm_small_big_ext;
    ERL_NIF_TERM ATOM_vterm_small_integer_ext;
    ERL_NIF_TERM ATOM_vterm_small_tuple_ext;
    ERL_NIF_TERM ATOM_vterm_string_ext;
    ERL_NIF_TERM ATOM_vterm_the_non_value;
    ERL_NIF_TERM ATOM_vterm_v4_port_ext;
};

/* Global Variables */

extern erldist_filter_nif_atom_table_t *erldist_filter_nif_atom_table;

/* Macros */

#define ATOM(Id) erldist_filter_nif_atom_table->ATOM_##Id

/* Function Declarations */

extern void erldist_filter_nif_make_atoms(ErlNifEnv *env);

#ifdef __cplusplus
}
#endif

#endif
