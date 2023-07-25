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
%%% Created :  20 Sep 2022 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(erldist_filter_nif).
-compile(warn_missing_spec).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-wacov(ignore).

-on_load(init/0).

%% NIF API
-export([
    atom2cix/1,
    atom_text_table_list/0,
    atom_text_table_size/0,
    atom_text_inspect/1,
    atom_text_put_and_keep/2,
    atom_text_release/1,
    channel_open/5,
    channel_close/1,
    channel_index_get/0,
    channel_inspect/1,
    channel_list/0,
    channel_list/1,
    channel_recv/2,
    channel_set_controlling_process/2,
    channel_set_tracing_process/2,
    config_get/0,
    config_get/1,
    config_set/2,
    dist_ext_to_term/2,
    dist_ext_to_vdist/2,
    dist_ext_to_vterm/2,
    dist_ext_to_vterm/3,
    dist_int_to_vdist/2,
    dist_int_to_vterm/2,
    dist_int_to_vterm/3,
    distribution_flags/0,
    internal_hash/1,
    logger_open/0,
    logger_close/1,
    logger_inspect/1,
    logger_list/0,
    logger_recv/1,
    logger_set_capacity/1,
    logger_set_controlling_process/2,
    version_build_date/0,
    world_stats_get/0
]).

-nifs([
    atom2cix/1,
    atom_text_table_list/0,
    atom_text_table_size/0,
    atom_text_inspect/1,
    atom_text_put_and_keep/2,
    atom_text_release/1,
    channel_open/5,
    channel_close/1,
    channel_index_get/0,
    channel_inspect/1,
    channel_list/0,
    channel_list/1,
    channel_recv/2,
    channel_set_controlling_process/2,
    channel_set_tracing_process/2,
    config_get/0,
    config_get/1,
    config_set/2,
    dist_ext_to_term/2,
    dist_ext_to_vdist/2,
    dist_ext_to_vterm/2,
    dist_ext_to_vterm/3,
    dist_int_to_vdist/2,
    dist_int_to_vterm/2,
    dist_int_to_vterm/3,
    distribution_flags/0,
    internal_hash/1,
    logger_open/0,
    logger_close/1,
    logger_inspect/1,
    logger_list/0,
    logger_recv/1,
    logger_set_capacity/1,
    logger_set_controlling_process/2,
    world_stats_get/0
]).

%% Internal API
-export([
    init/0
]).

%% Types
-type action() :: {emit, binary()} | {log, non_neg_integer(), {logger_time(), logger_event()}}.
-type channel() :: erlang:nif_resource().
-type connection_id() :: 0..16#ffffffff.
-type creation() :: 0..16#ffffffff.
-type distribution_flags() :: 0..16#ffffffffffffffff.
-type packet_size() :: 0 | 1 | 2 | 4 | 8.
-type sysname() :: node().
-type dop_stats() :: #{
    seen := non_neg_integer(),
    emit := non_neg_integer(),
    drop := non_neg_integer()
}.
-type channel_stats() :: #{
    packet_count := non_neg_integer(),
    emit_count := non_neg_integer(),
    drop_count := non_neg_integer(),
    dist_header_count := non_neg_integer(),
    dist_frag_header_count := non_neg_integer(),
    dist_frag_cont_count := non_neg_integer(),
    dist_pass_through_count := non_neg_integer(),
    atom_cache_read_count := non_neg_integer(),
    atom_cache_write_count := non_neg_integer(),
    atom_cache_overwrite_count := non_neg_integer(),
    rewrite_fragment_header_count := non_neg_integer(),
    rollback_atom_cache_count := non_neg_integer(),
    compact_external_count := non_neg_integer(),
    compact_fragment_count := non_neg_integer(),
    control_has_export_ext := non_neg_integer(),
    control_has_new_fun_ext := non_neg_integer(),
    payload_has_export_ext := non_neg_integer(),
    payload_has_new_fun_ext := non_neg_integer(),
    dop_link := dop_stats(),
    dop_send := dop_stats(),
    dop_exit := dop_stats(),
    dop_unlink := dop_stats(),
    dop_reg_send := dop_stats(),
    dop_group_leader := dop_stats(),
    dop_exit2 := dop_stats(),
    dop_send_tt := dop_stats(),
    dop_exit_tt := dop_stats(),
    dop_reg_send_tt := dop_stats(),
    dop_exit2_tt := dop_stats(),
    dop_monitor_p := dop_stats(),
    dop_demonitor_p := dop_stats(),
    dop_monitor_p_exit := dop_stats(),
    dop_send_sender := dop_stats(),
    dop_send_sender_tt := dop_stats(),
    dop_payload_exit := dop_stats(),
    dop_payload_exit_tt := dop_stats(),
    dop_payload_exit2 := dop_stats(),
    dop_payload_exit2_tt := dop_stats(),
    dop_payload_monitor_p_exit := dop_stats(),
    dop_spawn_request := dop_stats(),
    dop_spawn_request_tt := dop_stats(),
    dop_spawn_reply := dop_stats(),
    dop_spawn_reply_tt := dop_stats(),
    dop_alias_send := dop_stats(),
    dop_alias_send_tt := dop_stats(),
    dop_unlink_id := dop_stats(),
    dop_unlink_id_ack := dop_stats()
}.
-type channel_index() :: #{
    rx_stats := channel_stats(),
    slots := non_neg_integer()
}.
-type channel_inspection() :: #{
    controlling_process := pid(),
    entry := #{
        connection_id := connection_id(),
        creation := creation(),
        dflags := distribution_flags(),
        sysname := sysname()
    },
    rx := #{
        atom_cache := [{0..2047, atom()}],
        ioq_size := non_neg_integer(),
        packet_size := non_neg_integer(),
        stats := channel_stats(),
        vec_capacity := non_neg_integer(),
        vec_len := non_neg_integer()
    },
    tracing_process := undefined | pid()
}.
-type logger() :: erlang:nif_resource().
-type logger_event() :: {sysname(), logger_event_atoms(), logger_event_control(), logger_event_payload()}.
-type logger_event_atoms() :: tuple() | eqwalizer:dynamic().
-type logger_event_control() :: undefined | binary().
-type logger_event_payload() :: undefined | binary().
-type logger_inspection() :: #{
    controlling_process := pid(),
    dropped := non_neg_integer(),
    received := non_neg_integer()
}.
-type logger_select_handle() :: reference().
-type logger_time() :: integer().
-type world_stats() :: #{
    channels_created := non_neg_integer(),
    channels_destroyed := non_neg_integer()
}.

-export_type([
    action/0,
    channel/0,
    channel_index/0,
    channel_inspection/0,
    channel_stats/0,
    connection_id/0,
    creation/0,
    distribution_flags/0,
    dop_stats/0,
    logger/0,
    logger_event/0,
    logger_event_atoms/0,
    logger_event_control/0,
    logger_event_payload/0,
    logger_inspection/0,
    logger_select_handle/0,
    logger_time/0,
    packet_size/0,
    sysname/0,
    world_stats/0
]).

%%%=============================================================================
%%% NIF API functions
%%%=============================================================================

-spec atom2cix(Atom) -> CacheIndex when Atom :: atom(), CacheIndex :: integer().
atom2cix(_Atom) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec atom_text_table_list() -> [atom()].
atom_text_table_list() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec atom_text_table_size() -> non_neg_integer().
atom_text_table_size() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec atom_text_inspect(Atom) -> ok when Atom :: atom().
atom_text_inspect(_Atom) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec atom_text_put_and_keep(AtomText, AtomEncoding) -> Atom when
    AtomText :: binary(), AtomEncoding :: latin1 | utf8, Atom :: atom().
atom_text_put_and_keep(_AtomText, _AtomEncoding) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec atom_text_release(Atom) -> ok when Atom :: atom().
atom_text_release(_Atom) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec channel_open(PacketSize, Sysname, Creation, ConnectionId, DistributionFlags) -> Channel when
    PacketSize :: packet_size(),
    Sysname :: sysname(),
    Creation :: creation(),
    ConnectionId :: connection_id(),
    DistributionFlags :: distribution_flags(),
    Channel :: channel().
channel_open(_PacketSize, _Sysname, _Creation, _ConnectionId, _DistributionFlags) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec channel_close(Channel :: channel()) -> ok | {error, closed | not_owner}.
channel_close(_Channel) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec channel_index_get() -> channel_index().
channel_index_get() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec channel_inspect(Channel :: channel()) -> channel_inspection().
channel_inspect(_Channel) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec channel_list() -> [channel()].
channel_list() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec channel_list(Sysname) -> [channel()] when Sysname :: sysname().
channel_list(_Sysname) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec channel_recv(Channel :: channel(), Iovec :: erlang:iovec()) -> [action()].
channel_recv(_Channel, _Iovec) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec channel_set_controlling_process(Channel, NewOwnerPid) -> ok | {error, Reason} when
    Channel :: channel(), NewOwnerPid :: pid(), Reason :: closed | not_owner.
channel_set_controlling_process(_Channel, _NewOwnerPid) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec channel_set_tracing_process(Channel, NewTracePid) -> ok | {error, Reason} when
    Channel :: channel(), NewTracePid :: pid(), Reason :: closed | not_owner.
channel_set_tracing_process(_Channel, _NewTracePid) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec config_get() ->
    #{
        compact_fragments := boolean(),
        deep_packet_inspection := boolean(),
        logging := boolean(),
        redirect_dist_operations := boolean()
    }.
config_get() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec config_get
    (compact_fragments) -> boolean();
    (deep_packet_inspection) -> boolean();
    (logging) -> boolean();
    (redirect_dist_operations) -> boolean().
config_get(_Key) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec config_set
    (compact_fragments, boolean()) -> ok;
    (deep_packet_inspection, boolean()) -> ok;
    (logging, boolean()) -> ok;
    (redirect_dist_operations, boolean()) -> ok.
config_set(_Key, _Val) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec dist_ext_to_term(AtomsTuple, InputBinary) -> Term when
    AtomsTuple :: logger_event_atoms(), InputBinary :: binary(), Term :: eqwalizer:dynamic().
dist_ext_to_term(_AtomsTuple, _InputBinary) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec dist_ext_to_vdist(AtomsTuple, InputBinary) -> VDist when
    AtomsTuple :: logger_event_atoms(), InputBinary :: binary(), VDist :: vdist:dop_t().
dist_ext_to_vdist(_AtomsTuple, _InputBinary) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec dist_ext_to_vterm(AtomsTuple, InputBinary) -> VTerm when
    AtomsTuple :: logger_event_atoms(), InputBinary :: binary(), VTerm :: vterm:t().
dist_ext_to_vterm(_AtomsTuple, _InputBinary) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec dist_ext_to_vterm(AtomsTuple, InputBinary, Limit) -> VTerm when
    AtomsTuple :: logger_event_atoms(), InputBinary :: binary(), Limit :: integer(), VTerm :: vterm:t().
dist_ext_to_vterm(_AtomsTuple, _InputBinary, _Limit) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec dist_int_to_vdist(AtomsTuple, InputBinary) -> VDist when
    AtomsTuple :: logger_event_atoms(), InputBinary :: binary(), VDist :: vdist:dop_t().
dist_int_to_vdist(_AtomsTuple, _InputBinary) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec dist_int_to_vterm(AtomsTuple, InputBinary) -> VTerm when
    AtomsTuple :: logger_event_atoms(), InputBinary :: binary(), VTerm :: vterm:t().
dist_int_to_vterm(_AtomsTuple, _InputBinary) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec dist_int_to_vterm(AtomsTuple, InputBinary, Limit) -> VTerm when
    AtomsTuple :: logger_event_atoms(), InputBinary :: binary(), Limit :: integer(), VTerm :: vterm:t().
dist_int_to_vterm(_AtomsTuple, _InputBinary, _Limit) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec distribution_flags() -> #{atom() => distribution_flags()}.
distribution_flags() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec internal_hash(Term) -> Hash when Term :: term(), Hash :: integer().
internal_hash(_Term) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec logger_open() -> Logger when Logger :: logger().
logger_open() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec logger_close(Logger) -> ok | {error, Reason} when
    Logger :: logger(), Reason :: closed | not_owner.
logger_close(_Logger) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec logger_inspect(Logger) -> logger_inspection() | {error, Reason} when
    Logger :: logger(), Reason :: closed.
logger_inspect(_Logger) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec logger_list() -> [logger()].
logger_list() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec logger_recv(Logger) -> Select | Batch | {error, Reason} when
    Logger :: logger(),
    Select :: {select, logger_select_handle()},
    Batch :: {Size, Drop, [Element]},
    Size :: non_neg_integer(),
    Drop :: non_neg_integer(),
    Element :: {Time, Event},
    Time :: logger_time(),
    Event :: logger_event(),
    Reason :: closed | not_owner.
logger_recv(_Logger) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec logger_set_capacity(NewCapacity) -> OldCapacity when
    NewCapacity :: non_neg_integer(),
    OldCapacity :: non_neg_integer().
logger_set_capacity(_NewCapacity) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec logger_set_controlling_process(Logger, NewOwnerPid) -> ok | {error, Reason} when
    Logger :: logger(),
    NewOwnerPid :: pid(),
    Reason :: closed | not_owner.
logger_set_controlling_process(_Logger, _NewOwnerPid) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

% TODO: This should be embedded in the NIF at build-time; see T151767026
-spec version_build_date() -> non_neg_integer().
version_build_date() ->
    20230617.

-spec world_stats_get() -> world_stats().
world_stats_get() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec init() -> ok | Error when
    Error :: {error, {Reason, Text :: string()}},
    Reason :: load_failed | bad_lib | load | reload | upgrade | old_code.
init() ->
    SoName = filename:join([erldist_filter:nif_dir(), ?MODULE_STRING]),
    erlang:load_nif(SoName, 0).
