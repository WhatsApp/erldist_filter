%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(erldist_filter_nif).
-moduledoc """
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2022-09-20", modified => "2025-08-13"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-on_load(init/0).

%% NIF API
-export([
    altact_sig_flags/0,
    atom2cix/1,
    atom_text_get/2,
    atom_text_put/2,
    channel_open/5,
    channel_close/1,
    channel_inspect/1,
    channel_list/0,
    channel_list/1,
    channel_recv/2,
    channel_set_controlling_process/2,
    channel_set_tracing_process/2,
    config_get/0,
    config_get/1,
    config_set/1,
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
    otp_name_blocklist/0,
    otp_name_is_blocked/1,
    router_info/0,
    router_name/1,
    spawn_flags/0,
    version/0,
    world_stats_get/0
]).

-nifs([
    altact_sig_flags/0,
    atom2cix/1,
    atom_text_get/2,
    atom_text_put/2,
    channel_open/5,
    channel_close/1,
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
    otp_name_blocklist/0,
    otp_name_is_blocked/1,
    router_info/0,
    router_name/1,
    spawn_flags/0,
    version/0,
    world_stats_get/0
]).

%% Internal API
-export([
    init/0
]).

%% Types
-type action() :: action_emit() | action_log().
-type action_emit() :: {emit, binary()}.
-type action_log() :: {log, non_neg_integer(), {logger_time(), logger_event()}}.
-type channel() :: erlang:nif_resource().
-type connection_id() :: 0..16#ffffffff.
-type creation() :: 0..16#ffffffff.
-type distribution_flags() :: 0..16#ffffffffffffffff.
-type packet_size() :: 0 | 1 | 2 | 4 | 8.
-type sysname() :: node().
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
        stats := erldist_filter_nif_types:channel_stats(),
        vec_capacity := non_neg_integer(),
        vec_len := non_neg_integer()
    },
    tracing_process := undefined | pid()
}.
-type logger() :: erlang:nif_resource().
-type logger_event() :: {sysname(), logger_event_atoms(), logger_event_control(), logger_event_payload()}.
-type logger_event_atoms() :: tuple() | dynamic().
-type logger_event_control() :: undefined | binary().
-type logger_event_payload() :: undefined | binary().
-type logger_inspection() :: #{
    controlling_process := pid(),
    dropped := non_neg_integer(),
    received := non_neg_integer()
}.
-type logger_select_handle() :: reference().
-type logger_time() :: integer().
-type world_stat_group_channel() :: #{
    create := non_neg_integer(),
    destroy := non_neg_integer(),
    rx_stats := erldist_filter_nif_types:channel_stats()
}.
-type world_stat_group_memory() :: #{
    vec_own_bin_create := non_neg_integer(),
    vec_own_bin_create_capacity := non_neg_integer(),
    vec_own_bin_realloc := non_neg_integer(),
    vec_own_bin_realloc_capacity := non_neg_integer(),
    vec_own_bin_destroy := non_neg_integer(),
    vec_own_mem_create := non_neg_integer(),
    vec_own_mem_create_capacity := non_neg_integer(),
    vec_own_mem_realloc := non_neg_integer(),
    vec_own_mem_realloc_capacity := non_neg_integer(),
    vec_own_mem_destroy := non_neg_integer(),
    vec_ref_bin_create := non_neg_integer(),
    vec_ref_bin_destroy := non_neg_integer(),
    vec_ref_ioq_create := non_neg_integer(),
    vec_ref_ioq_destroy := non_neg_integer()
}.
-type router_info() :: #{
    count := pos_integer(),
    limit := pos_integer(),
    names := [atom()]
}.
-type world_stats() :: #{
    channel := world_stat_group_channel(),
    memory := world_stat_group_memory(),
    slots := non_neg_integer()
}.

-export_type([
    action/0,
    action_emit/0,
    action_log/0,
    channel/0,
    channel_inspection/0,
    connection_id/0,
    creation/0,
    distribution_flags/0,
    logger/0,
    logger_event/0,
    logger_event_atoms/0,
    logger_event_control/0,
    logger_event_payload/0,
    logger_inspection/0,
    logger_select_handle/0,
    logger_time/0,
    packet_size/0,
    router_info/0,
    sysname/0,
    world_stat_group_channel/0,
    world_stat_group_memory/0,
    world_stats/0
]).

%%%=============================================================================
%%% NIF API functions
%%%=============================================================================

-spec altact_sig_flags() -> erldist_filter_nif_types:altact_sig_flags().
altact_sig_flags() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec atom2cix(Atom) -> CacheIndex when Atom :: atom(), CacheIndex :: integer().
atom2cix(_Atom) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec atom_text_get(Atom, AtomEncoding) -> error | AtomText when
    Atom :: atom(), AtomEncoding :: latin1 | utf8, AtomText :: binary().
atom_text_get(_Atom, _AtomEncoding) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec atom_text_put(AtomText, AtomEncoding) -> Atom when
    AtomText :: binary(), AtomEncoding :: latin1 | utf8, Atom :: atom().
atom_text_put(_AtomText, _AtomEncoding) ->
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

-spec channel_inspect(Channel :: channel()) -> channel_inspection().
channel_inspect(_Channel) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec channel_list() -> [channel()].
channel_list() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec channel_list(Sysname) -> [channel()] when Sysname :: sysname().
channel_list(_Sysname) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec channel_recv(Channel :: channel(), Input :: binary() | erlang:iovec()) -> [action()].
channel_recv(_Channel, _Input) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec channel_set_controlling_process(Channel, NewOwnerPid) -> ok | {error, Reason} when
    Channel :: channel(), NewOwnerPid :: pid(), Reason :: closed | not_owner.
channel_set_controlling_process(_Channel, _NewOwnerPid) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec channel_set_tracing_process(Channel, NewTracePid) -> ok | {error, Reason} when
    Channel :: channel(), NewTracePid :: pid(), Reason :: closed | not_owner.
channel_set_tracing_process(_Channel, _NewTracePid) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec config_get() -> ConfigMap when
    ConfigMap :: erldist_filter_nif_types:config_map().
config_get() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec config_get(ConfigKey) -> ConfigVal when
    ConfigKey :: erldist_filter_nif_types:config_key(),
    ConfigVal :: boolean().
config_get(_ConfigKey) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec config_set(ConfigMapSet) -> ok when
    ConfigMapSet :: erldist_filter_nif_types:config_map_set().
config_set(ConfigMapSet) when is_map(ConfigMapSet) ->
    config_set_from_maps_iterator(maps:iterator(ConfigMapSet)).

-spec config_set(ConfigKey, ConfigVal) -> ok when
    ConfigKey :: erldist_filter_nif_types:config_key(),
    ConfigVal :: boolean().
config_set(_ConfigKey, _ConfigVal) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec dist_ext_to_term(AtomsTuple, InputBinary) -> Term when
    AtomsTuple :: logger_event_atoms(), InputBinary :: binary(), Term :: dynamic().
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

-spec distribution_flags() -> erldist_filter_nif_types:distribution_flags().
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

-spec otp_name_blocklist() -> BlockList when BlockList :: [atom()].
otp_name_blocklist() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec otp_name_is_blocked(Name) -> IsBlocked when Name :: atom(), IsBlocked :: boolean().
otp_name_is_blocked(_Name) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec router_info() -> RouterInfo when RouterInfo :: router_info().
router_info() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec router_name(Sysname) -> RouterName when
    Sysname :: sysname(),
    RouterName :: atom().
router_name(_Sysname) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec spawn_flags() -> erldist_filter_nif_types:spawn_flags().
spawn_flags() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec version() -> non_neg_integer().
version() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

-spec world_stats_get() -> world_stats().
world_stats_get() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-doc false.
-spec config_set_from_maps_iterator(Iterator) -> ok when
    Iterator :: maps:iterator(ConfigKey, ConfigVal),
    ConfigKey :: erldist_filter_nif_types:config_key(),
    ConfigVal :: boolean().
config_set_from_maps_iterator(Iterator) ->
    case maps:next(Iterator) of
        none ->
            ok;
        {ConfigKey, ConfigVal, NextIterator} ->
            ok = config_set(ConfigKey, ConfigVal),
            config_set_from_maps_iterator(NextIterator)
    end.

-doc false.
-spec init() -> ok | Error when
    Error :: {error, {Reason, Text :: string()}},
    Reason :: load_failed | bad_lib | load | reload | upgrade | old_code.
init() ->
    SoName = filename:join([erldist_filter:nif_dir(), ?MODULE_STRING]),
    erlang:load_nif(SoName, 0).
