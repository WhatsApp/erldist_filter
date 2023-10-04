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
%%% Created :  27 Sep 2022 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(erldist_filter_nif_spbt_shim).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(warn_missing_spec_all).

%% Shim API
-export([
    noop/1,
    channel_open/5,
    channel_close/1,
    channel_dop_with_payload/2,
    channel_dop_without_payload/2,
    channel_fill_atom_cache/2,
    channel_get_rx_atom_cache/1
]).

%% Types
-type channel() :: {channel, pid()}.

-export_type([
    channel/0
]).

%%%=============================================================================
%%% Shim API functions
%%%=============================================================================

-spec noop(Channel :: channel()) -> ok.
noop(_Channel) ->
    ok.

-spec channel_open(PacketSize, Sysname, Creation, ConnectionId, DistributionFlags) -> ChannelPid when
    PacketSize :: erldist_filter_nif:packet_size(),
    Sysname :: erldist_filter_nif:sysname(),
    Creation :: erldist_filter_nif:creation(),
    ConnectionId :: erldist_filter_nif:connection_id(),
    DistributionFlags :: erldist_filter_nif:distribution_flags(),
    ChannelPid :: pid().
channel_open(PacketSize, Sysname, Creation, ConnectionId, DistributionFlags) ->
    % Channel = erldist_filter_nif:channel_open(PacketSize, Sysname, Creation, ConnectionId, DistributionFlags),
    {ok, ChannelPid} = erldist_filter_proxy:start(PacketSize, Sysname, Creation, ConnectionId, DistributionFlags),
    ChannelPid.

-spec channel_close(Channel) -> ok | {error, Reason} when
    Channel :: channel(),
    Reason :: not_owner | closed.
channel_close({channel, Channel}) ->
    Result = erldist_filter_proxy:channel_close(Channel),
    Result.

-spec channel_dop_with_payload(Channel, Spec) -> [Action] | {error, Reason} when
    Channel :: channel(),
    Spec :: erldist_filter_nif_spbt_model_channel:dop_spec(),
    Action :: erldist_filter_nif:action(),
    Reason :: not_owner | closed.
channel_dop_with_payload({channel, Channel}, _Spec = #{packets := Packets}) ->
    Result = erldist_filter_proxy:channel_recv(Channel, Packets),
    should_log_result(Result) andalso io:format(user, "~s -> ~p~n", [?FUNCTION_NAME, Result]),
    Result.

-spec channel_dop_without_payload(Channel, Spec) -> [Action] | {error, Reason} when
    Channel :: channel(),
    Spec :: erldist_filter_nif_spbt_model_channel:dop_spec(),
    Action :: erldist_filter_nif:action(),
    Reason :: not_owner | closed.
channel_dop_without_payload({channel, Channel}, _Spec = #{packets := Packets}) ->
    Result = erldist_filter_proxy:channel_recv(Channel, Packets),
    should_log_result(Result) andalso io:format(user, "~s -> ~p~n", [?FUNCTION_NAME, Result]),
    Result.

-spec channel_fill_atom_cache(Channel, Spec) -> [Action] | {error, Reason} when
    Channel :: channel(),
    Spec :: erldist_filter_nif_spbt_model_channel:dop_spec(),
    Action :: erldist_filter_nif:action(),
    Reason :: not_owner | closed.
channel_fill_atom_cache({channel, Channel}, _Spec = #{packets := Packets}) ->
    Result = erldist_filter_proxy:channel_recv(Channel, Packets),
    should_log_result(Result) andalso io:format(user, "~s -> ~p~n", [?FUNCTION_NAME, Result]),
    Result.

-spec channel_get_rx_atom_cache(Channel) -> RxAtomCache when
    Channel :: channel(),
    RxAtomCache :: [{0..2047, atom()}].
channel_get_rx_atom_cache({channel, Channel}) ->
    #{rx := #{atom_cache := RxAtomCache}} = erldist_filter_proxy:channel_inspect(Channel),
    RxAtomCache.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec should_log_result(eqwalizer:dynamic()) -> boolean().
should_log_result({error, not_owner}) ->
    false;
should_log_result(Result) ->
    not is_list(Result).
