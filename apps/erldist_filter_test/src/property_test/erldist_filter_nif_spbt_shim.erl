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
-compile(nowarn_missing_spec).

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

%%%=============================================================================
%%% Shim API functions
%%%=============================================================================

noop(_Channel) ->
    ok.

channel_open(PacketSize, Sysname, Creation, ConnectionId, DistributionFlags) ->
    % Channel = erldist_filter_nif:channel_open(PacketSize, Sysname, Creation, ConnectionId, DistributionFlags),
    {ok, Channel} = erldist_filter_proxy:start(PacketSize, Sysname, Creation, ConnectionId, DistributionFlags),
    Channel.

channel_close({channel, Channel}) ->
    Result = erldist_filter_proxy:channel_close(Channel),
    Result.

channel_dop_with_payload({channel, Channel}, _Spec = #{packets := Packets}) ->
    Result = erldist_filter_proxy:channel_recv(Channel, Packets),
    should_log_result(Result) andalso io:format(user, "~s -> ~p~n", [?FUNCTION_NAME, Result]),
    Result.

channel_dop_without_payload({channel, Channel}, _Spec = #{packets := Packets}) ->
    Result = erldist_filter_proxy:channel_recv(Channel, Packets),
    should_log_result(Result) andalso io:format(user, "~s -> ~p~n", [?FUNCTION_NAME, Result]),
    Result.

channel_fill_atom_cache({channel, Channel}, _Spec = #{packets := Packets}) ->
    Result = erldist_filter_proxy:channel_recv(Channel, Packets),
    should_log_result(Result) andalso io:format(user, "~s -> ~p~n", [?FUNCTION_NAME, Result]),
    Result.

channel_get_rx_atom_cache({channel, Channel}) ->
    #{rx := #{atom_cache := RxAtomCache}} = erldist_filter_proxy:channel_inspect(Channel),
    RxAtomCache.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

should_log_result({error, not_owner}) ->
    false;
should_log_result(Result) ->
    not is_list(Result).
