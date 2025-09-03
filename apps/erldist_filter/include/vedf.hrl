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
-ifndef(VEDF_HRL).

-define(VEDF_HRL, 1).

-record(vedf_channel, {
    packet_size = 0 :: 0 | 1 | 2 | 4 | 8,
    dflags = 0 :: vterm:u64(),
    rx_sequences = maps:new() :: #{vdist:sequence_id() => vdist_external:t()},
    rx_atom_cache = undefined :: undefined | vdist_atom_cache:t(),
    rx_logger_time = 0 :: non_neg_integer(),
    rx_router_name :: atom(),
    rx_sort = 0 :: non_neg_integer(),
    compact_fragments = false :: boolean(),
    deep_packet_inspection = false :: boolean(),
    logging = false :: boolean(),
    otp_name_blocklist = false :: boolean(),
    redirect_dist_operations = false :: boolean(),
    sysname = undefined :: undefined | erldist_filter_nif:sysname(),
    untrusted = false :: boolean()
}).

-endif.
