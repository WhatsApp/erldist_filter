%%% %CopyrightBegin%
%%%
%%% Copyright Ericsson AB 1996-2022. All Rights Reserved.
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% %CopyrightEnd%
%%%
%%% % @format
%% @oncall whatsapp_clr
-ifndef(ERLDIST_FILTER_ERTS_DIST_HRL).

-define(ERLDIST_FILTER_ERTS_DIST_HRL, 1).

%%% See [erts/emulator/beam/dist.h](https://github.com/erlang/otp/blob/OTP-25.2.3/erts/emulator/beam/dist.h) in the
%%% Erlang/OTP source code.

-define(DFLAG_PUBLISHED, 16#01).
-define(DFLAG_ATOM_CACHE, 16#02).
-define(DFLAG_EXTENDED_REFERENCES, 16#04).
-define(DFLAG_DIST_MONITOR, 16#08).
-define(DFLAG_FUN_TAGS, 16#10).
-define(DFLAG_DIST_MONITOR_NAME, 16#20).
-define(DFLAG_HIDDEN_ATOM_CACHE, 16#40).
-define(DFLAG_NEW_FUN_TAGS, 16#80).
-define(DFLAG_EXTENDED_PIDS_PORTS, 16#100).
-define(DFLAG_EXPORT_PTR_TAG, 16#200).
-define(DFLAG_BIT_BINARIES, 16#400).
-define(DFLAG_NEW_FLOATS, 16#800).
-define(DFLAG_UNICODE_IO, 16#1000).
-define(DFLAG_DIST_HDR_ATOM_CACHE, 16#2000).
-define(DFLAG_SMALL_ATOM_TAGS, 16#4000).
-define(DFLAG_ETS_COMPRESSED, 16#8000).
-define(DFLAG_UTF8_ATOMS, 16#10000).
-define(DFLAG_MAP_TAG, 16#20000).
-define(DFLAG_BIG_CREATION, 16#40000).
-define(DFLAG_SEND_SENDER, 16#80000).
-define(DFLAG_BIG_SEQTRACE_LABELS, 16#100000).
-define(DFLAG_PENDING_CONNECT, 16#200000).
-define(DFLAG_EXIT_PAYLOAD, 16#400000).
-define(DFLAG_FRAGMENTS, 16#800000).
-define(DFLAG_HANDSHAKE_23, 16#1000000).
-define(DFLAG_UNLINK_ID, 16#2000000).
-define(DFLAG_MANDATORY_25_DIGEST, 16#4000000).
-define(DFLAG_RESERVED, 16#f8000000).
%%
%% As the old handshake only support 32 flag bits, we reserve the remaining
%% bits in the lower 32 for changes in the handshake protocol or potentially
%% new capabilities that we also want to backport to OTP-22 or older.
%%
-define(DFLAG_SPAWN, (16#1 bsl 32)).
-define(DFLAG_NAME_ME, (16#2 bsl 32)).
-define(DFLAG_V4_NC, (16#4 bsl 32)).
-define(DFLAG_ALIAS, (16#8 bsl 32)).
%%
%% In term_to_binary/2, we will use DFLAG_ATOM_CACHE to mean
%% DFLAG_DETERMINISTIC.
%%
-define(DFLAG_DETERMINISTIC, ?DFLAG_ATOM_CACHE).
%% Mandatory flags for distribution in OTP 25.
-define(DFLAG_DIST_MANDATORY_25,
    (?DFLAG_EXTENDED_REFERENCES bor ?DFLAG_FUN_TAGS bor ?DFLAG_EXTENDED_PIDS_PORTS bor ?DFLAG_UTF8_ATOMS bor
        ?DFLAG_NEW_FUN_TAGS bor
        ?DFLAG_BIG_CREATION bor ?DFLAG_NEW_FLOATS bor ?DFLAG_MAP_TAG bor ?DFLAG_EXPORT_PTR_TAG bor ?DFLAG_BIT_BINARIES bor
        ?DFLAG_BIT_BINARIES bor
        ?DFLAG_HANDSHAKE_23)
).
%% New mandatory flags for distribution in OTP 26
-define(DFLAG_DIST_MANDATORY_26, (?DFLAG_V4_NC bor ?DFLAG_UNLINK_ID)).
%% Mandatory flags for distribution.
-define(DFLAG_DIST_MANDATORY, (?DFLAG_DIST_MANDATORY_25 bor ?DFLAG_DIST_MANDATORY_26)).
%%
%% Additional optimistic flags when encoding toward pending connection.
%% If remote node (erl_interface) does not support these then we may need
%% to transcode messages enqueued before connection setup was finished.
%%
-define(DFLAG_DIST_HOPEFULLY, (?DFLAG_DIST_MONITOR bor ?DFLAG_DIST_MONITOR_NAME bor ?DFLAG_SPAWN bor ?DFLAG_ALIAS)).
%% Our preferred set of flags. Used for connection setup handshake
-define(DFLAG_DIST_DEFAULT,
    (?DFLAG_DIST_MANDATORY bor ?DFLAG_DIST_HOPEFULLY bor ?DFLAG_UNICODE_IO bor ?DFLAG_DIST_HDR_ATOM_CACHE bor
        ?DFLAG_SMALL_ATOM_TAGS bor
        ?DFLAG_SEND_SENDER bor ?DFLAG_BIG_SEQTRACE_LABELS bor ?DFLAG_EXIT_PAYLOAD bor ?DFLAG_FRAGMENTS bor ?DFLAG_SPAWN bor
        ?DFLAG_ALIAS bor
        ?DFLAG_MANDATORY_25_DIGEST)
).
%% Flags addable by local distr implementations
-define(DFLAG_DIST_ADDABLE, ?DFLAG_DIST_DEFAULT).
%% Flags rejectable by local distr implementation
-define(DFLAG_DIST_REJECTABLE,
    (?DFLAG_DIST_HDR_ATOM_CACHE bor ?DFLAG_HIDDEN_ATOM_CACHE bor ?DFLAG_FRAGMENTS bor ?DFLAG_ATOM_CACHE)
).
%% Flags for all features needing strict order delivery
-define(DFLAG_DIST_STRICT_ORDER, ?DFLAG_DIST_HDR_ATOM_CACHE).
%% All flags that should be enabled when term_to_binary/1 is used.
-define(TERM_TO_BINARY_DFLAGS, ?DFLAG_NEW_FLOATS).

%% opcodes used in distribution messages
% enum dop {
-define(DOP_LINK, 1).
-define(DOP_SEND, 2).
-define(DOP_EXIT, 3).
-define(DOP_UNLINK, 4).
%% Ancient DOP_NODE_LINK (5) was here, can be reused
-define(DOP_REG_SEND, 6).
-define(DOP_GROUP_LEADER, 7).
-define(DOP_EXIT2, 8).

-define(DOP_SEND_TT, 12).
-define(DOP_EXIT_TT, 13).
-define(DOP_REG_SEND_TT, 16).
-define(DOP_EXIT2_TT, 18).

-define(DOP_MONITOR_P, 19).
-define(DOP_DEMONITOR_P, 20).
-define(DOP_MONITOR_P_EXIT, 21).

-define(DOP_SEND_SENDER, 22).
-define(DOP_SEND_SENDER_TT, 23).

%% These are used when DFLAG_EXIT_PAYLOAD is detected
-define(DOP_PAYLOAD_EXIT, 24).
-define(DOP_PAYLOAD_EXIT_TT, 25).
-define(DOP_PAYLOAD_EXIT2, 26).
-define(DOP_PAYLOAD_EXIT2_TT, 27).
-define(DOP_PAYLOAD_MONITOR_P_EXIT, 28).

-define(DOP_SPAWN_REQUEST, 29).
-define(DOP_SPAWN_REQUEST_TT, 30).
-define(DOP_SPAWN_REPLY, 31).
-define(DOP_SPAWN_REPLY_TT, 32).

-define(DOP_ALIAS_SEND, 33).
-define(DOP_ALIAS_SEND_TT, 34).

-define(DOP_UNLINK_ID, 35).
-define(DOP_UNLINK_ID_ACK, 36).
% };

-define(ERTS_DIST_SPAWN_FLAG_LINK, (1 bsl 0)).
-define(ERTS_DIST_SPAWN_FLAG_MONITOR, (1 bsl 1)).

-endif.
