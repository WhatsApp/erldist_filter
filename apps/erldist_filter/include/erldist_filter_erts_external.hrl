%%% % @format
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
%% @oncall whatsapp_clr
-ifndef(ERLDIST_FILTER_ERTS_EXTERNAL_HRL).

-define(ERLDIST_FILTER_ERTS_EXTERNAL_HRL, 1).

%%% See
%%% [erts/emulator/beam/external.h](https://github.com/erlang/otp/blob/OTP-25.2.3/erts/emulator/beam/external.h#L31-L82)
%%% in the Erlang/OTP source code.

-define(SMALL_INTEGER_EXT, $a).
-define(INTEGER_EXT, $b).
-define(FLOAT_EXT, $c).
-define(ATOM_EXT, $d).
-define(SMALL_ATOM_EXT, $s).
-define(REFERENCE_EXT, $e).
-define(NEW_REFERENCE_EXT, $r).
-define(NEWER_REFERENCE_EXT, $Z).
-define(PORT_EXT, $f).
-define(NEW_PORT_EXT, $Y).
-define(NEW_FLOAT_EXT, $F).
-define(PID_EXT, $g).
-define(NEW_PID_EXT, $X).
-define(SMALL_TUPLE_EXT, $h).
-define(LARGE_TUPLE_EXT, $i).
-define(NIL_EXT, $j).
-define(STRING_EXT, $k).
-define(LIST_EXT, $l).
-define(BINARY_EXT, $m).
-define(BIT_BINARY_EXT, $M).
-define(SMALL_BIG_EXT, $n).
-define(LARGE_BIG_EXT, $o).
-define(NEW_FUN_EXT, $p).
-define(EXPORT_EXT, $q).
-define(MAP_EXT, $t).
-define(FUN_EXT, $u).
-define(ATOM_UTF8_EXT, $v).
-define(SMALL_ATOM_UTF8_EXT, $w).
-define(V4_PORT_EXT, $x).

-define(DIST_HEADER, $D).
-define(DIST_FRAG_HEADER, $E).
-define(DIST_FRAG_CONT, $F).
%% -define(HOPEFUL_DATA, $H).
-define(ATOM_CACHE_REF, $R).
%% -define(ATOM_INTERNAL_REF2, $I).
%% -define(ATOM_INTERNAL_REF3, $K).
%% -define(BINARY_INTERNAL_REF, $J).
%% -define(BITSTRING_INTERNAL_REF, $L).
%% -define(MAGIC_REF_INTERNAL_REF, $N).
-define(COMPRESSED, $P).

-define(VERSION_MAGIC, 131).

-define(ERTS_ATOM_CACHE_SIZE, 2048).
-define(ERTS_USE_ATOM_CACHE_SIZE, 2039).
-define(ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES, 255).

-define(ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTE_IX(IIX), (((IIX) bsr 1) band 16#7fffffff)).
-define(ERTS_DIST_HDR_ATOM_CACHE_FLAG_BIT_IX(IIX), (((IIX) bsl 2) band 7)).
-define(ERTS_DIST_HDR_ATOM_CACHE_FLAG_BYTES(NO_ATOMS), ((((NO_ATOMS) bsr 1) band 16#7fffffff) + 1)).
-define(ERTS_DIST_HDR_LONG_ATOMS_FLG, (1 bsl 0)).

-define(PASS_THROUGH, $p).

-define(ERTS_MAX_REF_NUMBERS, 5).
-define(MAX_ARG, 255).

-define(is_atom_cache_index(IX), (is_integer(IX) andalso (IX) >= 0 andalso (IX) < ?ERTS_ATOM_CACHE_SIZE)).

-endif.
