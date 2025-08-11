%% NOTE: This file is imported from https://raw.githubusercontent.com/erlang/otp/refs/heads/maint-28/lib/kernel/src/inet6_tcp_dist.erl

%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1997-2025. All Rights Reserved.
%% Copyright (c) Meta Platforms, Inc. and affiliates.
%% Copyright (c) WhatsApp LLC
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(erldist_filter_otp_28_inet6_tcp_dist).
-moduledoc false.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("erldist_filter/include/erldist_filter_otp_28_net_address.hrl").

%% Handles the connection setup phase with other Erlang nodes.

-export([listen/2, accept/1, accept_connection/5,
         setup/5, close/1, select/1, address/0, is_node_name/1]).

-export([setopts/2, getopts/2]).

%% ------------------------------------------------------------
%%  Select this protocol based on node name
%%  select(Node) => Bool
%% ------------------------------------------------------------

-spec select(Node) -> boolean() when
    Node :: node().
select(Node) ->
    erldist_filter_otp_28_inet_tcp_dist:gen_select(inet6_tcp, Node).

%% ------------------------------------------------------------
%%  Get address family
%%  address() => #net_address{}
%% ------------------------------------------------------------

-spec address() -> Address :: #net_address{}.
address() ->
    erldist_filter_otp_28_inet_tcp_dist:gen_address(inet6_tcp).

%% ------------------------------------------------------------
%% Create the listen socket, i.e. the port that this erlang
%% node is accessible through.
%% ------------------------------------------------------------

-spec listen(Name, Host) ->
    {ok, {ListeningSocket, Address, Creation}}
    | {error, Reason}
when
    Name :: node(),
    Host :: inet:hostname(),
    ListeningSocket :: gen_tcp:socket(),
    Address :: #net_address{},
    Creation :: 1..16#FFFFFFFF,
    Reason :: system_limit | inet:posix().
listen(Name, Host) ->
    erldist_filter_otp_28_inet_tcp_dist:gen_listen(inet6_tcp, Name, Host).

%% ------------------------------------------------------------
%% Accepts new connection attempts from other Erlang nodes.
%% ------------------------------------------------------------

-spec accept(Listen) -> AcceptPid :: pid() when
    Listen :: gen_tcp:socket().
accept(Listen) ->
    erldist_filter_otp_28_inet_tcp_dist:gen_accept(inet6_tcp, Listen).

%% ------------------------------------------------------------
%% Accepts a new connection attempt from another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

-spec accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    ConnectionSupervisorPid :: pid()
when
    AcceptPid :: pid(),
    Socket :: gen_tcp:socket(),
    MyNode :: node(),
    Allowed :: list(),
    SetupTime :: non_neg_integer().
accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    erldist_filter_otp_28_inet_tcp_dist:gen_accept_connection(inet6_tcp, AcceptPid, Socket, MyNode, Allowed, SetupTime).

%% ------------------------------------------------------------
%% Setup a new connection to another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

-spec setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    ConnectionSupervisorPid :: pid()
when
    Node :: node(),
    Type :: atom(),
    MyNode :: node(),
    LongOrShortNames :: shortnames | longnames,
    SetupTime :: non_neg_integer().
setup(Node, Type, MyNode, LongOrShortNames,SetupTime) ->
    erldist_filter_otp_28_inet_tcp_dist:gen_setup(inet6_tcp, Node, Type, MyNode, LongOrShortNames, SetupTime).

%%
%% Close a socket.
%%
-spec close(Socket) -> ok when
    Socket :: gen_tcp:socket().
close(Socket) ->
    inet6_tcp:close(Socket).

-spec is_node_name(Node) -> boolean() when
    Node :: node().
is_node_name(Node) when is_atom(Node) ->
    erldist_filter_otp_28_inet_tcp_dist:is_node_name(Node).

-spec setopts(ListeningSocket, Options) -> ok | {error, Error} when
    ListeningSocket :: gen_tcp:socket(),
    Options :: [inet:socket_setopt()],
    Error :: inet:posix() | {badopts, Options}.
setopts(S, Opts) ->
    erldist_filter_otp_28_inet_tcp_dist:setopts(S, Opts).

-spec getopts(ListeningSocket, Options) ->
    {ok, OptionValues}
    | {error, Error}
when
    ListeningSocket :: gen_tcp:socket(),
    Options :: [inet:socket_getopt()],
    OptionValues :: [inet:socket_optval()],
    Error :: inet:posix().
getopts(S, Opts) ->
    erldist_filter_otp_28_inet_tcp_dist:getopts(S, Opts).
