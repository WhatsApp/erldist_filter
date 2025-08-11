%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% Created :  10 Aug 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
-module(erldist_filter_inet_tcp_dist).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-export([
    accept/1,
    accept_connection/5,
    address/0,
    close/1,
    getopts/2,
    is_node_name/1,
    listen/2,
    select/1,
    setopts/2,
    setup/5
]).

-compile(
    {inline, [
        accept/1,
        accept_connection/5,
        address/0,
        close/1,
        getopts/2,
        is_node_name/1,
        listen/2,
        select/1,
        setopts/2,
        setup/5
    ]}
).

%% Macros
-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 28).
-define(DELEGATE, erldist_filter_otp_28_inet_tcp_dist).
-include_lib("erldist_filter/include/erldist_filter_otp_28_net_address.hrl").
-else.
-define(DELEGATE, erldist_filter_otp_27_inet_tcp_dist).
-include_lib("erldist_filter/include/erldist_filter_otp_27_net_address.hrl").
-endif.
-else.
-error("Macro OTP_RELEASE must be defined.").
-endif.

-spec accept(Listen) -> AcceptPid :: pid() when
    Listen :: gen_tcp:socket().
accept(Listen) ->
    ?DELEGATE:accept(Listen).

-spec accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    ConnectionSupervisorPid :: pid()
when
    AcceptPid :: pid(),
    Socket :: gen_tcp:socket(),
    MyNode :: node(),
    Allowed :: list(),
    SetupTime :: non_neg_integer().
accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    ?DELEGATE:accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime).

-spec address() -> Address :: #net_address{}.
address() ->
    ?DELEGATE:address().

-spec close(Socket) -> ok when
    Socket :: gen_tcp:socket().
close(Socket) ->
    ?DELEGATE:close(Socket).

-spec getopts(ListeningSocket, Options) ->
    {ok, OptionValues}
    | {error, Error}
when
    ListeningSocket :: gen_tcp:socket(),
    Options :: [inet:socket_getopt()],
    OptionValues :: [inet:socket_optval()],
    Error :: inet:posix().
getopts(ListeningSocket, Options) ->
    ?DELEGATE:getopts(ListeningSocket, Options).

-spec is_node_name(Node) -> boolean() when
    Node :: node().
is_node_name(Node) when is_atom(Node) ->
    ?DELEGATE:is_node_name(Node).

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
    ?DELEGATE:listen(Name, Host).

-spec select(Node) -> boolean() when
    Node :: node().
select(Node) ->
    ?DELEGATE:select(Node).

-spec setopts(ListeningSocket, Options) -> ok | {error, Error} when
    ListeningSocket :: gen_tcp:socket(),
    Options :: [inet:socket_setopt()],
    Error :: inet:posix() | {badopts, Options}.
setopts(ListeningSocket, Options) ->
    ?DELEGATE:setopts(ListeningSocket, Options).

-spec setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    ConnectionSupervisorPid :: pid()
when
    Node :: node(),
    Type :: atom(),
    MyNode :: node(),
    LongOrShortNames :: shortnames | longnames,
    SetupTime :: non_neg_integer().
setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    ?DELEGATE:setup(Node, Type, MyNode, LongOrShortNames, SetupTime).
