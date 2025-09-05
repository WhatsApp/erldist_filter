%% NOTE: This file is imported from https://raw.githubusercontent.com/erlang/otp/refs/heads/maint-28/lib/kernel/src/inet_tcp_dist.erl
%% It has been modified to support using the erldist_filter_nif.

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
-module(erldist_filter_otp_28_inet_tcp_dist).
-moduledoc false.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("kernel/include/logger.hrl").

%% Handles the connection setup phase with other Erlang nodes.

-export([listen/1, listen/2, accept/1, accept_connection/5,
         setup/5, close/1, select/1, address/0, is_node_name/1]).

%% Optional
-export([setopts/2, getopts/2]).

%% Generalized dist API
-export([gen_listen/3, gen_accept/2, gen_accept_connection/6,
         gen_setup/6, gen_select/2, gen_address/1]).
-export([fam_select/2, fam_address/1, fam_listen/4, fam_setup/4]).
%% OTP internal (e.g ssl)
-export([gen_hs_data/2, nodelay/0]).

-export([merge_options/2, merge_options/3]).

%% erldist_filter specific functions
-export([sender/1, receiver/1]).

%% internal exports

-export([accept_loop/3,do_accept/7,do_setup/7,getstat/1,tick/2]).

-import(error_logger,[error_msg/2]).

-include_lib("erldist_filter/include/erldist_filter_otp_28_net_address.hrl").

% -include_lib("erldist_filter/include/erldist_filter_otp_28_dist.hrl").
-include_lib("erldist_filter/include/erldist_filter_otp_28_dist_util.hrl").

-define(DRIVER, inet_tcp).
-define(PROTOCOL, tcp).

%% ------------------------------------------------------------
%%  Select this protocol based on node name
%%  select(Node) => Bool
%% ------------------------------------------------------------

-spec select(Node) -> boolean() when
      Node :: node().
select(Node) ->
    gen_select(?DRIVER, Node).

-spec gen_select(Driver, Node) -> boolean() when
      Driver :: module(),
      Node :: node().
gen_select(Driver, Node) ->
    fam_select(Driver:family(), Node).

-spec fam_select(Family, Node) -> boolean() when
      Family :: inet:address_family(),
      Node :: node().
fam_select(Family, Node) ->
    case erldist_filter_otp_28_dist_util:split_node(Node) of
        {node, Name, Host} ->
            EpmdMod = net_kernel:epmd_module(),
            case
                call_epmd_function(
                  EpmdMod, address_please, [Name, Host, Family])
            of
                {ok, _Addr} -> true;
                {ok, _Addr, _Port, _Creation} -> true;
                _ -> false
            end;
        _ -> false
    end.

%% ------------------------------------------------------------
%% Get the address family that this distribution uses
%% ------------------------------------------------------------
-spec address() -> Address :: #net_address{}.
address() ->
    gen_address(?DRIVER).

-spec gen_address(Driver) -> Address when
      Driver :: module(),
      Address :: #net_address{}.
gen_address(Driver) ->
    fam_address(Driver:family()).

-spec fam_address(Family) -> Address when
      Family :: inet:address_family(),
      Address :: #net_address{}.
fam_address(Family) ->
    {ok, Host} = inet:gethostname(),
    #net_address{
       host = Host,
       protocol = ?PROTOCOL,
       family = Family
      }.

%% ------------------------------------------------------------
%% Set up the general fields in #hs_data{}
%% ------------------------------------------------------------
-spec gen_hs_data(Driver, Socket) -> HSData when
      Driver :: module(),
      Socket :: gen_tcp:socket(),
      HSData :: #hs_data{}.
gen_hs_data(Driver, Socket) ->
    %% The only thing Driver actually is used for is to
    %% implement non-blocking send of distribution tick
    Nodelay = nodelay(),
    #hs_data{
       socket = Socket,
       f_send = fun Driver:send/2,
       f_recv = fun Driver:recv/3,
       f_setopts_pre_nodeup =
           fun (S) ->
                   inet:setopts(
                     S,
                     [{active, false}, {packet, 4}, Nodelay])
           end,
       f_setopts_post_nodeup =
           fun (S) ->
                   inet:setopts(
                     S,
                     [{active, true}, {packet,4},
                      {deliver, port}, binary, Nodelay])
           end,
       f_getll    = fun inet:getll/1,
       mf_tick    = fun (S) -> ?MODULE:tick(Driver, S) end,
       mf_getstat = fun ?MODULE:getstat/1,
       mf_setopts = fun ?MODULE:setopts/2,
       mf_getopts = fun ?MODULE:getopts/2}.

%% ------------------------------------------------------------
%% Create the listen socket, i.e. the port that this erlang
%% node is accessible through.
%% ------------------------------------------------------------

%% Keep this function for third-party dist controllers reusing this API
-spec listen(Name) ->
                    {ok, {ListeningSocket, Address, Creation}} |
                    {error, Reason} when
      Name :: node(),
      ListeningSocket :: gen_tcp:socket(),
      Address :: #net_address{},
      Creation :: 1..16#FFFFFFFF,
      Reason :: system_limit | inet:posix().
listen(Name) ->
    {ok, Host} = inet:gethostname(),
    listen(Name, Host).

-spec listen(Name, Host) ->
                    {ok, {ListeningSocket, Address, Creation}} |
                    {error, Reason} when
      Name :: node(),
      Host :: inet:hostname(),
      ListeningSocket :: gen_tcp:socket(),
      Address :: #net_address{},
      Creation :: 1..16#FFFFFFFF,
      Reason :: system_limit | inet:posix().
listen(Name, Host) ->
    gen_listen(?DRIVER, Name, Host).

-spec gen_listen(Driver, Name, Host) ->
                    {ok, {ListeningSocket, Address, Creation}} |
                    {error, Reason} when
      Driver :: module(),
      Name :: node(),
      Host :: inet:hostname(),
      ListeningSocket :: gen_tcp:socket(),
      Address :: #net_address{},
      Creation :: 1..16#FFFFFFFF,
      Reason :: system_limit | inet:posix().
gen_listen(Driver, Name, Host) ->
    ForcedOptions = [{active, false}, {packet,2}, {nodelay, true}],
    ListenFun =
        fun (First, Last, ListenOptions) ->
                listen_loop(
                  Driver, First, Last,
                  dynamic_cast(merge_options(dynamic_cast(ListenOptions), ForcedOptions)))
        end,
    Family = Driver:family(),
    maybe
        %%
        {ok, {ListenSocket, Address, Creation}} ?=
            fam_listen(Family, Name, Host, ListenFun),
        NetAddress =
            #net_address{
               host = Host,
               protocol = ?PROTOCOL,
               family = Family,
               address = Address},
        {ok, {ListenSocket, NetAddress, Creation}}
    else
        Error = {error, _} ->
            Error
    end.

-spec listen_loop(Driver, First, Last, Options) ->
                    {ok, ListeningSocket} |
                    {error, Reason} when
      Driver :: module(),
      First :: inet:port_number(),
      Last :: inet:port_number(),
      Options :: [gen_tcp:listen_option()],
      ListeningSocket :: gen_tcp:socket(),
      Reason :: system_limit | inet:posix().
listen_loop(_Driver, First, Last, _Options) when First > Last ->
    {error,eaddrinuse};
listen_loop(Driver, First, Last, Options) ->
    case Driver:listen(First, Options) of
        {error, eaddrinuse} ->
            listen_loop(Driver, First+1, Last, Options);
        Other ->
            Other
    end.


-spec fam_listen(Family, Name, Host, ListenFun) ->
                    {ok, {ListeningSocket, Address, Creation}} |
                    {error, Reason} when
      Family :: inet:address_family(),
      Name :: node(),
      Host :: inet:hostname(),
      ListeningSocket :: gen_tcp:socket(),
      Address :: {inet:ip_address(), inet:port_number()},
      Creation :: 1..16#FFFFFFFF,
      Reason :: system_limit | inet:posix(),
      ListenFun :: fun((inet:port_number(), inet:port_number(), [gen_tcp:listen_option()]) ->
                           {ok, ListeningSocket} |
                           {error, Reason}).
fam_listen(Family, Name, Host, ListenFun) ->
    maybe
        EpmdMod = net_kernel:epmd_module(),
        %%
        {ok, ListenSocket} ?=
            case
                call_epmd_function(
                  EpmdMod, listen_port_please, [Name, Host])
            of
                {ok, 0} ->
                    {First,Last} = get_port_range(),
                    ListenFun(First, Last, listen_options());
                {ok, PortNum} ->
                    ListenFun(PortNum, PortNum, listen_options())
            end,
        {ok, {_IP,Port} = Address} = dynamic_cast(inet:sockname(ListenSocket)),
        %%
        {ok, Creation} ?=
            EpmdMod:register_node(Name, Port, Family),
        {ok, {ListenSocket, Address, Creation}}
    else
        Error = {error, _} ->
            Error
    end.

-spec get_port_range() -> {inet:port_number(), inet:port_number()}.
get_port_range() ->
    % elp:ignore W0011
    case application:get_env(kernel,inet_dist_listen_min) of
        {ok,N} when is_integer(N) ->
            % elp:ignore W0011
            case application:get_env(kernel,inet_dist_listen_max) of
                {ok,M} when is_integer(M) ->
                    {N,M};
                _ ->
                    {N,N}
            end;
        _ ->
            {0,0}
    end.


-spec listen_options() -> [gen_tcp:listen_option()].
listen_options() ->
    DefaultOpts = [{reuseaddr, true}, {backlog, 128}],
    ForcedOpts =
        % elp:ignore W0011
        case application:get_env(kernel, inet_dist_use_interface) of
            {ok, Ip}  -> [{ip, Ip}];
            undefined -> []
        end,
    InetDistListenOpts =
        % elp:ignore W0011
        case application:get_env(kernel, inet_dist_listen_options) of
            {ok, Opts} -> Opts;
            undefined  -> []
        end,
    dynamic_cast(merge_options(InetDistListenOpts, ForcedOpts, DefaultOpts)).


-spec merge_options(Opts, ForcedOpts) -> MergedOpts when
      Opts :: proplists:proplist(),
      ForcedOpts :: proplists:proplist(),
      MergedOpts :: proplists:proplist().
merge_options(Opts, ForcedOpts) ->
    merge_options(Opts, ForcedOpts, []).
%%
-spec merge_options(Opts, ForcedOpts, DefaultOpts) -> MergedOpts when
    Opts :: proplists:proplist(),
    ForcedOpts :: proplists:proplist(),
    DefaultOpts :: proplists:proplist(),
    MergedOpts :: proplists:proplist().
merge_options(Opts, ForcedOpts, DefaultOpts) ->
    Forced = merge_options(ForcedOpts),
    Default = merge_options(DefaultOpts),
    ForcedOpts ++ merge_options(Opts, Forced, DefaultOpts, Default).

%% Collect expanded 2-tuple options in a map
-spec merge_options(dynamic()) -> #{atom() => dynamic()}.
merge_options(Opts) ->
    lists:foldr(
      fun (Opt, Acc) ->
              case expand_option(Opt) of
                  {OptName, OptVal} ->
                      Acc#{OptName => OptVal};
                  _ ->
                      Acc
              end
      end, #{}, Opts).

%% Pass through all options that are not forced,
%% which we already have prepended,
%% and remove options that we see from the Default map
%%
-spec merge_options(Opts, Forced, DefaultOpts, Default) -> MergedOpts when
        Opts :: proplists:proplist(),
        Forced :: #{atom() => dynamic()},
        DefaultOpts :: proplists:proplist(),
        Default :: #{atom() => dynamic()},
        MergedOpts :: proplists:proplist().
merge_options([Opt | Opts], Forced, DefaultOpts, Default) ->
    case expand_option(dynamic_cast(Opt)) of
        {OptName, _} ->
            %% Remove from the Default map
            Default_1 = maps:remove(OptName, Default),
            if
                is_map_key(OptName, Forced) ->
                    %% Forced option - do not pass through
                    merge_options(Opts, Forced, DefaultOpts, Default_1);
                true ->
                    %% Pass through
                    [Opt |
                     merge_options(Opts, Forced, DefaultOpts, Default_1)]
            end;
        _ ->
            %% Unhandled options e.g {raw, ...} - pass through
            [Opt | merge_options(Opts, Forced, DefaultOpts, Default)]
    end;
merge_options([], _Forced, DefaultOpts, Default) ->
    %% Append the needed default options (that we have not seen)
    [Opt ||
        Opt <- DefaultOpts,
        is_map_key(element(1, expand_option(dynamic_cast(Opt))), Default)].

%% Expand an atom option into its tuple equivalence,
%% pass through others
-spec expand_option('binary' | 'inet' | 'inet6' | 'list' | 'local' | tuple()) -> {atom(), dynamic()} | tuple().
expand_option(Opt) ->
    if
        Opt =:= list; Opt =:= binary ->
            {mode, Opt};
        Opt =:= inet; Opt =:= inet6; Opt =:= local ->
            %% 'family' is not quite an option name, but could/should be
            {family, Opt};
        is_tuple(Opt) andalso tuple_size(Opt) >= 2 ->
            Opt
    end.

%% ------------------------------------------------------------
%% Accepts new connection attempts from other Erlang nodes.
%% ------------------------------------------------------------

-spec accept(Listen) -> AcceptPid :: pid() when
      Listen :: gen_tcp:socket().
accept(Listen) ->
    gen_accept(?DRIVER, Listen).

-spec gen_accept(Driver, Listen) -> AcceptPid :: pid() when
      Driver :: module(),
      Listen :: gen_tcp:socket().
gen_accept(Driver, Listen) ->
    case spawn_opt(?MODULE, accept_loop, [Driver, self(), Listen], [link, {priority, max}]) of
        AcceptPid when is_pid(AcceptPid) ->
            AcceptPid
    end.

-spec accept_loop(Driver, Kernel, Listen) -> no_return() when
      Driver :: module(),
      Kernel :: pid(),
      Listen :: gen_tcp:socket().
accept_loop(Driver, Kernel, Listen) ->
    case Driver:accept(Listen) of
        {ok, Socket} ->
            Kernel ! {accept,self(),Socket,Driver:family(),?PROTOCOL},
            _ = controller(Driver, Kernel, Socket),
            accept_loop(Driver, Kernel, Listen);
        Error ->
            exit(Error)
    end.

-spec controller(Driver, Kernel, Socket) -> Ignored | no_return() when
      Driver :: module(),
      Kernel :: pid(),
      Socket :: gen_tcp:socket(),
      Ignored :: term().
controller(Driver, Kernel, Socket) ->
    receive
        {Kernel, controller, Pid} ->
            flush_controller(Pid, Socket),
            Driver:controlling_process(Socket, Pid),
            flush_controller(Pid, Socket),
            Pid ! {self(), controller};
        {Kernel, unsupported_protocol} ->
            exit(unsupported_protocol)
    end.

-spec flush_controller(Pid, Socket) -> ok when
      Pid :: pid(),
      Socket :: gen_tcp:socket().
flush_controller(Pid, Socket) ->
    receive
        {tcp, Socket, Data} ->
            Pid ! {tcp, Socket, Data},
            flush_controller(Pid, Socket);
        {tcp_closed, Socket} ->
            Pid ! {tcp_closed, Socket},
            flush_controller(Pid, Socket)
    after 0 ->
            ok
    end.

%% ------------------------------------------------------------
%% Accepts a new connection attempt from another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

-spec accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
                               ConnectionSupervisorPid :: pid() when
      AcceptPid :: pid(),
      Socket :: gen_tcp:socket(),
      MyNode :: node(),
      Allowed :: list(),
      SetupTime :: non_neg_integer().
accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    gen_accept_connection(?DRIVER, AcceptPid, Socket, MyNode, Allowed, SetupTime).

-spec gen_accept_connection(Driver, AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
                               ConnectionSupervisorPid :: pid() when
      Driver :: module(),
      AcceptPid :: pid(),
      Socket :: gen_tcp:socket(),
      MyNode :: node(),
      Allowed :: list(),
      SetupTime :: non_neg_integer().
gen_accept_connection(Driver, AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    case spawn_opt(?MODULE, do_accept,
              [Driver, self(), AcceptPid, Socket, MyNode, Allowed, SetupTime],
              erldist_filter_otp_28_dist_util:net_ticker_spawn_options()) of
        ConnectionSupervisorPid when is_pid(ConnectionSupervisorPid) ->
            ConnectionSupervisorPid
    end.

-spec do_accept(Driver, Kernel, AcceptPid, Socket, MyNode, Allowed, SetupTime) -> no_return() when
      Driver :: module(),
      Kernel :: pid(),
      AcceptPid :: pid(),
      Socket :: gen_tcp:socket(),
      MyNode :: node(),
      Allowed :: [node()],
      SetupTime :: non_neg_integer().
do_accept(Driver, Kernel, AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    receive
        {AcceptPid, controller} ->
            Timer = erldist_filter_otp_28_dist_util:start_timer(SetupTime),
            case check_ip(Driver, Socket) of
                true ->
                    Sender = spawn_link(fun() -> sender(Driver) end),
                    Receiver = spawn_link(fun() -> receiver(Driver) end),
                    Family = Driver:family(),
                    HSData =
                        (gen_hs_data(Driver, Socket))
                        #hs_data{
                          kernel_pid = Kernel,
                          this_node = MyNode,
                          timer = Timer,
                          this_flags = 0,
                          allowed = Allowed,
                          f_address =
                              fun (S, Node) ->
                                      get_remote_id(Family, S, Node)
                              end,
                          f_getll = fun(_) -> {ok, Sender} end,
                          mf_tick = fun(_) -> Sender ! tick, ok end,
                          f_handshake_complete =
                              fun(FinSocket, _FinNode, FinDHandle, FinHSData) ->
                                      Sender ! {handshake_complete, FinSocket, FinDHandle, FinHSData, self(), Receiver},
                                      ok = receive {Receiver, req_socket_control} -> ok end,
                                      ok = Driver:controlling_process(FinSocket, Receiver),
                                      Receiver ! {self(), rep_socket_control, FinSocket},
                                      ok
                              end},
                    erldist_filter_otp_28_dist_util:handshake_other_started(HSData);
                {false,IP} ->
                    % elp:ignore W0053 (no_error_logger)
                    error_msg("** Connection attempt from "
                              "disallowed IP ~w ** ~n", [IP]),
                    ?shutdown(no_node)
            end
    end.


%% we may not always want the nodelay behaviour
%% for performance reasons

-spec nodelay() -> {nodelay, boolean()}.
nodelay() ->
    % elp:ignore W0011
    case application:get_env(kernel, dist_nodelay) of
        undefined ->
            {nodelay, true};
        {ok, true} ->
            {nodelay, true};
        {ok, false} ->
            {nodelay, false};
        _ ->
            {nodelay, true}
    end.

%% ------------------------------------------------------------
%% Get remote information about a Socket.
%% ------------------------------------------------------------
-spec get_remote_id(Family, Socket, Node) -> NetAddress | no_return() when
      Family :: inet | inet6,
      Socket :: gen_tcp:socket(),
      Node :: node(),
      NetAddress :: #net_address{}.
get_remote_id(Family, Socket, Node) ->
    case inet:peername(Socket) of
        {ok,Address} ->
            case split_node(atom_to_list(Node), $@, []) of
                [_,Host] ->
                    #net_address{address=Address,host=Host,
                                 protocol=?PROTOCOL,family=Family};
                _ ->
                    %% No '@' or more than one '@' in node name.
                    ?shutdown(no_node)
            end;
        {error, _Reason} ->
            ?shutdown(no_node)
    end.

%% ------------------------------------------------------------
%% Setup a new connection to another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

-spec setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
                   ConnectionSupervisorPid :: pid() when
      Node :: node(),
      Type :: atom(),
      MyNode :: node(),
      LongOrShortNames :: shortnames | longnames,
      SetupTime :: non_neg_integer().
setup(Node, Type, MyNode, LongOrShortNames,SetupTime) ->
    gen_setup(?DRIVER, Node, Type, MyNode, LongOrShortNames, SetupTime).

-spec gen_setup(Driver, Node, Type, MyNode, LongOrShortNames, SetupTime) ->
                   ConnectionSupervisorPid :: pid() when
      Driver :: module(),
      Node :: node(),
      Type :: atom(),
      MyNode :: node(),
      LongOrShortNames :: shortnames | longnames,
      SetupTime :: non_neg_integer().
gen_setup(Driver, Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    case spawn_opt(?MODULE, do_setup,
              [Driver, self(), Node, Type, MyNode, LongOrShortNames, SetupTime],
              erldist_filter_otp_28_dist_util:net_ticker_spawn_options()) of
        ConnectionSupervisorPid when is_pid(ConnectionSupervisorPid) ->
            ConnectionSupervisorPid
    end.

-spec do_setup(Driver, Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime) -> no_return() when
      Driver :: module(),
      Kernel :: pid(),
      Node :: node(),
      Type :: atom(),
      MyNode :: node(),
      LongOrShortNames :: shortnames | longnames,
      SetupTime :: non_neg_integer().
do_setup(Driver, Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    ?trace("~p~n",[{?MODULE,self(),setup,Node}]),
    Timer = erldist_filter_otp_28_dist_util:start_timer(SetupTime),
    Family = Driver:family(),
    {#net_address{ address = {Ip, TcpPort} } = NetAddress,
     ConnectOptions,
     Version} =
        fam_setup(
          Family, Node, LongOrShortNames, fun Driver:parse_address/1),
    erldist_filter_otp_28_dist_util:reset_timer(Timer),
    case Driver:connect(Ip, TcpPort, ConnectOptions) of
        {ok, Socket} ->
            Sender = spawn_link(fun() -> sender(Driver) end),
            Receiver = spawn_link(fun() -> receiver(Driver) end),
            HSData =
                (gen_hs_data(Driver, Socket))
                #hs_data{
                  kernel_pid = Kernel,
                  other_node = Node,
                  this_node = MyNode,
                  timer = Timer,
                  this_flags = 0,
                  other_version = Version,
                  f_address =
                      fun(_,_) ->
                              NetAddress
                      end,
                  request_type = Type,
                  f_getll = fun(_) -> {ok, Sender} end,
                  mf_tick = fun(_) -> Sender ! tick, ok end,
                  f_handshake_complete =
                      fun(FinSocket, _FinNode, FinDHandle, FinHSData) ->
                              Sender ! {handshake_complete, FinSocket, FinDHandle, FinHSData, self(), Receiver},
                              ok = receive {Receiver, req_socket_control} -> ok end,
                              ok = Driver:controlling_process(FinSocket, Receiver),
                             Receiver ! {self(), rep_socket_control, FinSocket},
                             ok
                    end},
            erldist_filter_otp_28_dist_util:handshake_we_started(HSData);
        _ ->
            %% Other Node may have closed since
            %% discovery !
            ?trace("other node (~p) "
                   "closed since discovery (port_please).~n",
                   [Node]),
            ?shutdown(Node)
    end.

-spec fam_setup(Family, Node, LongOrShortNames, ParseAddress) ->
                        {NetAddress, ConnectOptions, Version} when
      Family :: inet:address_family(),
      Node :: node(),
      LongOrShortNames :: shortnames | longnames,
      ParseAddress :: fun((inet:hostname()) -> {'ok', inet:ip_address()} | {'error', dynamic()}),
      NetAddress :: #net_address{},
      ConnectOptions :: [gen_tcp:connect_option()],
      Version :: non_neg_integer().
fam_setup(Family, Node, LongOrShortNames, ParseAddress) ->
    ?trace("~p~n",[{?MODULE,self(),?FUNCTION_NAME,Node}]),
    [Name, Host] = splitnode(ParseAddress, Node, LongOrShortNames),
    ErlEpmd = net_kernel:epmd_module(),
    case
        call_epmd_function(
          ErlEpmd, address_please, [Name, Host, Family])
    of
        {ok, Ip, TcpPort, Version} ->
            ?trace("address_please(~p) -> version ~p~n", [Node,Version]),
            fam_setup(Family, Host, Ip, TcpPort, Version);
        {ok, Ip} ->
            case ErlEpmd:port_please(Name, Ip) of
                {port, TcpPort, Version} ->
                    ?trace("port_please(~p) -> version ~p~n",
                           [Node,Version]),
                    fam_setup(Family, Host, Ip, TcpPort, Version);
                _ ->
                    ?trace("port_please (~p) failed.~n", [Node]),
                    ?shutdown(Node)
            end;
        _Other ->
            ?trace("inet_getaddr(~p) failed (~p).~n", [Node,_Other]),
            ?shutdown(Node)
    end.

-spec fam_setup(Family, Host, Ip, TcpPort, Version) -> {NetAddress, ConnectOptions, Version} when
        Family :: inet:address_family(),
        Host :: inet:hostname(),
        Ip :: inet:ip_address(),
        TcpPort :: inet:port_number(),
        Version :: non_neg_integer(),
        NetAddress :: #net_address{},
        ConnectOptions :: [gen_tcp:connect_option()].
fam_setup(Family, Host, Ip, TcpPort, Version) ->
    NetAddress =
        #net_address{
           address = {Ip, TcpPort},
           host = Host,
           protocol = ?PROTOCOL,
           family = Family},
    {NetAddress, connect_options(), Version}.

-spec connect_options() -> [gen_tcp:connect_option()].
connect_options() ->
    dynamic_cast(merge_options(
        % elp:ignore W0011
      case application:get_env(kernel, inet_dist_connect_options) of
          {ok, ConnectOpts} ->
              ConnectOpts;
          _ ->
              []
      end, [{active, false}, {packet, 2}])).


%%
%% Close a socket.
%%
-spec close(Socket) -> ok when
      Socket :: gen_tcp:socket().
close(Socket) ->
    ?DRIVER:close(Socket).


%% If Node is illegal terminate the connection setup!!
-spec splitnode(ParseAddress, Node, LongOrShortNames) -> [Name | Host] | no_return() when
      ParseAddress :: fun((inet:hostname()) -> {ok, inet:ip_address()} | {error, dynamic()}),
      Node :: node(),
      LongOrShortNames :: shortnames | longnames,
      Name :: dynamic(),
      Host :: dynamic().
splitnode(ParseAddress, Node, LongOrShortNames) ->
    case split_node(atom_to_list(Node), $@, []) of
        [Name|Tail] when Tail =/= [] ->
            Host = lists:append(Tail),
            case split_node(Host, $., []) of
                [_] when LongOrShortNames =:= longnames ->
                    case ParseAddress(Host) of
                        {ok, _} ->
                            [Name, Host];
                        _ ->
                            % elp:ignore W0053 (no_error_logger)
                            error_msg("** System running to use "
                                      "fully qualified "
                                      "hostnames **~n"
                                      "** Hostname ~ts is illegal **~n",
                                      [Host]),
                            ?shutdown(Node)
                    end;
                L when length(L) > 1, LongOrShortNames =:= shortnames ->
                    % elp:ignore W0053 (no_error_logger)
                    error_msg("** System NOT running to use fully qualified "
                              "hostnames **~n"
                              "** Hostname ~ts is illegal **~n",
                              [Host]),
                    ?shutdown(Node);
                _ ->
                    [Name, Host]
            end;
        [_] ->
            % elp:ignore W0053 (no_error_logger)
            error_msg("** Nodename ~p illegal, no '@' character **~n",
                      [Node]),
            ?shutdown(Node);
        _ ->
            % elp:ignore W0053 (no_error_logger)
            error_msg("** Nodename ~p illegal **~n", [Node]),
            ?shutdown(Node)
    end.

-spec split_node(String, Char, Ack) -> [Ack] when
      String :: string(),
      Char :: char(),
      Ack :: string().
split_node([Chr|T], Chr, Ack) -> [lists:reverse(Ack)|split_node(T, Chr, [])];
split_node([H|T], Chr, Ack)   -> split_node(T, Chr, [H|Ack]);
split_node([], _, Ack)        -> [lists:reverse(Ack)].

%% ------------------------------------------------------------
%% Determine if EPMD module supports the called functions.
%% If not call the builtin erl_epmd
%% ------------------------------------------------------------
-spec call_epmd_function(Mod, Fun, Args) -> Result when
      Mod :: module(),
      Fun :: atom(),
      Args :: [dynamic()],
      Result :: dynamic().
call_epmd_function(Mod, Fun, Args) ->
    case erlang:function_exported(Mod, Fun, length(Args)) of
        true -> apply(Mod,Fun,Args);
        _    -> apply(erl_epmd, Fun, Args)
    end.

%% ------------------------------------------------------------
%% Do only accept new connection attempts from nodes at our
%% own LAN, if the check_ip environment parameter is true.
%% ------------------------------------------------------------
-spec check_ip(Driver, Socket) -> true | {false, inet:ip_address()} | no_return() when
        Driver :: module(),
        Socket :: gen_tcp:socket().
check_ip(Driver, Socket) ->
    case application:get_env(check_ip) of
        {ok, true} ->
            case get_ifs(Socket) of
                {ok, IFs, IP} ->
                    check_ip(Driver, IFs, IP);
                _ ->
                    ?shutdown(no_node)
            end;
        _ ->
            true
    end.

-spec get_ifs(Socket) -> {ok, IFs, IP} | {error, inet:posix()} when
        Socket :: gen_tcp:socket(),
        IFs :: [{inet:ip_address(), inet:ip_address() | 'undefined', inet:ip_address()}],
        IP :: inet:ip_address().
get_ifs(Socket) ->
    case inet:peername(Socket) of
        {ok, {IP, _}} ->
            case inet:getif(Socket) of
                {ok, IFs} -> {ok, IFs, dynamic_cast(IP)};
                Error     -> Error
            end;
        Error ->
            Error
    end.

-spec check_ip(Driver, IFs, PeerIP) -> true | {false, PeerIP} when
        Driver :: module(),
        IFs :: [{inet:ip_address(), inet:ip_address() | 'undefined', inet:ip_address()}],
        PeerIP :: inet:ip_address().
check_ip(Driver, [{OwnIP, _, Netmask}|IFs], PeerIP) ->
    case {Driver:mask(Netmask, PeerIP), Driver:mask(Netmask, OwnIP)} of
        {M, M} -> true;
        _      -> check_ip(Driver, IFs, PeerIP)
    end;
check_ip(_Driver, [], PeerIP) ->
    {false, PeerIP}.

-spec is_node_name(Node) -> boolean() when
      Node :: node().
is_node_name(Node) when is_atom(Node) ->
    case split_node(atom_to_list(Node), $@, []) of
        [_, _Host] -> true;
        _ -> false
    end;
is_node_name(_Node) ->
    false.

-spec tick(Driver, Socket) -> ok | {error, closed | inet:posix()} when
      Driver :: module(),
      Socket :: gen_tcp:socket().
tick(Driver, Socket) ->
    case Driver:send(Socket, [], [force]) of
        {error, closed} ->
            self() ! {tcp_closed, Socket},
            {error, closed};
        R ->
            R
    end.

-spec getstat(Socket) -> {ok, RecvCnt, SendCnt, SendPend} | {error, inet:posix()} when
      Socket :: gen_tcp:socket(),
      RecvCnt :: integer(),
      SendCnt :: integer(),
      SendPend :: integer().
getstat(Socket) ->
    case inet:getstat(Socket, [recv_cnt, send_cnt, send_pend]) of
        {ok, Stat} ->
            split_stat(dynamic_cast(Stat),0,0,0);
        Error ->
            Error
    end.

-spec split_stat(Stat, RecvCnt, SendCnt, SendPend) -> {ok, RecvCnt, SendCnt, SendPend} when
      Stat :: [{recv_cnt, integer()} | {send_cnt, integer()} | {send_pend, integer()}],
      RecvCnt :: integer(),
      SendCnt :: integer(),
      SendPend :: integer().
split_stat([{recv_cnt, R}|Stat], _, W, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_cnt, W}|Stat], R, _, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_pend, P}|Stat], R, W, _) ->
    split_stat(Stat, R, W, P);
split_stat([], R, W, P) ->
    {ok, R, W, P}.


-spec setopts(ListeningSocket, Options) -> ok | {error, Error} when
      ListeningSocket :: gen_tcp:socket(),
      Options :: [inet:socket_setopt()],
      Error :: inet:posix() | {badopts, Options}.
setopts(S, Opts) ->
    case [Opt || {K,_}=Opt <- Opts,
                 K =:= active orelse K =:= deliver orelse K =:= packet] of
        [] -> inet:setopts(S,Opts);
        Opts1 -> {error, {badopts,Opts1}}
    end.

-spec getopts(ListeningSocket, Options) -> {ok, OptionValues} |
                                           {error, Error} when
      ListeningSocket :: gen_tcp:socket(),
      Options :: [inet:socket_getopt()],
      OptionValues :: [inet:socket_optval()],
      Error :: inet:posix().
getopts(S, Opts) ->
    inet:getopts(S, Opts).

-record(sender, {
    driver = undefined :: module(),
    socket = undefined :: inet:socket(),
    receiver = undefined :: pid(),
    dist_handle = undefined :: erlang:dist_handle(),
    dist_notify = false :: boolean(),
    dist_util = undefined :: pid()
}).

-spec sender(Driver) -> no_return() when
      Driver :: module().
sender(Driver) ->
    receive
        {handshake_complete, Socket, DistHandle, HSData, DistUtil, Receiver} ->
            ok = erlang:dist_ctrl_input_handler(DistHandle, Receiver),
            _ = erlang:dist_ctrl_set_opt(DistHandle, get_size, true),
            Receiver ! {handshake_complete, Socket, DistHandle, HSData, DistUtil, self()},
            State = #sender{
                driver = Driver,
                socket = Socket,
                receiver = Receiver,
                dist_handle = DistHandle,
                dist_notify = false,
                dist_util = DistUtil
            },
            sender_enter_loop(State)
    end.

-spec sender_enter_loop(State) -> no_return() when
        State :: #sender{}.
sender_enter_loop(State = #sender{receiver = Receiver}) ->
    receive
        {Receiver, receiver_ack} ->
            sender_before_loop(State)
    end.

-spec sender_before_loop(State) -> no_return() when
        State :: #sender{}.
sender_before_loop(State0 = #sender{dist_handle = DistHandle, dist_notify = false}) ->
    ok = erlang:dist_ctrl_get_data_notification(DistHandle),
    State1 = State0#sender{dist_notify = true},
    sender_before_loop(State1);
sender_before_loop(State) ->
    sender_loop(State).

-spec sender_loop(State) -> no_return() when
        State :: #sender{}.
sender_loop(State0 = #sender{driver = Driver, socket = Socket, dist_util = DistUtil}) ->
    receive
        dist_data ->
            State1 = State0#sender{dist_notify = false},
            DistData = sender_get_dist_data(State1, [], 0),
            case Driver:send(Socket, DistData) of
                {error, _} ->
                    DistUtil ! {tcp_closed, Socket},
                    Driver:close(Socket);
                ok ->
                    sender_before_loop(State1)
            end;
        tick ->
            case Driver:send(Socket, <<0:1/unsigned-big-integer-unit:32>>, [force]) of
                {error, _} ->
                    DistUtil ! {tcp_closed, Socket},
                    Driver:close(Socket);
                ok ->
                    sender_loop(State0)
            end;
        BadMsg ->
            exit({badmsg, BadMsg})
    end.

-spec sender_get_dist_data(State, Iolist, Size) -> Iolist when
        State :: #sender{},
        Iolist :: iolist(),
        Size :: non_neg_integer().
sender_get_dist_data(State = #sender{dist_handle = DistHandle}, Iolist, Size) ->
    case erlang:dist_ctrl_get_data(DistHandle) of
        {Len, Data} when (Size + Len) =< 16#FFFF ->
            sender_get_dist_data(State, [Iolist, <<Len:1/unsigned-big-integer-unit:32>>, Data], Size + 4 + Len);
        {Len, Data} ->
            [Iolist, <<Len:1/unsigned-big-integer-unit:32>>, Data];
        none ->
            Iolist
    end.

-record(receiver, {
    driver = undefined :: module(),
    socket = undefined :: inet:socket(),
    socket_notify = false :: boolean(),
    sender = undefined :: pid(),
    channel = undefined :: erldist_filter_nif:channel(),
    dist_handle = undefined :: erlang:dist_handle(),
    dist_util = undefined :: pid()
}).

-spec get_connection_id(erlang:dist_handle()) -> non_neg_integer().
get_connection_id(DistHandle) ->
    case dynamic_cast(DistHandle) of
        {ConnectionId, _} when is_integer(ConnectionId) andalso ConnectionId >= 0 ->
            ConnectionId;
        _ ->
            0
    end.

-spec receiver(Driver) -> no_return() when
      Driver :: module().
receiver(Driver) ->
    receive
        {handshake_complete, Socket, DistHandle, HSData, DistUtil, Sender} ->
            ConnectionId = get_connection_id(DistHandle),
            PacketSize = 4,
            Sysname = HSData#hs_data.other_node,
            Creation = HSData#hs_data.other_creation,
            DistFlags = HSData#hs_data.other_flags,
            Channel = erldist_filter_nif:channel_open(PacketSize, Sysname, Creation, ConnectionId, DistFlags),
            State = #receiver{
                driver = Driver,
                socket = Socket,
                socket_notify = false,
                sender = Sender,
                channel = Channel,
                dist_handle = DistHandle,
                dist_util = DistUtil
            },
            receiver_enter_loop(State)
    end.

-spec receiver_enter_loop(State) -> no_return() when
        State :: #receiver{}.
receiver_enter_loop(State = #receiver{socket = Socket, dist_util = DistUtil, sender = Sender}) ->
    ok = inet:setopts(Socket, [
        binary,
        {active, false},
        {deliver, term},
        {packet, raw},
        nodelay()
    ]),
    DistUtil ! {self(), req_socket_control},
    ok = receive {DistUtil, rep_socket_control, Socket} -> ok end,
    Sender ! {self(), receiver_ack},
    receiver_before_loop(State).

-spec receiver_before_loop(State) -> no_return() when
        State :: #receiver{}.
receiver_before_loop(State0 = #receiver{socket = Socket, socket_notify = false}) ->
    ok = inet:setopts(Socket, [{active, once}]),
    State1 = State0#receiver{socket_notify = true},
    receiver_before_loop(State1);
receiver_before_loop(State) ->
    receiver_loop(State).

-spec receiver_loop(State) -> no_return() when
        State :: #receiver{}.
receiver_loop(State0 = #receiver{socket = Socket, channel = Channel}) ->
    receive
        {tcp, Socket, Data} ->
            State1 = State0#receiver{socket_notify = false},
            Actions = erldist_filter_nif:channel_recv(Channel, [Data]),
            receiver_loop_actions(Actions, State1);
        BadMsg ->
            exit({badmsg, BadMsg})
    end.

-spec receiver_loop_actions(ActionList, State) -> no_return() when
        ActionList :: [Action],
        Action :: erldist_filter_nif:action(),
        State :: #receiver{}.
receiver_loop_actions([{emit, DistData} | Actions], State = #receiver{dist_handle = DistHandle}) ->
    ok = erlang:dist_ctrl_put_data(DistHandle, DistData),
    receiver_loop_actions(Actions, State);
receiver_loop_actions([{log, FragmentCount, {MonotonicTime, {Sysname, Atoms, Control0, Payload0}}} | Actions], State) ->
    Control = maybe_decode(Atoms, Control0),
    Payload = maybe_decode(Atoms, Payload0),
    ?LOG_DEBUG(#{
        fragment_count => FragmentCount,
        monotonic_time => MonotonicTime,
        sysname => Sysname,
        atoms => Atoms,
        control => Control,
        payload => Payload
    }),
    receiver_loop_actions(Actions, State);
receiver_loop_actions([], State) ->
    receiver_before_loop(State).

-spec maybe_decode(Atoms, Message) -> Term when
        Atoms :: dynamic(),
        Message :: undefined | binary(),
        Term :: dynamic().
maybe_decode(_Atoms, undefined) ->
    undefined;
maybe_decode(Atoms, Message) ->
    erts_debug:dist_ext_to_term(Atoms, Message).

-compile({inline, [dynamic_cast/1]}).
-spec dynamic_cast(term()) -> dynamic().
dynamic_cast(X) -> X.
