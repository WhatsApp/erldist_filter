%% NOTE: This file is imported from https://raw.githubusercontent.com/erlang/otp/OTP-25.2.3/lib/kernel/src/inet_tcp_dist.erl
%% It has been modified to support using the erldist_filter_nif.

%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2022. All Rights Reserved.
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
-module(erldist_filter_otp_25_2_3_inet_tcp_dist).
-compile(warn_missing_spec).
-oncall("whatsapp_clr").
-wacov(ignore).

-include_lib("kernel/include/logger.hrl").

-dialyzer({nowarn_function, get_connection_id/1}).

%% Handles the connection setup phase with other Erlang nodes.

-export([listen/1, listen/2, accept/1, accept_connection/5,
	 setup/5, close/1, select/1, address/0, is_node_name/1]).

%% Optional
-export([setopts/2, getopts/2]).

%% Generalized dist API
-export([gen_listen/3, gen_accept/2, gen_accept_connection/6,
	 gen_setup/6, gen_select/2, gen_address/1]).

%% erldist_filter specific functions
-export([sender/1, receiver/1]).

%% internal exports

-export([accept_loop/3,do_accept/7,do_setup/7,getstat/1,tick/2]).

-import(error_logger,[error_msg/2]).

-include_lib("erldist_filter/include/erldist_filter_otp_25_2_3_net_address.hrl").

-include_lib("erldist_filter/include/erldist_filter_otp_25_2_3_dist.hrl").
-include_lib("erldist_filter/include/erldist_filter_otp_25_2_3_dist_util.hrl").

%% ------------------------------------------------------------
%%  Select this protocol based on node name
%%  select(Node) => Bool
%% ------------------------------------------------------------

-spec select(Node) -> boolean() when
      Node :: node().
select(Node) ->
    gen_select(inet_tcp, Node).

-spec gen_select(Driver, Node) -> boolean() when
      Driver :: module(),
      Node :: node().
gen_select(Driver, Node) ->
    case erldist_filter_otp_25_2_3_dist_util:split_node(Node) of
	{node, Name, Host} ->
            case call_epmd_function(
                   net_kernel:epmd_module(), address_please,
                   [Name, Host, Driver:family()]) of
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
    gen_address(inet_tcp).
-spec gen_address(Driver) -> Address when
      Driver :: module(),
      Address :: #net_address{}.
gen_address(Driver) ->
    get_tcp_address(Driver).

%% ------------------------------------------------------------
%% Create the listen socket, i.e. the port that this erlang
%% node is accessible through.
%% ------------------------------------------------------------

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
    gen_listen(inet_tcp, Name, Host).

-spec listen(Name) ->
                    {ok, {ListeningSocket, Address, Creation}} |
                    {error, Reason} when
      Name :: node(),
      ListeningSocket :: gen_tcp:socket(),
      Address :: #net_address{},
      Creation :: 1..16#FFFFFFFF,
      Reason :: system_limit | inet:posix().
%% Keep this clause for third-party dist controllers reusing this API
listen(Name) ->
    {ok, Host} = inet:gethostname(),
    listen(Name, Host).

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
    ErlEpmd = net_kernel:epmd_module(),
    case gen_listen(ErlEpmd, Name, Host, Driver) of
	{ok, Socket} ->
	    TcpAddress = get_tcp_address(Driver, Socket),
	    {_,Port} = TcpAddress#net_address.address,
	    case ErlEpmd:register_node(Name, Port, Driver) of
		{ok, Creation} ->
		    {ok, {Socket, TcpAddress, Creation}};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

gen_listen(ErlEpmd, Name, Host, Driver) ->
    ListenOptions = listen_options(),
    case call_epmd_function(ErlEpmd, listen_port_please, [Name, Host]) of
        {ok, 0} ->
            {First,Last} = get_port_range(),
            do_listen(Driver, First, Last, ListenOptions);
        {ok, Prt} ->
            do_listen(Driver, Prt, Prt, ListenOptions)
    end.

get_port_range() ->
    case application:get_env(kernel,inet_dist_listen_min) of
        {ok,N} when is_integer(N) ->
            case application:get_env(kernel,inet_dist_listen_max) of
                {ok,M} when is_integer(M) ->
                    {N,M};
                _ ->
                    {N,N}
            end;
        _ ->
            {0,0}
    end.

do_listen(_Driver, First,Last,_) when First > Last ->
    {error,eaddrinuse};
do_listen(Driver, First,Last,Options) ->
    case Driver:listen(First, Options) of
	{error, eaddrinuse} ->
	    do_listen(Driver, First+1,Last,Options);
	Other ->
	    Other
    end.

listen_options() ->
    DefaultOpts = [{reuseaddr, true}, {backlog, 128}],
    ForcedOpts =
        [{active, false}, {packet,2} |
         case application:get_env(kernel, inet_dist_use_interface) of
             {ok, Ip}  -> [{ip, Ip}];
             undefined -> []
         end],
    Force = maps:from_list(ForcedOpts),
    InetDistListenOpts =
        case application:get_env(kernel, inet_dist_listen_options) of
            {ok, Opts} -> Opts;
            undefined  -> []
        end,
    ListenOpts = listen_options(InetDistListenOpts, ForcedOpts, Force),
    Seen =
        maps:from_list(
          lists:filter(
            fun ({_,_}) -> true;
                (_)     -> false
            end, ListenOpts)),
    lists:filter(
      fun ({OptName,_}) when is_map_key(OptName, Seen) ->
              false;
          (_) ->
              true
      end, DefaultOpts) ++ ListenOpts.

%% Pass through all but forced
listen_options([Opt | Opts], ForcedOpts, Force) ->
    case Opt of
        {OptName,_} ->
            case is_map_key(OptName, Force) of
                true ->
                    listen_options(Opts, ForcedOpts, Force);
                false ->
                    [Opt |
                     listen_options(Opts, ForcedOpts, Force)]
            end;
        _ ->
            [Opt |
             listen_options(Opts, ForcedOpts, Force)]
    end;
listen_options([], ForcedOpts, _Force) ->
    %% Append forced
    ForcedOpts.


%% ------------------------------------------------------------
%% Accepts new connection attempts from other Erlang nodes.
%% ------------------------------------------------------------

-spec accept(Listen) -> AcceptPid :: pid() when
      Listen :: gen_tcp:socket().
accept(Listen) ->
    gen_accept(inet_tcp, Listen).

-spec gen_accept(Driver, Listen) -> AcceptPid :: pid() when
      Driver :: module(),
      Listen :: gen_tcp:socket().
gen_accept(Driver, Listen) ->
    case spawn_opt(?MODULE, accept_loop, [Driver, self(), Listen], [link, {priority, max}]) of
        Pid when is_pid(Pid) ->
            % workaround to get eqwalizer to calm down
            Pid
    end.

-spec accept_loop(Driver, Kernel, Listen) -> no_return() when
      Driver :: module(),
      Kernel :: pid(),
      Listen :: gen_tcp:socket().
accept_loop(Driver, Kernel, Listen) ->
    case Driver:accept(Listen) of
	{ok, Socket} ->
	    Kernel ! {accept,self(),Socket,Driver:family(),tcp},
	    _ = controller(Driver, Kernel, Socket),
	    accept_loop(Driver, Kernel, Listen);
	Error ->
	    exit(Error)
    end.

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
    gen_accept_connection(inet_tcp, AcceptPid, Socket, MyNode, Allowed, SetupTime).

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
	      erldist_filter_otp_25_2_3_dist_util:net_ticker_spawn_options()) of
        Pid when is_pid(Pid) ->
            % workaround to get eqwalizer to calm down
            Pid
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
	    Timer = erldist_filter_otp_25_2_3_dist_util:start_timer(SetupTime),
	    case check_ip(Driver, Socket) of
		true ->
		    Sender = spawn_link(fun() -> sender(Driver) end),
		    Receiver = spawn_link(fun() -> receiver(Driver) end),
		    HSData = #hs_data{
		      kernel_pid = Kernel,
		      this_node = MyNode,
		      socket = Socket,
		      timer = Timer,
		      this_flags = 0,
		      allowed = Allowed,
		      f_send = fun Driver:send/2,
		      f_recv = fun Driver:recv/3,
		      f_setopts_pre_nodeup =
		      fun(S) ->
			      inet:setopts(S,
					   [{active, false},
					    {packet, 4},
					    nodelay()])
		      end,
		      f_setopts_post_nodeup =
		      fun(S) ->
			      inet:setopts(S,
					   [{active, true},
					    {deliver, port},
					    {packet, 4},
                                            binary,
					    nodelay()])
		      end,
		      f_getll = fun(_) -> {ok, Sender} end,
		      f_address = fun(S, Node) -> get_remote_id(Driver, S, Node) end,
		      mf_tick = fun(_) -> Sender ! tick, ok end,
		      mf_getstat = fun ?MODULE:getstat/1,
		      mf_setopts = fun ?MODULE:setopts/2,
		      mf_getopts = fun ?MODULE:getopts/2,
		      f_handshake_complete = fun(FinSocket, _FinNode, FinDHandle, FinHSData) ->
			Sender ! {handshake_complete, FinSocket, FinDHandle, FinHSData, self(), Receiver},
			ok = receive {Receiver, req_socket_control} -> ok end,
			ok = Driver:controlling_process(FinSocket, Receiver),
			Receiver ! {self(), rep_socket_control, FinSocket},
			ok
		      end
		     },
		    erldist_filter_otp_25_2_3_dist_util:handshake_other_started(HSData);
		{false,IP} ->
		    error_msg("** Connection attempt from "
			      "disallowed IP ~w ** ~n", [IP]),
		    ?shutdown(no_node)
	    end
    end.


%% we may not always want the nodelay behaviour
%% for performance reasons

nodelay() ->
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
get_remote_id(Driver, Socket, Node) ->
    case inet:peername(Socket) of
	{ok,Address} ->
	    case split_node(atom_to_list(Node), $@, []) of
		[_,Host] ->
		    #net_address{address=Address,host=Host,
				 protocol=tcp,family=Driver:family()};
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
    gen_setup(inet_tcp, Node, Type, MyNode, LongOrShortNames, SetupTime).

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
	      erldist_filter_otp_25_2_3_dist_util:net_ticker_spawn_options()) of
        Pid when is_pid(Pid) ->
            % workaround to get eqwalizer to calm down
            Pid
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
    ?trace("~p~n",[{inet_tcp_dist,self(),setup,Node}]),
    [Name, Address] = splitnode(Driver, Node, LongOrShortNames),
    AddressFamily = Driver:family(),
    ErlEpmd = net_kernel:epmd_module(),
    Timer = erldist_filter_otp_25_2_3_dist_util:start_timer(SetupTime),
    case call_epmd_function(ErlEpmd,address_please,[Name, Address, AddressFamily]) of
	{ok, Ip, TcpPort, Version} ->
		?trace("address_please(~p) -> version ~p~n",
			[Node,Version]),
		do_setup_connect(Driver, Kernel, Node, Address, AddressFamily,
		                 Ip, TcpPort, Version, Type, MyNode, Timer);
	{ok, Ip} ->
	    case ErlEpmd:port_please(Name, Ip) of
		{port, TcpPort, Version} ->
		    ?trace("port_please(~p) -> version ~p~n",
			   [Node,Version]),
			do_setup_connect(Driver, Kernel, Node, Address, AddressFamily,
			                 Ip, TcpPort, Version, Type, MyNode, Timer);
		_ ->
		    ?trace("port_please (~p) failed.~n", [Node]),
		    ?shutdown(Node)
	    end;
	_Other ->
	    ?trace("inet_getaddr(~p) "
		   "failed (~p).~n", [Node,_Other]),
	    ?shutdown(Node)
    end.

%%
%% Actual setup of connection
%%
do_setup_connect(Driver, Kernel, Node, Address, AddressFamily,
                 Ip, TcpPort, Version, Type, MyNode, Timer) ->
	erldist_filter_otp_25_2_3_dist_util:reset_timer(Timer),
	case
	Driver:connect(
	  Ip, TcpPort,
	  connect_options([{active, false}, {packet, 2}]))
	of
	{ok, Socket} ->
		Sender = spawn_link(fun() -> sender(Driver) end),
		Receiver = spawn_link(fun() -> receiver(Driver) end),
		HSData = #hs_data{
		  kernel_pid = Kernel,
		  other_node = Node,
		  this_node = MyNode,
		  socket = Socket,
		  timer = Timer,
		  this_flags = 0,
		  other_version = Version,
		  f_send = fun Driver:send/2,
		  f_recv = fun Driver:recv/3,
		  f_setopts_pre_nodeup = fun(_) -> ok end,
		  f_setopts_post_nodeup = fun (_) -> ok end,
		  f_getll = fun(_) -> {ok, Sender} end,
		  f_address =
		  fun(_,_) ->
			  #net_address{
		   address = {Ip,TcpPort},
		   host = Address,
		   protocol = tcp,
		   family = AddressFamily}
		  end,
		  mf_tick = fun(_) -> Sender ! tick, ok end,
		  mf_getstat = fun ?MODULE:getstat/1,
		  request_type = Type,
		  mf_setopts = fun ?MODULE:setopts/2,
		  mf_getopts = fun ?MODULE:getopts/2,
		  f_handshake_complete = fun(FinSocket, _FinNode, FinDHandle, FinHSData) ->
		      Sender ! {handshake_complete, FinSocket, FinDHandle, FinHSData, self(), Receiver},
		      ok = receive {Receiver, req_socket_control} -> ok end,
		      ok = Driver:controlling_process(FinSocket, Receiver),
		      Receiver ! {self(), rep_socket_control, FinSocket},
		      ok
		  end
		 },
		erldist_filter_otp_25_2_3_dist_util:handshake_we_started(HSData);
	_ ->
		%% Other Node may have closed since
		%% discovery !
		?trace("other node (~p) "
		   "closed since discovery (port_please).~n",
		   [Node]),
		?shutdown(Node)
	end.

connect_options(Opts) ->
    case application:get_env(kernel, inet_dist_connect_options) of
	{ok,ConnectOpts} ->
	    ConnectOpts ++ Opts;
	_ ->
	    Opts
    end.

%%
%% Close a socket.
%%
-spec close(Socket) -> ok when
      Socket :: gen_tcp:socket().
close(Socket) ->
    inet_tcp:close(Socket).


%% If Node is illegal terminate the connection setup!!
splitnode(Driver, Node, LongOrShortNames) ->
    case split_node(atom_to_list(Node), $@, []) of
	[Name|Tail] when Tail =/= [] ->
	    Host = lists:append(Tail),
	    case split_node(Host, $., []) of
		[_] when LongOrShortNames =:= longnames ->
                    case Driver:parse_address(Host) of
                        {ok, _} ->
                            [Name, Host];
                        _ ->
                            error_msg("** System running to use "
                                      "fully qualified "
                                      "hostnames **~n"
                                      "** Hostname ~ts is illegal **~n",
                                      [Host]),
                            ?shutdown(Node)
                    end;
		L when length(L) > 1, LongOrShortNames =:= shortnames ->
		    error_msg("** System NOT running to use fully qualified "
			      "hostnames **~n"
			      "** Hostname ~ts is illegal **~n",
			      [Host]),
		    ?shutdown(Node);
		_ ->
		    [Name, Host]
	    end;
	[_] ->
	    error_msg("** Nodename ~p illegal, no '@' character **~n",
		      [Node]),
	    ?shutdown(Node);
	_ ->
	    error_msg("** Nodename ~p illegal **~n", [Node]),
	    ?shutdown(Node)
    end.

split_node([Chr|T], Chr, Ack) -> [lists:reverse(Ack)|split_node(T, Chr, [])];
split_node([H|T], Chr, Ack)   -> split_node(T, Chr, [H|Ack]);
split_node([], _, Ack)        -> [lists:reverse(Ack)].

%% ------------------------------------------------------------
%% Fetch local information about a Socket.
%% ------------------------------------------------------------
get_tcp_address(Driver, Socket) ->
    {ok, Address} = inet:sockname(Socket),
    NetAddr = get_tcp_address(Driver),
    NetAddr#net_address{ address = Address }.
get_tcp_address(Driver) ->
    {ok, Host} = inet:gethostname(),
    #net_address {
		  host = Host,
		  protocol = tcp,
		  family = Driver:family()
		 }.

%% ------------------------------------------------------------
%% Determine if EPMD module supports the called functions.
%% If not call the builtin erl_epmd
%% ------------------------------------------------------------
call_epmd_function(Mod, Fun, Args) ->
    case erlang:function_exported(Mod, Fun, length(Args)) of
        true -> apply(Mod,Fun,Args);
        _    -> apply(erl_epmd, Fun, Args)
    end.

%% ------------------------------------------------------------
%% Do only accept new connection attempts from nodes at our
%% own LAN, if the check_ip environment parameter is true.
%% ------------------------------------------------------------
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

get_ifs(Socket) ->
    case inet:peername(Socket) of
	{ok, {IP, _}} ->
	    case inet:getif(Socket) of
		{ok, IFs} -> {ok, IFs, IP};
		Error     -> Error
	    end;
	Error ->
	    Error
    end.

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
	    split_stat(Stat,0,0,0);
	Error ->
	    Error
    end.

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
	% TODO: change to erlang:dist_handle() once this is fixed upstream in OTP
    dist_handle = undefined :: eqwalizer:dynamic(),
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

sender_enter_loop(State = #sender{receiver = Receiver}) ->
    receive
        {Receiver, receiver_ack} ->
            sender_before_loop(State)
    end.

sender_before_loop(State0 = #sender{dist_handle = DistHandle, dist_notify = false}) ->
    ok = erlang:dist_ctrl_get_data_notification(DistHandle),
    State1 = State0#sender{dist_notify = true},
    sender_before_loop(State1);
sender_before_loop(State) ->
    sender_loop(State).

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

get_connection_id({ConnectionId, _}) ->
	ConnectionId;
get_connection_id(_) ->
	0.

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

receiver_before_loop(State0 = #receiver{socket = Socket, socket_notify = false}) ->
    ok = inet:setopts(Socket, [{active, once}]),
    State1 = State0#receiver{socket_notify = true},
    receiver_before_loop(State1);
receiver_before_loop(State) ->
    receiver_loop(State).

receiver_loop(State0 = #receiver{socket = Socket, channel = Channel}) ->
    receive
        {tcp, Socket, Data} ->
            State1 = State0#receiver{socket_notify = false},
            Actions = erldist_filter_nif:channel_recv(Channel, [Data]),
            receiver_loop_actions(Actions, State1);
        BadMsg ->
            exit({badmsg, BadMsg})
    end.

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

maybe_decode(_Atoms, undefined) ->
    undefined;
maybe_decode(Atoms, Message) ->
    erts_debug:dist_ext_to_term(Atoms, Message).
