%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(erldist_filter_test_peer).
-moduledoc """
UUIDv7: time-ordered + randomness with low collision probability
See https://www.rfc-editor.org/rfc/rfc9562.html#section-5.7
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-08-26", modified => "2025-08-26"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% OTP callbacks
-export([
    child_spec/0,
    start_link/1
]).
%% Public API
-export([
    decode/1,
    encode/1,
    new/1,
    new/2,
    os_pid/0,
    rand_a/0,
    rand_b/0,
    unix_ts_ms/0,
    wait_until_net_kernel_started/1
]).

%% Types
-type host() :: inet:ip_address().
-type name() :: unicode:unicode_binary().
-type os_pid() :: 0..16#FFFFFFFF.
-type rand_a() :: <<_:12>>.
-type rand_b() :: <<_:62>>.
-type t() :: #{
    host := host(),
    name := name(),
    os_pid := os_pid(),
    rand_a := rand_a(),
    rand_b := rand_b(),
    unix_ts_ms := unix_ts_ms()
}.
-type unix_ts_ms() :: 0..16#FFFFFFFFFFFF.

-export_type([
    host/0,
    name/0,
    os_pid/0,
    rand_a/0,
    rand_b/0,
    t/0,
    unix_ts_ms/0
]).

%%%=============================================================================
%%% OTP callbacks
%%%=============================================================================

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    #{
        id => undefined,
        start => {?MODULE, start_link, []},
        restart => temporary,
        shutdown => brutal_kill,
        type => worker,
        modules => [?MODULE, peer]
    }.

-spec start_link(PeerNode) -> StartLinkReturn when
    PeerNode :: node(),
    StartLinkReturn :: {ok, PeerPid} | {error, Reason},
    PeerPid :: pid(),
    Reason :: dynamic().
start_link(PeerNode) when is_atom(PeerNode) ->
    [PeerName, PeerHost] = string:lexemes(erlang:atom_to_list(PeerNode), "@"),
    Options = #{
        args => flatten_peer_args([
            {"-connect_all", "false"},
            {"-kernel", "dist_auto_connect", "never"},
            {"-kernel", "start_distribution", "false"},
            {"-proto_dist", "erldist_filter_inet_tcp"},
            {"-setcookie", "connect_cookie"}
        ]),
        connection => standard_io,
        detached => true,
        host => PeerHost,
        name => PeerName
    },
    case peer:start_link(Options) of
        {ok, PeerPid, _ActualPeerNode} when is_pid(PeerPid) ->
            maybe
                true ?= peer:call(PeerPid, code, set_path, [code:get_path()]),
                {ok, _} ?= peer:call(PeerPid, net_kernel, start, [PeerNode, #{name_domain => longnames}]),
                ok ?= peer:call(PeerPid, ?MODULE, wait_until_net_kernel_started, [PeerNode]),
                {ok, PeerPid}
            else
                _NetKernelFailure ->
                    io:format(standard_error, "net_kernel start failure: ~0tp\n", [_NetKernelFailure]),
                    ok = peer:stop(PeerPid),
                    {error, net_kernel_start_failed}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%%=============================================================================
%%% Public API functions
%%%=============================================================================

-spec decode(Node) -> Peer when Node :: node(), Peer :: t().
decode(Node) when is_atom(Node) ->
    NodeBytes = erlang:atom_to_binary(Node, unicode),
    maybe
        [NameDataBytes, HostBytes] ?= binary:split(NodeBytes, <<"@">>, [global, trim_all]),
        HostString = erlang:binary_to_list(HostBytes),
        {ok, Host} ?= inet:parse_address(HostString),
        NameDataSize = byte_size(NameDataBytes),
        NameSize = NameDataSize - 32,
        true ?= NameSize > 0,
        <<Name:NameSize/bytes, Data:32/bytes>> ?= NameDataBytes,
        MetaULID = <<<<(crockford_base32_decode(C)):5>> || <<C:8>> <= Data>>,
        <<OsPid:1/unsigned-big-integer-unit:32, UnixTsMs:1/unsigned-big-integer-unit:48, 7:4, RandA:12/bits, 2:2,
            RandB:62/bits>> ?= MetaULID,
        #{
            host => Host,
            name => Name,
            os_pid => OsPid,
            rand_a => RandA,
            rand_b => RandB,
            unix_ts_ms => UnixTsMs
        }
    else
        _ ->
            erlang:error(badarg, [Node])
    end.

-spec encode(Peer) -> Node when Peer :: t(), Node :: node().
encode(
    Peer = #{
        host := Host,
        name := Name,
        os_pid := OsPid,
        rand_a := RandA,
        rand_b := RandB,
        unix_ts_ms := UnixTsMs
    }
) ->
    maybe
        HostString = [_ | _] ?= inet:ntoa(Host),
        HostBytes = erlang:list_to_binary(HostString),
        MetaULID =
            <<OsPid:1/unsigned-big-integer-unit:32, UnixTsMs:1/unsigned-big-integer-unit:48, 7:4, RandA:12/bits, 2:2,
                RandB:62/bits>>,
        Data = <<<<(crockford_base32_encode(C)):8>> || <<C:5>> <= MetaULID>>,
        NodeBytes = <<Name/bytes, Data/bytes, $@, HostBytes/bytes>>,
        erlang:binary_to_atom(NodeBytes, unicode)
    else
        _ ->
            erlang:error(badarg, [Peer])
    end.

-spec new(Name) -> Peer when Name :: name(), Peer :: t().
new(Name) when is_binary(Name) ->
    new(Name, {127, 0, 0, 1}).

-spec new(Name, Host) -> Peer when Name :: name(), Host :: host(), Peer :: t().
new(Name, Host) when
    is_binary(Name) andalso (is_tuple(Host) andalso (tuple_size(Host) =:= 4 orelse tuple_size(Host) =:= 8))
->
    #{
        host => Host,
        name => Name,
        os_pid => os_pid(),
        rand_a => rand_a(),
        rand_b => rand_b(),
        unix_ts_ms => unix_ts_ms()
    }.

-spec os_pid() -> OsPid when OsPid :: os_pid().
os_pid() ->
    case erlang:list_to_integer(os:getpid()) of
        OsPid when is_integer(OsPid) andalso OsPid >= 0 andalso OsPid =< 16#FFFFFFFF ->
            OsPid
    end.

-spec rand_a() -> RandA when RandA :: rand_a().
rand_a() ->
    <<RandA:12/bits, _:4/bits>> = crypto:strong_rand_bytes(2),
    RandA.

-spec rand_b() -> RandB when RandB :: rand_b().
rand_b() ->
    <<_:2/bits, RandB:62/bits>> = crypto:strong_rand_bytes(8),
    RandB.

-spec unix_ts_ms() -> UnixTsMs when UnixTsMs :: unix_ts_ms().
unix_ts_ms() ->
    case erlang:system_time(millisecond) of
        UnixTsMs when is_integer(UnixTsMs) andalso UnixTsMs >= 0 andalso UnixTsMs =< 16#FFFFFFFFFFFF ->
            UnixTsMs
    end.

-spec wait_until_net_kernel_started(Node) -> ok when Node :: node().
wait_until_net_kernel_started(Node) when is_atom(Node) ->
    case net_kernel:get_state() of
        #{started := no} ->
            _ = net_kernel:start(Node, #{name_domain => longnames}),
            ok =
                receive
                after 100 -> ok
                end,
            wait_until_net_kernel_started(Node);
        #{started := Started, name := Node} when Started =/= no ->
            ok
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-compile({inline, [crockford_base32_decode/1]}).
-spec crockford_base32_decode(In) -> Out when In :: byte(), Out :: 0..31.
crockford_base32_decode($0) -> 0;
crockford_base32_decode($1) -> 1;
crockford_base32_decode($2) -> 2;
crockford_base32_decode($3) -> 3;
crockford_base32_decode($4) -> 4;
crockford_base32_decode($5) -> 5;
crockford_base32_decode($6) -> 6;
crockford_base32_decode($7) -> 7;
crockford_base32_decode($8) -> 8;
crockford_base32_decode($9) -> 9;
crockford_base32_decode($A) -> 10;
crockford_base32_decode($B) -> 11;
crockford_base32_decode($C) -> 12;
crockford_base32_decode($D) -> 13;
crockford_base32_decode($E) -> 14;
crockford_base32_decode($F) -> 15;
crockford_base32_decode($G) -> 16;
crockford_base32_decode($H) -> 17;
crockford_base32_decode($J) -> 18;
crockford_base32_decode($K) -> 19;
crockford_base32_decode($M) -> 20;
crockford_base32_decode($N) -> 21;
crockford_base32_decode($P) -> 22;
crockford_base32_decode($Q) -> 23;
crockford_base32_decode($R) -> 24;
crockford_base32_decode($S) -> 25;
crockford_base32_decode($T) -> 26;
crockford_base32_decode($V) -> 27;
crockford_base32_decode($W) -> 28;
crockford_base32_decode($X) -> 29;
crockford_base32_decode($Y) -> 30;
crockford_base32_decode($Z) -> 31.

-compile({inline, [crockford_base32_encode/1]}).
-spec crockford_base32_encode(In) -> Out when In :: 0..31, Out :: byte().
crockford_base32_encode(0) -> $0;
crockford_base32_encode(1) -> $1;
crockford_base32_encode(2) -> $2;
crockford_base32_encode(3) -> $3;
crockford_base32_encode(4) -> $4;
crockford_base32_encode(5) -> $5;
crockford_base32_encode(6) -> $6;
crockford_base32_encode(7) -> $7;
crockford_base32_encode(8) -> $8;
crockford_base32_encode(9) -> $9;
crockford_base32_encode(10) -> $A;
crockford_base32_encode(11) -> $B;
crockford_base32_encode(12) -> $C;
crockford_base32_encode(13) -> $D;
crockford_base32_encode(14) -> $E;
crockford_base32_encode(15) -> $F;
crockford_base32_encode(16) -> $G;
crockford_base32_encode(17) -> $H;
crockford_base32_encode(18) -> $J;
crockford_base32_encode(19) -> $K;
crockford_base32_encode(20) -> $M;
crockford_base32_encode(21) -> $N;
crockford_base32_encode(22) -> $P;
crockford_base32_encode(23) -> $Q;
crockford_base32_encode(24) -> $R;
crockford_base32_encode(25) -> $S;
crockford_base32_encode(26) -> $T;
crockford_base32_encode(27) -> $V;
crockford_base32_encode(28) -> $W;
crockford_base32_encode(29) -> $X;
crockford_base32_encode(30) -> $Y;
crockford_base32_encode(31) -> $Z.

-spec flatten_peer_args(Args) -> Args when Args :: [dynamic()].
flatten_peer_args([T | Rest]) when is_tuple(T) ->
    tuple_to_list(T) ++ flatten_peer_args(Rest);
% flatten_peer_args([L | Rest]) when is_list(L) ->
%     [L | flatten_peer_args(Rest)];
flatten_peer_args([]) ->
    [].
