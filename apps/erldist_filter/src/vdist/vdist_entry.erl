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
%%% Created :  27 Mar 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(vdist_entry).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include("erldist_filter.hrl").
-include("erldist_filter_erts_dist.hrl").
-include("erldist_filter_erts_external.hrl").

%% API
-export([
    new/1,
    decode/2,
    encode/3,
    encode/4,
    fill_tx_atom_cache/2,
    is_tx_atom_cache_filled/1,
    reg_send_noop/0,
    reg_send_noop/3,
    reg_send_noop_alt/0
]).

%% Types
-type t() :: #vdist_entry{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(DistributionFlags) -> T when DistributionFlags :: vterm:u64(), T :: t().
new(DistributionFlags) when is_integer(DistributionFlags) ->
    case
        (DistributionFlags band ?DFLAG_DIST_HDR_ATOM_CACHE) =/= 0 orelse (DistributionFlags band ?DFLAG_FRAGMENTS) =/= 0
    of
        true ->
            RxAtomCache = vdist_atom_cache:new(),
            TxAtomCache = vdist_atom_cache:new(),
            #vdist_entry{
                dflags = DistributionFlags,
                rx_atom_cache = RxAtomCache,
                tx_atom_cache = TxAtomCache
            };
        false ->
            #vdist_entry{
                dflags = DistributionFlags,
                rx_atom_cache = undefined,
                tx_atom_cache = undefined
            }
    end.

-spec decode(OldEntry, Input) -> {ok, ControlMessage, MaybePayload, NewEntry, Rest} | {cont, NewEntry} when
    OldEntry :: t(),
    Input :: binary(),
    ControlMessage :: vdist:dop_t(),
    MaybePayload :: vterm:t() | undefined,
    NewEntry :: t(),
    Rest :: binary().
decode(Entry = #vdist_entry{}, Input) when is_binary(Input) ->
    vdist_entry_decode:decode(Entry, Input).

-spec encode(Entry, ControlMessage, Options) -> {ok, Fragments, Entry} when
    Entry :: t(),
    ControlMessage :: vdist:dop_without_payload_t(),
    Options :: vdist_entry_encode:options(),
    Fragments :: [binary()].
encode(Entry = #vdist_entry{}, ControlMessage, Options) when
    ?is_vdist_dop_without_payload_t(ControlMessage) andalso is_map(Options)
->
    vdist_entry_encode:encode(Entry, ControlMessage, Options).

-spec encode(Entry, ControlMessage, Payload, Options) -> {ok, Fragments, Entry} when
    Entry :: t(),
    ControlMessage :: vdist:dop_with_payload_t(),
    Payload :: vterm:t(),
    Options :: vdist_entry_encode:options(),
    Fragments :: [binary()].
encode(Entry = #vdist_entry{}, ControlMessage, Payload, Options) when
    ?is_vdist_dop_with_payload_t(ControlMessage) andalso
        ?is_vterm_t(Payload) andalso is_map(Options)
->
    vdist_entry_encode:encode(Entry, ControlMessage, Payload, Options).

-spec fill_tx_atom_cache(Entry, Options) -> {ok, Fragments, Entry} when
    Entry :: t(),
    Options :: vdist_entry_encode:options(),
    Fragments :: [binary()].
fill_tx_atom_cache(Entry0 = #vdist_entry{tx_atom_cache = #vdist_atom_cache{}}, Options) ->
    {NoopControlMessage, NoopPayload} = reg_send_noop(),
    {ok, Fragments0, Entry1 = #vdist_entry{tx_atom_cache = CacheA = #vdist_atom_cache{}}} = encode(
        Entry0, NoopControlMessage, NoopPayload, Options
    ),
    CacheB = vdist_atom_cache:fill(CacheA),
    #{add := KeysAdded, del := []} = vdist_atom_cache:diff(CacheA, CacheB),
    case KeysAdded of
        [] ->
            {ok, Fragments0, Entry1};
        [_ | _] ->
            % io:format(user, "filling with ~p~n", [KeysAdded]),
            do_fill_tx_atom_cache(Entry1, NoopControlMessage, KeysAdded, Fragments0, Options)
    end.

-spec is_tx_atom_cache_filled(Entry) -> boolean() when Entry :: t().
is_tx_atom_cache_filled(#vdist_entry{tx_atom_cache = TxAtomCache = #vdist_atom_cache{}}) ->
    vdist_atom_cache:is_filled(TxAtomCache).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% @private
-spec do_fill_tx_atom_cache(Entry, NoopControlMessage, KeysAdded, Fragments, Options) ->
    {ok, Fragments, Entry}
when
    Entry :: t(),
    NoopControlMessage :: vdist:dop_with_payload_t(),
    KeysAdded :: [atom()],
    Fragments :: [binary()],
    Options :: vdist_entry_encode:options().
do_fill_tx_atom_cache(
    Entry0 = #vdist_entry{tx_atom_cache = #vdist_atom_cache{}}, NoopControlMessage, KeysAdded0, Fragments0, Options
) when length(KeysAdded0) > ?ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES ->
    {PayloadTerm, KeysAdded1} = lists:split(?ERTS_MAX_INTERNAL_ATOM_CACHE_ENTRIES, KeysAdded0),
    PayloadVTerm = vterm:expand(PayloadTerm),
    {ok, NextFragments, Entry1} = encode(Entry0, NoopControlMessage, PayloadVTerm, Options),
    Fragments1 = lists:append(Fragments0, NextFragments),
    do_fill_tx_atom_cache(Entry1, NoopControlMessage, KeysAdded1, Fragments1, Options);
do_fill_tx_atom_cache(
    Entry0 = #vdist_entry{tx_atom_cache = #vdist_atom_cache{}}, NoopControlMessage, PayloadTerm, Fragments0, Options
) ->
    PayloadVTerm = vterm:expand(PayloadTerm),
    {ok, NextFragments, Entry1} = encode(Entry0, NoopControlMessage, PayloadVTerm, Options),
    Fragments1 = lists:append(Fragments0, NextFragments),
    {ok, Fragments1, Entry1}.

%% @private
-spec reg_send_noop() -> {Message, Payload} when Message :: vdist:dop_with_payload_t(), Payload :: vterm:t().
reg_send_noop() ->
    reg_send_noop(0, 0, 0).
% EmptyAtom = vterm_atom_utf8_ext:new(0, <<>>),
% UndefinedAtom = vterm_atom_utf8_ext:new(9, <<"undefined">>),
% FromPid = vterm_new_pid_ext:new(EmptyAtom, 0, 0, 0),
% RegSend = vdist_dop_reg_send:new(FromPid, EmptyAtom, UndefinedAtom),
% Payload = vterm_nil_ext:new(),
% {RegSend, Payload}.

%% @private
-spec reg_send_noop(Id, Serial, Creation) -> {Message, Payload} when
    Id :: non_neg_integer(),
    Serial :: non_neg_integer(),
    Creation :: non_neg_integer(),
    Message :: vdist:dop_with_payload_t(),
    Payload :: vterm:t().
reg_send_noop(Id, Serial, Creation) when ?is_u32(Id) andalso ?is_u32(Serial) andalso ?is_u32(Creation) ->
    EmptyAtom = vterm_atom_utf8_ext:new(0, <<>>),
    UndefinedAtom = vterm_atom_utf8_ext:new(9, <<"undefined">>),
    FromPid = vterm_new_pid_ext:new(EmptyAtom, Id, Serial, Creation),
    RegSend = vdist_dop_reg_send:new(FromPid, EmptyAtom, UndefinedAtom),
    Payload = vterm_nil_ext:new(),
    {RegSend, Payload}.

%% @private
-spec reg_send_noop_alt() -> {Message, Payload} when Message :: vdist:dop_with_payload_t(), Payload :: vterm:t().
reg_send_noop_alt() ->
    reg_send_noop(1, 1, 1).
% EmptyAtom = vterm_atom_utf8_ext:new(0, <<>>),
    % UndefinedAtom = vterm_atom_utf8_ext:new(9, <<"undefined">>),
    % FromPid = vterm_new_pid_ext:new(EmptyAtom, 1, 1, 1),
    % RegSend = vdist_dop_reg_send:new(FromPid, EmptyAtom, UndefinedAtom),
    % Payload = vterm_nil_ext:new(),
    % {RegSend, Payload}.
