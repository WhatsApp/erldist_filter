%%% % @format
%%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% Created :  27 Mar 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
-module(vterm_decode).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("erldist_filter/include/erldist_filter_erts_external.hrl").

%% API
-export([
    external_binary_to_term/1,
    external_binary_to_vterm/1,
    external_binary_to_vterm_lazy/2,
    internal_binary_to_term/1,
    internal_binary_to_vterm/1,
    internal_binary_to_vterm_atom/1,
    internal_binary_to_vterm_elements/3,
    internal_binary_to_vterm_fixed_integer/1,
    internal_binary_to_vterm_lazy/2,
    internal_binary_to_vterm_pairs/3,
    internal_binary_to_vterm_pid/1,
    internal_binary_to_vterm_small_integer/1,
    vterm_lazy_limit_dec/0,
    vterm_lazy_limit_erase/0,
    vterm_lazy_limit_get/0,
    vterm_lazy_limit_set/1
]).

%% Macros
-define(LAZY_LIMIT_KEY, '$vterm_lazy_limit').
-define(LAZY_DEC(), vterm_lazy_limit_dec()).
-define(LAZY_PAUSE(), begin
    Lazy@Paused = vterm_lazy_limit_erase()
end).
-define(LAZY_CONTINUE(), begin
    ok = vterm_lazy_limit_set(Lazy@Paused)
end).
-define(LAZY(LazyLimit, NewVT),
    case (LazyLimit) of
        0 ->
            SliceSize = byte_size(InternalEncodedTerm) - byte_size(Rest),
            Slice = binary:part(InternalEncodedTerm, 0, SliceSize),
            {ok, vterm_lazy_term:new(Slice), Rest};
        _ ->
            {ok, (NewVT), Rest}
    end
).
% -define(LAZY(NewVT), ?LAZY(?LAZY_DEC(), (NewVT))).
-define(NOT_LAZY(LazyLimit, NewVT), begin
    _ = (LazyLimit),
    {ok, (NewVT), Rest}
end).
-define(NOT_LAZY(NewVT), ?NOT_LAZY(vterm_lazy_limit_dec(), (NewVT))).
-define(MAYBE_LAZY(N, LazyLimit, NewVT),
    case (N) of
        0 ->
            ?NOT_LAZY((LazyLimit), (NewVT));
        _ ->
            ?LAZY((LazyLimit), (NewVT))
    end
).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec external_binary_to_term(Binary) -> {ok, Term, Rest} | {error, Reason} when
    Binary :: binary(), Term :: term(), Rest :: bitstring(), Reason :: term().
external_binary_to_term(Binary) when is_binary(Binary) ->
    case erlang:binary_to_term(Binary, [used]) of
        {Term, Used} when is_integer(Used) ->
            <<_:Used/bytes, Rest/bits>> = Binary,
            {ok, Term, Rest}
    end.

-spec external_binary_to_vterm(Binary) -> {ok, VTerm, Rest} | {error, Reason} when
    Binary :: binary(), VTerm :: vterm:t(), Rest :: bitstring(), Reason :: term().
external_binary_to_vterm(<<?VERSION_MAGIC:8, InternalEncodedTerm/bytes>>) ->
    internal_binary_to_vterm(InternalEncodedTerm).

-spec external_binary_to_vterm_lazy(Binary, Limit) -> {ok, VTerm, Rest} | {error, Reason} when
    Binary :: binary(), Limit :: integer(), VTerm :: vterm:t(), Rest :: bitstring(), Reason :: term().
external_binary_to_vterm_lazy(<<?VERSION_MAGIC:8, InternalEncodedTerm/bytes>>, Limit) when is_integer(Limit) ->
    internal_binary_to_vterm_lazy(InternalEncodedTerm, Limit).

-spec internal_binary_to_term(Binary) -> {ok, Term, Rest} | {error, Reason} when
    Binary :: binary(), Term :: term(), Rest :: bitstring(), Reason :: term().
internal_binary_to_term(Binary) when is_binary(Binary) ->
    external_binary_to_term(<<?VERSION_MAGIC:8, Binary/bytes>>).

-spec internal_binary_to_vterm(Binary) -> {ok, VTerm, Rest} | {error, Reason} when
    Binary :: binary(), VTerm :: vterm:t(), Rest :: bitstring(), Reason :: term().
internal_binary_to_vterm(InternalEncodedTerm) ->
    case InternalEncodedTerm of
        <<?SMALL_INTEGER_EXT:8, Value:8, Rest/bytes>> ->
            ?NOT_LAZY(vterm_small_integer_ext:new(Value));
        <<?INTEGER_EXT:8, Value:1/signed-big-integer-unit:32, Rest/bytes>> ->
            ?NOT_LAZY(vterm_integer_ext:new(Value));
        <<?FLOAT_EXT:8, FloatString:31/bytes, Rest/bytes>> ->
            ?NOT_LAZY(vterm_float_ext:new(FloatString));
        <<?ATOM_EXT:8, Len:16, Name:Len/bytes, Rest/bytes>> ->
            ?NOT_LAZY(vterm_atom_ext:new(Len, Name));
        <<?SMALL_ATOM_EXT:8, Len:8, Name:Len/bytes, Rest/bytes>> ->
            ?NOT_LAZY(vterm_small_atom_ext:new(Len, Name));
        <<?REFERENCE_EXT:8, InternalEncodedReferenceExt/bytes>> ->
            LazyLimit = ?LAZY_DEC(),
            ?LAZY_PAUSE(),
            {ok, Node, <<Id:32, Creation:8, Rest/bytes>>} = internal_binary_to_vterm_atom(InternalEncodedReferenceExt),
            ?LAZY_CONTINUE(),
            ?NOT_LAZY(LazyLimit, vterm_reference_ext:new(Node, Id, Creation));
        <<?NEW_REFERENCE_EXT:8, Len:16, InternalEncodedNewReferenceExt/bytes>> ->
            LazyLimit = ?LAZY_DEC(),
            ?LAZY_PAUSE(),
            EncodedIdsBits = Len * 32,
            {ok, Node, <<Creation:8, EncodedIds:EncodedIdsBits/bits, Rest/bytes>>} = internal_binary_to_vterm_atom(
                InternalEncodedNewReferenceExt
            ),
            Ids = [Id || <<Id:32>> <= EncodedIds],
            ?LAZY_CONTINUE(),
            ?NOT_LAZY(LazyLimit, vterm_new_reference_ext:new(Len, Node, Creation, Ids));
        <<?NEWER_REFERENCE_EXT:8, Len:16, InternalEncodedNewerReferenceExt/bytes>> ->
            LazyLimit = ?LAZY_DEC(),
            ?LAZY_PAUSE(),
            EncodedIdsBits = Len * 32,
            {ok, Node, <<Creation:32, EncodedIds:EncodedIdsBits/bits, Rest/bytes>>} = internal_binary_to_vterm_atom(
                InternalEncodedNewerReferenceExt
            ),
            Ids = [Id || <<Id:32>> <= EncodedIds],
            ?LAZY_CONTINUE(),
            ?NOT_LAZY(LazyLimit, vterm_newer_reference_ext:new(Len, Node, Creation, Ids));
        <<?PORT_EXT:8, InternalEncodedPortExt/bytes>> ->
            LazyLimit = ?LAZY_DEC(),
            ?LAZY_PAUSE(),
            {ok, Node, <<Id:32, Creation:8, Rest/bytes>>} = internal_binary_to_vterm_atom(InternalEncodedPortExt),
            ?LAZY_CONTINUE(),
            ?NOT_LAZY(LazyLimit, vterm_port_ext:new(Node, Id, Creation));
        <<?NEW_PORT_EXT:8, InternalEncodedNewPortExt/bytes>> ->
            LazyLimit = ?LAZY_DEC(),
            ?LAZY_PAUSE(),
            {ok, Node, <<Id:32, Creation:32, Rest/bytes>>} = internal_binary_to_vterm_atom(InternalEncodedNewPortExt),
            ?LAZY_CONTINUE(),
            ?NOT_LAZY(LazyLimit, vterm_new_port_ext:new(Node, Id, Creation));
        <<?NEW_FLOAT_EXT:8, IEEEFloat:8/bytes, Rest/bytes>> ->
            ?NOT_LAZY(vterm_new_float_ext:new(IEEEFloat));
        <<?PID_EXT:8, InternalEncodedPidExt/bytes>> ->
            LazyLimit = ?LAZY_DEC(),
            ?LAZY_PAUSE(),
            {ok, Node, <<Id:32, Serial:32, Creation:8, Rest/bytes>>} = internal_binary_to_vterm_atom(
                InternalEncodedPidExt
            ),
            ?LAZY_CONTINUE(),
            ?NOT_LAZY(LazyLimit, vterm_pid_ext:new(Node, Id, Serial, Creation));
        <<?NEW_PID_EXT:8, InternalEncodedNewPidExt/bytes>> ->
            LazyLimit = ?LAZY_DEC(),
            ?LAZY_PAUSE(),
            {ok, Node, <<Id:32, Serial:32, Creation:32, Rest/bytes>>} = internal_binary_to_vterm_atom(
                InternalEncodedNewPidExt
            ),
            ?LAZY_CONTINUE(),
            ?NOT_LAZY(LazyLimit, vterm_new_pid_ext:new(Node, Id, Serial, Creation));
        <<?SMALL_TUPLE_EXT:8, Arity:8, InternalEncodedElements/bytes>> ->
            LazyLimit = ?LAZY_DEC(),
            {ok, Elements, Rest} = internal_binary_to_vterm_elements(Arity, InternalEncodedElements, []),
            ?MAYBE_LAZY(Arity, LazyLimit, vterm_small_tuple_ext:new(Arity, Elements));
        <<?LARGE_TUPLE_EXT:8, Arity:32, InternalEncodedElements/bytes>> ->
            LazyLimit = ?LAZY_DEC(),
            {ok, Elements, Rest} = internal_binary_to_vterm_elements(Arity, InternalEncodedElements, []),
            ?MAYBE_LAZY(Arity, LazyLimit, vterm_large_tuple_ext:new(Arity, Elements));
        <<?NIL_EXT:8, Rest/bytes>> ->
            ?NOT_LAZY(vterm_nil_ext:new());
        <<?STRING_EXT:8, Len:16, Characters:Len/bytes, Rest/bytes>> ->
            ?NOT_LAZY(vterm_string_ext:new(Len, Characters));
        <<?LIST_EXT:8, Len:32, InternalEncodedElements/bytes>> ->
            LazyLimit = ?LAZY_DEC(),
            {ok, Elements, InternalEncodedTail} = internal_binary_to_vterm_elements(Len, InternalEncodedElements, []),
            {ok, Tail, Rest} = internal_binary_to_vterm(InternalEncodedTail),
            ?MAYBE_LAZY(Len, LazyLimit, vterm_list_ext:new(Len, Elements, Tail));
        <<?BINARY_EXT:8, Len:32, Data:Len/bytes, Rest/bytes>> ->
            ?NOT_LAZY(vterm_binary_ext:new(Len, Data));
        <<?BIT_BINARY_EXT:8, Len:32, Bits:8, Data:Len/bytes, Rest/bytes>> ->
            ?NOT_LAZY(vterm_bit_binary_ext:new(Len, Bits, Data));
        <<?SMALL_BIG_EXT:8, N:8, Sign:8, D:N/bytes, Rest/bytes>> ->
            ?NOT_LAZY(vterm_small_big_ext:new(N, Sign, D));
        <<?LARGE_BIG_EXT:8, N:32, Sign:8, D:N/bytes, Rest/bytes>> ->
            ?NOT_LAZY(vterm_large_big_ext:new(N, Sign, D));
        <<?EXPORT_EXT:8, InternalEncodedModule/bytes>> ->
            LazyLimit = ?LAZY_DEC(),
            ?LAZY_PAUSE(),
            {ok, Module, InternalEncodedFunction} = internal_binary_to_vterm_atom(InternalEncodedModule),
            {ok, Function, InternalEncodedArity} = internal_binary_to_vterm_atom(InternalEncodedFunction),
            {ok, Arity, Rest} = internal_binary_to_vterm_small_integer(InternalEncodedArity),
            ?LAZY_CONTINUE(),
            ?NOT_LAZY(LazyLimit, vterm_export_ext:new(Module, Function, Arity));
        <<?NEW_FUN_EXT:8, Size:32, Arity:8, Uniq:16/bytes, Index:32, NumFree:32, InternalEncodedModule/bytes>> ->
            LazyLimit = ?LAZY_DEC(),
            ?LAZY_PAUSE(),
            {ok, Module, InternalEncodedOldIndex} = internal_binary_to_vterm_atom(InternalEncodedModule),
            {ok, OldIndex, InternalEncodedOldUniq} = internal_binary_to_vterm_fixed_integer(InternalEncodedOldIndex),
            {ok, OldUniq, InternalEncodedPid} = internal_binary_to_vterm_fixed_integer(InternalEncodedOldUniq),
            {ok, Pid, InternalEncodedFreeVars} = internal_binary_to_vterm_pid(InternalEncodedPid),
            ?LAZY_CONTINUE(),
            {ok, FreeVars, Rest} = internal_binary_to_vterm_elements(NumFree, InternalEncodedFreeVars, []),
            ?LAZY(
                LazyLimit,
                vterm_new_fun_ext:new(Size, Arity, Uniq, Index, NumFree, Module, OldIndex, OldUniq, Pid, FreeVars)
            );
        <<?MAP_EXT:8, Arity:32, InternalEncodedPairs/bytes>> ->
            LazyLimit = ?LAZY_DEC(),
            {ok, Pairs, Rest} = internal_binary_to_vterm_pairs(Arity, InternalEncodedPairs, []),
            ?MAYBE_LAZY(Arity, LazyLimit, vterm_map_ext:new(Arity, Pairs));
        <<?ATOM_UTF8_EXT:8, Len:16, Name:Len/bytes, Rest/bytes>> ->
            ?NOT_LAZY(vterm_atom_utf8_ext:new(Len, Name));
        <<?SMALL_ATOM_UTF8_EXT:8, Len:8, Name:Len/bytes, Rest/bytes>> ->
            ?NOT_LAZY(vterm_small_atom_utf8_ext:new(Len, Name));
        <<?V4_PORT_EXT:8, InternalEncodedV4PortExt/bytes>> ->
            LazyLimit = ?LAZY_DEC(),
            ?LAZY_PAUSE(),
            {ok, Node, <<Id:64, Creation:32, Rest/bytes>>} = internal_binary_to_vterm_atom(InternalEncodedV4PortExt),
            ?LAZY_CONTINUE(),
            ?NOT_LAZY(LazyLimit, vterm_v4_port_ext:new(Node, Id, Creation));
        <<?ATOM_CACHE_REF:8, Index:8, Rest/bytes>> ->
            ?NOT_LAZY(vterm_atom_cache_ref:new(Index))
    end.

-spec internal_binary_to_vterm_atom(Binary) -> {ok, VTermAtom, Rest} | {error, Reason} when
    Binary :: binary(), VTermAtom :: vterm:atom_t(), Rest :: bitstring(), Reason :: term().
internal_binary_to_vterm_atom(Binary) when is_binary(Binary) ->
    case Binary of
        <<?ATOM_EXT:8, Len:16, Name:Len/bytes, Rest/bytes>> ->
            {ok, vterm_atom_ext:new(Len, Name), Rest};
        <<?SMALL_ATOM_EXT:8, Len:8, Name:Len/bytes, Rest/bytes>> ->
            {ok, vterm_small_atom_ext:new(Len, Name), Rest};
        <<?ATOM_UTF8_EXT:8, Len:16, Name:Len/bytes, Rest/bytes>> ->
            {ok, vterm_atom_utf8_ext:new(Len, Name), Rest};
        <<?SMALL_ATOM_UTF8_EXT:8, Len:8, Name:Len/bytes, Rest/bytes>> ->
            {ok, vterm_small_atom_utf8_ext:new(Len, Name), Rest};
        <<?ATOM_CACHE_REF:8, Index:8, Rest/bytes>> ->
            {ok, vterm_atom_cache_ref:new(Index), Rest}
    end.

-spec internal_binary_to_vterm_elements(Arity, Binary, PrevVTermElements) ->
    {ok, NextVTermElements, Rest} | {error, Reason}
when
    Arity :: non_neg_integer(),
    Binary :: binary(),
    PrevVTermElements :: [vterm:t()],
    NextVTermElements :: [vterm:t()],
    Rest :: bitstring(),
    Reason :: term().
internal_binary_to_vterm_elements(0, Rest, Elements) ->
    {ok, lists:reverse(Elements), Rest};
internal_binary_to_vterm_elements(Arity, InternalEncodedElements, Elements) when is_integer(Arity) andalso Arity > 0 ->
    case internal_binary_to_vterm(InternalEncodedElements) of
        {ok, Element, Rest} ->
            internal_binary_to_vterm_elements(Arity - 1, Rest, [Element | Elements])
    end.

-spec internal_binary_to_vterm_fixed_integer(Binary) -> {ok, VTermFixedInteger, Rest} | {error, Reason} when
    Binary :: binary(), VTermFixedInteger :: vterm:fixed_integer_t(), Rest :: bitstring(), Reason :: term().
internal_binary_to_vterm_fixed_integer(Binary) when is_binary(Binary) ->
    case Binary of
        <<?SMALL_INTEGER_EXT:8, Value:8, Rest/bytes>> ->
            {ok, vterm_small_integer_ext:new(Value), Rest};
        <<?INTEGER_EXT:8, Value:1/signed-big-integer-unit:32, Rest/bytes>> ->
            {ok, vterm_integer_ext:new(Value), Rest}
    end.

-spec internal_binary_to_vterm_lazy(Binary, Limit) -> {ok, VTerm, Rest} | {error, Reason} when
    Binary :: binary(), Limit :: integer(), VTerm :: vterm:t(), Rest :: bitstring(), Reason :: term().
internal_binary_to_vterm_lazy(InternalEncodedTerm, Limit) when
    is_binary(InternalEncodedTerm) andalso is_integer(Limit)
->
    OriginalLazyLimit = vterm_lazy_limit_erase(),
    ok = vterm_lazy_limit_set(Limit),
    try
        internal_binary_to_vterm(InternalEncodedTerm)
    after
        ok = vterm_lazy_limit_set(OriginalLazyLimit)
    end.

-spec internal_binary_to_vterm_pairs(Arity, Binary, PrevVTermPairs) -> {ok, NextVTermPairs, Rest} | {error, Reason} when
    Arity :: non_neg_integer(),
    Binary :: binary(),
    PrevVTermPairs :: [{VTermKey, VTermValue}],
    NextVTermPairs :: [{VTermKey, VTermValue}],
    VTermKey :: vterm:t(),
    VTermValue :: vterm:t(),
    Rest :: bitstring(),
    Reason :: term().
internal_binary_to_vterm_pairs(0, Rest, Pairs) ->
    {ok, lists:reverse(Pairs), Rest};
internal_binary_to_vterm_pairs(Arity, InternalEncodedKey, Pairs) when is_integer(Arity) andalso Arity > 0 ->
    case internal_binary_to_vterm(InternalEncodedKey) of
        {ok, Key, InternalEncodedValue} ->
            case internal_binary_to_vterm(InternalEncodedValue) of
                {ok, Value, Rest} ->
                    internal_binary_to_vterm_pairs(Arity - 1, Rest, [{Key, Value} | Pairs])
            end
    end.

-spec internal_binary_to_vterm_pid(Binary) -> {ok, VTermPid, Rest} | {error, Reason} when
    Binary :: binary(), VTermPid :: vterm:pid_t(), Rest :: bitstring(), Reason :: term().
internal_binary_to_vterm_pid(Binary) when is_binary(Binary) ->
    case Binary of
        <<?PID_EXT:8, InternalEncodedPidExt/bytes>> ->
            {ok, Node, <<Id:32, Serial:32, Creation:8, Rest/bytes>>} = internal_binary_to_vterm_atom(
                InternalEncodedPidExt
            ),
            {ok, vterm_pid_ext:new(Node, Id, Serial, Creation), Rest};
        <<?NEW_PID_EXT:8, InternalEncodedNewPidExt/bytes>> ->
            {ok, Node, <<Id:32, Serial:32, Creation:32, Rest/bytes>>} = internal_binary_to_vterm_atom(
                InternalEncodedNewPidExt
            ),
            {ok, vterm_new_pid_ext:new(Node, Id, Serial, Creation), Rest}
    end.

-spec internal_binary_to_vterm_small_integer(Binary) -> {ok, VTermSmallInteger, Rest} | {error, Reason} when
    Binary :: binary(), VTermSmallInteger :: vterm:small_integer_t(), Rest :: bitstring(), Reason :: term().
internal_binary_to_vterm_small_integer(Binary) when is_binary(Binary) ->
    case Binary of
        <<?SMALL_INTEGER_EXT:8, Value:8, Rest/bytes>> ->
            {ok, vterm_small_integer_ext:new(Value), Rest}
    end.

-spec vterm_lazy_limit_dec() -> Limit when Limit :: integer().
vterm_lazy_limit_dec() ->
    case vterm_lazy_limit_get() of
        I when I =< 0 ->
            I;
        I when I > 0 ->
            ok = vterm_lazy_limit_set(I - 1),
            I
    end.

-spec vterm_lazy_limit_erase() -> dynamic().
vterm_lazy_limit_erase() ->
    erase(?LAZY_LIMIT_KEY).

-spec vterm_lazy_limit_get() -> Limit when Limit :: integer().
vterm_lazy_limit_get() ->
    case get(?LAZY_LIMIT_KEY) of
        undefined ->
            -1;
        Limit when is_integer(Limit) ->
            Limit
    end.

-spec vterm_lazy_limit_set
    (undefined) -> ok;
    (Limit) -> ok when Limit :: integer().
vterm_lazy_limit_set(undefined) ->
    _ = vterm_lazy_limit_erase(),
    ok;
vterm_lazy_limit_set(Limit) when is_integer(Limit) ->
    _ = put(?LAZY_LIMIT_KEY, Limit),
    ok.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
