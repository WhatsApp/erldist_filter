%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% Created :  12 Oct 2022 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
-module(proper_vterm).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(warn_missing_spec_all).

-include_lib("erldist_filter_test/include/proper_erldist_filter_test.hrl").
-include_lib("erldist_filter/include/erldist_filter.hrl").

%% Public API
-export([
    mostly/2,
    i32/0,
    u8/0,
    u16/0,
    u32/0,
    u64/0,
    vterm/0,
    vterm/1,
    term_expand_into_vterm/0,
    vterm_simplify_into_term/0,
    vterm_simplify_into_term/1,
    vterm_atom/0,
    vterm_atom/1,
    vterm_atom_cache_ref/0,
    vterm_atom_ext/0,
    vterm_atom_utf8_ext/0,
    vterm_binary_ext/0,
    vterm_bit_binary_ext/0,
    vterm_export_ext/0,
    vterm_export_ext/1,
    vterm_fixed_integer/0,
    vterm_fixed_integer/2,
    vterm_float_ext/0,
    vterm_integer/0,
    vterm_integer/2,
    vterm_integer_ext/0,
    vterm_large_big_ext/0,
    vterm_large_binary/0,
    vterm_large_tuple_ext/0,
    vterm_large_tuple_ext/1,
    vterm_list_ext/0,
    vterm_list_ext/1,
    vterm_map_ext/0,
    vterm_map_ext/1,
    vterm_new_float_ext/0,
    vterm_new_fun_ext/0,
    vterm_new_fun_ext/1,
    vterm_new_pid_ext/0,
    vterm_new_pid_ext/1,
    vterm_new_port_ext/0,
    vterm_new_port_ext/1,
    vterm_new_reference_ext/0,
    vterm_new_reference_ext/1,
    vterm_newer_reference_ext/0,
    vterm_newer_reference_ext/1,
    vterm_nil_ext/0,
    vterm_pid/0,
    vterm_pid/1,
    vterm_pid_ext/0,
    vterm_pid_ext/1,
    vterm_port_ext/0,
    vterm_port_ext/1,
    vterm_proper_list/0,
    vterm_proper_list/1,
    vterm_proper_list/2,
    vterm_reference/0,
    vterm_reference/1,
    vterm_reference_ext/0,
    vterm_reference_ext/1,
    vterm_small_atom_utf8_ext/0,
    vterm_small_big_ext/0,
    vterm_small_integer_ext/0,
    vterm_small_tuple_ext/0,
    vterm_small_tuple_ext/1,
    vterm_string_ext/0,
    vterm_v4_port_ext/0,
    vterm_v4_port_ext/1,
    check_for_map_bug/1
]).

-type options() :: #{
    allow_atom_cache_refs => boolean(),
    large_binaries => boolean()
}.

-export_type([
    options/0
]).

%%%=============================================================================
%%% Public API functions
%%%=============================================================================

-spec mostly(U :: proper_types:type(), T :: proper_types:type()) -> proper_types:type().
mostly(U, T) ->
    frequency([
        {100, U},
        {1, T}
    ]).

-spec i32() -> proper_types:type().
i32() ->
    integer(-16#FFFFFFFF - 1, 16#FFFFFFFF).

-spec u8() -> proper_types:type().
u8() ->
    integer(0, 16#FF).

-spec u16() -> proper_types:type().
u16() ->
    integer(0, 16#FFFF).

-spec u32() -> proper_types:type().
u32() ->
    integer(0, 16#FFFFFFFF).

-spec u64() -> proper_types:type().
u64() ->
    integer(0, 16#FFFFFFFFFFFFFFFF).

-spec vterm() -> proper_types:type().
vterm() ->
    vterm(#{}).

-spec vterm(proper_vterm:options()) -> proper_types:type().
vterm(Opts) ->
    ?SUCHTHAT(
        VTerm,
        frequency(
            lists:append([
                [
                    {1, term_expand_into_vterm()},
                    % {1, ?LET(Term, vterm_simplify_into_term(), vterm:expand(Term))},
                    {1, vterm_atom_ext()},
                    {1, vterm_atom_utf8_ext()},
                    {1, vterm_binary_ext()},
                    {1, vterm_bit_binary_ext()},
                    {1, vterm_export_ext(Opts)},
                    {1, vterm_float_ext()},
                    {1, vterm_integer_ext()},
                    {1, vterm_large_big_ext()},
                    {1, vterm_large_tuple_ext(Opts)},
                    {1, vterm_list_ext(Opts)},
                    % NOTE: heap-based map decoding is broken in OTP right now
                    {1, vterm_map_ext(Opts)},
                    {1, vterm_new_float_ext()},
                    {1, vterm_new_fun_ext(Opts)},
                    {1, vterm_new_pid_ext(Opts)},
                    {1, vterm_new_port_ext(Opts)},
                    {1, vterm_new_reference_ext(Opts)},
                    {1, vterm_newer_reference_ext(Opts)},
                    {1, vterm_nil_ext()},
                    {1, vterm_pid_ext(Opts)},
                    {1, vterm_port_ext(Opts)},
                    {1, vterm_reference_ext(Opts)},
                    {1, vterm_small_atom_utf8_ext()},
                    {1, vterm_small_big_ext()},
                    {1, vterm_small_integer_ext()},
                    {1, vterm_small_tuple_ext(Opts)},
                    {1, vterm_string_ext()},
                    {1, vterm_v4_port_ext(Opts)}
                ],
                case Opts of
                    #{allow_atom_cache_refs := true} -> [{1, vterm_atom_cache_ref()}];
                    _ -> []
                end,
                case Opts of
                    #{large_binaries := true} -> [{1, vterm_large_binary()}];
                    _ -> []
                end
            ])
        ),
        case Opts of
            #{allow_atom_cache_refs := true} ->
                true;
            _ ->
                %% Uncomment below to force expensive check for map bug
                % VTermBinary = vterm_encode:external_vterm_to_binary(VTerm, Opts),
                % {ok, Term0, <<>>} = vterm_decode:external_binary_to_term(VTermBinary),
                % Term0Binary = vterm_encode:external_term_to_binary(Term0),
                % {ok, ExpectedTerm, <<>>} = vterm_decode:external_binary_to_term(Term0Binary),
                % Term1 = vterm:simplify(VTerm),
                % Term1Binary = vterm_encode:external_term_to_binary(Term1),
                % {ok, Term2, <<>>} = vterm_decode:external_binary_to_term(Term1Binary),
                % check_for_map_bug(Term0) andalso ExpectedTerm =:= Term0 andalso Term0 =:= Term2
                _ = VTerm,
                true
        end
    ).

-spec term_expand_into_vterm() -> proper_types:type().
term_expand_into_vterm() ->
    ?LET(Term, term(), vterm:expand(Term)).

-spec vterm_simplify_into_term() -> proper_types:type().
vterm_simplify_into_term() ->
    vterm_simplify_into_term(#{}).

-spec vterm_simplify_into_term(proper_vterm:options()) -> proper_types:type().
vterm_simplify_into_term(Opts) ->
    ?LET(VTerm, vterm(Opts), vterm:simplify(VTerm)).

-spec vterm_atom() -> proper_types:type().
vterm_atom() ->
    vterm_atom(#{}).

-spec vterm_atom(proper_vterm:options()) -> proper_types:type().
vterm_atom(Opts) ->
    frequency([
        % {1, vterm_atom_cache_ref()},
        {1, vterm_atom_ext()},
        {1, vterm_atom_utf8_ext()},
        {1, vterm_small_atom_ext()},
        {1, vterm_small_atom_utf8_ext()}
        | case Opts of
            #{allow_atom_cache_refs := true} -> [{1, vterm_atom_cache_ref()}];
            _ -> []
        end
    ]).

-spec vterm_atom_cache_ref() -> proper_types:type().
vterm_atom_cache_ref() ->
    ?LET(Index, u8(), vterm_atom_cache_ref:new(Index)).

-spec vterm_atom_ext() -> proper_types:type().
vterm_atom_ext() ->
    % technically, 510 is possible
    MaxAtomSize = 255,
    ?LET(
        {Len, Name}, ?LET(Size, integer(0, MaxAtomSize), {exactly(Size), binary(Size)}), vterm_atom_ext:new(Len, Name)
    ).

-spec vterm_atom_utf8_ext() -> proper_types:type().
vterm_atom_utf8_ext() ->
    % technically, 1020 is possible
    MaxAtomSize = 255,
    ?LET(
        Name,
        ?LET(
            Size,
            integer(0, MaxAtomSize),
            ?SUCHTHAT(AtomText, proper_unicode:utf8(Size), byte_size(AtomText) =< MaxAtomSize)
        ),
        vterm_atom_utf8_ext:new(byte_size(Name), Name)
    ).

-spec vterm_binary_ext() -> proper_types:type().
vterm_binary_ext() ->
    ?LET(Data, binary(), vterm_binary_ext:new(byte_size(Data), Data)).

-spec vterm_bit_binary_ext() -> proper_types:type().
vterm_bit_binary_ext() ->
    ?LET({Data, Bits}, {non_empty(binary()), integer(1, 7)}, vterm_bit_binary_ext:new(byte_size(Data), Bits, Data)).

-spec vterm_export_ext() -> proper_types:type().
vterm_export_ext() ->
    vterm_export_ext(#{}).

-spec vterm_export_ext(proper_vterm:options()) -> proper_types:type().
vterm_export_ext(Opts) ->
    ?LET(
        {Module, Function, Arity},
        {vterm_atom(Opts), vterm_atom(Opts), vterm_small_integer_ext()},
        vterm_export_ext:new(Module, Function, Arity)
    ).

-spec vterm_fixed_integer() -> proper_types:type().
vterm_fixed_integer() ->
    oneof([
        vterm_integer_ext(),
        vterm_small_integer_ext()
    ]).

-spec vterm_fixed_integer(Low :: proper_types:extint(), High :: proper_types:extint()) -> proper_types:type().
vterm_fixed_integer(Low0, High0) ->
    LowLimit = -16#FFFFFFFF - 1,
    HighLimit = 16#FFFFFFFF,
    Low =
        case Low0 of
            inf ->
                LowLimit;
            _ when is_integer(Low0) andalso Low0 < LowLimit ->
                LowLimit;
            _ when is_integer(Low0) ->
                Low0
        end,
    High =
        case High0 of
            inf ->
                HighLimit;
            _ when is_integer(High0) andalso High0 > HighLimit ->
                HighLimit;
            _ when is_integer(High0) ->
                High0
        end,
    ?LET(Integer, integer(Low, High), vterm:expand(Integer)).

-spec vterm_float_ext() -> proper_types:type().
vterm_float_ext() ->
    ?LET(Float, float(), vterm_float_ext:new(erlang:iolist_to_binary(io_lib:format("~-31.20.*e", [0, Float])))).

-spec vterm_integer() -> proper_types:type().
vterm_integer() ->
    oneof([
        vterm_integer_ext(),
        vterm_small_integer_ext(),
        vterm_small_big_ext(),
        vterm_large_big_ext()
    ]).

-spec vterm_integer(Low :: proper_types:extint(), High :: proper_types:extint()) -> proper_types:type().
vterm_integer(Low, High) ->
    ?LET(Integer, integer(Low, High), vterm:expand(Integer)).

-spec vterm_integer_ext() -> proper_types:type().
vterm_integer_ext() ->
    ?LET(Value, i32(), vterm_integer_ext:new(Value)).

-spec vterm_large_big_ext() -> proper_types:type().
vterm_large_big_ext() ->
    ?LET(
        {N, Sign, D}, ?LET({N, Sign}, {u32(), oneof([0, 1])}, {N, Sign, binary(N)}), vterm_large_big_ext:new(N, Sign, D)
    ).

-spec vterm_large_binary() -> proper_types:type().
vterm_large_binary() ->
    ?LET(
        {Byte, Size},
        % {byte(), integer(2 * 64 * 1024, 1 * 1024 * 1024)},
        {byte(), integer(2 * 255, 5 * 255)},
        vterm_binary_ext:new(Size, binary:copy(<<Byte>>, Size))
    ).

-spec vterm_large_tuple_ext() -> proper_types:type().
vterm_large_tuple_ext() ->
    vterm_large_tuple_ext(#{}).

-spec vterm_large_tuple_ext(proper_vterm:options()) -> proper_types:type().
vterm_large_tuple_ext(Opts) ->
    ?LET(
        {Arity, Elements},
        ?LET(Arity, mostly(integer(0, 4), u32()), {Arity, vector(Arity, vterm(Opts))}),
        vterm_large_tuple_ext:new(Arity, Elements)
    ).

-spec vterm_list_ext() -> proper_types:type().
vterm_list_ext() ->
    vterm_list_ext(#{}).

-spec vterm_list_ext(proper_vterm:options()) -> proper_types:type().
vterm_list_ext(Opts) ->
    ?LET(
        {Len, Elements, Tail},
        ?LET(Len, mostly(integer(0, 4), u32()), {Len, vector(Len, vterm(Opts)), mostly(vterm_nil_ext(), vterm(Opts))}),
        vterm_list_ext:new(Len, Elements, Tail)
    ).

-spec vterm_map_ext() -> proper_types:type().
vterm_map_ext() ->
    vterm_map_ext(#{}).

-spec vterm_map_ext(proper_vterm:options()) -> proper_types:type().
vterm_map_ext(Opts) ->
    ?LET(
        {Arity, Pairs},
        ?LET(
            Arity,
            mostly(integer(0, 4), integer(5, 33)),
            {Arity, vterm_map_ext_pairs(Arity, Opts)}
        ),
        vterm_map_ext:new(Arity, Pairs)
    ).

-spec vterm_map_ext_pairs(non_neg_integer(), proper_vterm:options()) -> proper_types:type().
vterm_map_ext_pairs(Arity, Opts) ->
    %% Sort the keys (easier for humans to read when debugging failures)
    ?LET(
        UnsortedPairs,
        %% No duplicate keys allowed in a map
        ?SUCHTHAT(
            UnsortedPairs,
            vector(Arity, {vterm(Opts), vterm(Opts)}),
            begin
                SetOfKeys = sets:from_list([simplify_map_key(Key) || {Key, _Value} <- UnsortedPairs], [{version, 2}]),
                sets:size(SetOfKeys) =:= Arity
            end
        ),
        begin
            WrapSorted = lists:sort([
                {simplify_map_key(Key), {Key, Value}}
             || {Key, Value} <- UnsortedPairs
            ]),
            SortedPairs = [Pair || {_SortKey, Pair} <- WrapSorted],
            SortedPairs
        end
    ).

-spec vterm_new_float_ext() -> proper_types:type().
vterm_new_float_ext() ->
    ?LET(Float, float(), vterm_new_float_ext:new(<<Float:64/float>>)).

-spec vterm_new_fun_ext() -> proper_types:type().
vterm_new_fun_ext() ->
    vterm_new_fun_ext(#{}).

-spec vterm_new_fun_ext(proper_vterm:options()) -> proper_types:type().
vterm_new_fun_ext(Opts) ->
    ?LET(
        {Arity, Uniq, Index, NumFree, Module, OldIndex, OldUniq, Pid, FreeVars},
        ?LET(
            NumFree,
            mostly(integer(0, 4), u8()),
            {
                u8(),
                bitstring(128),
                u32(),
                NumFree,
                vterm_atom(Opts),
                vterm_fixed_integer(),
                vterm_fixed_integer(),
                vterm_pid(Opts),
                vector(NumFree, vterm(Opts))
            }
        ),
        vterm_new_fun_ext:new_with_derived_size(Arity, Uniq, Index, NumFree, Module, OldIndex, OldUniq, Pid, FreeVars)
    ).

-spec vterm_new_pid_ext() -> proper_types:type().
vterm_new_pid_ext() ->
    vterm_new_pid_ext(#{}).

-spec vterm_new_pid_ext(proper_vterm:options()) -> proper_types:type().
vterm_new_pid_ext(Opts) ->
    ?LET(
        {Node, Id, Serial, Creation},
        {vterm_atom(Opts), u32(), u32(), u32()},
        vterm_new_pid_ext:new(Node, Id, Serial, Creation)
    ).

-spec vterm_new_port_ext() -> proper_types:type().
vterm_new_port_ext() ->
    vterm_new_port_ext(#{}).

-spec vterm_new_port_ext(proper_vterm:options()) -> proper_types:type().
vterm_new_port_ext(Opts) ->
    ?LET({Node, Id, Creation}, {vterm_atom(Opts), u32(), u32()}, vterm_new_port_ext:new(Node, Id, Creation)).

-spec vterm_new_reference_ext() -> proper_types:type().
vterm_new_reference_ext() ->
    vterm_new_reference_ext(#{}).

-spec vterm_new_reference_ext(proper_vterm:options()) -> proper_types:type().
vterm_new_reference_ext(Opts) ->
    ?LET(
        {Len, Node, Creation, Ids},
        ?LET(Len, integer(1, 5), {Len, vterm_atom(Opts), integer(0, 3), vector(Len, u32())}),
        vterm_new_reference_ext:new(Len, Node, Creation, Ids)
    ).

-spec vterm_newer_reference_ext() -> proper_types:type().
vterm_newer_reference_ext() ->
    vterm_newer_reference_ext(#{}).

-spec vterm_newer_reference_ext(proper_vterm:options()) -> proper_types:type().
vterm_newer_reference_ext(Opts) ->
    ?LET(
        {Len, Node, Creation, Ids},
        ?LET(Len, integer(1, 5), {Len, vterm_atom(Opts), u32(), vector(Len, u32())}),
        vterm_newer_reference_ext:new(Len, Node, Creation, Ids)
    ).

-spec vterm_nil_ext() -> proper_types:type().
vterm_nil_ext() ->
    exactly(vterm_nil_ext:new()).

-spec vterm_pid() -> proper_types:type().
vterm_pid() ->
    vterm_pid(#{}).

-spec vterm_pid(proper_vterm:options()) -> proper_types:type().
vterm_pid(Opts) ->
    frequency([
        {1, vterm_pid_ext(Opts)},
        {1, vterm_new_pid_ext(Opts)}
    ]).

-spec vterm_pid_ext() -> proper_types:type().
vterm_pid_ext() ->
    vterm_pid_ext(#{}).

-spec vterm_pid_ext(proper_vterm:options()) -> proper_types:type().
vterm_pid_ext(Opts) ->
    ?LET(
        {Node, Id, Serial, Creation},
        {vterm_atom(Opts), u32(), u32(), integer(0, 3)},
        vterm_pid_ext:new(Node, Id, Serial, Creation)
    ).

-spec vterm_port_ext() -> proper_types:type().
vterm_port_ext() ->
    vterm_port_ext(#{}).

-spec vterm_port_ext(proper_vterm:options()) -> proper_types:type().
vterm_port_ext(Opts) ->
    ?LET({Node, Id, Creation}, {vterm_atom(Opts), u32(), integer(0, 3)}, vterm_port_ext:new(Node, Id, Creation)).

-spec vterm_proper_list() -> proper_types:type().
vterm_proper_list() ->
    vterm_proper_list(#{}).

-spec vterm_proper_list(proper_vterm:options()) -> proper_types:type().
vterm_proper_list(Opts) ->
    ?LET(
        {Len, Elements, Tail},
        ?LET(Len, mostly(integer(0, 4), u32()), {Len, vector(Len, vterm(Opts)), vterm_nil_ext()}),
        vterm_list_ext:new(Len, Elements, Tail)
    ).

-spec vterm_proper_list(proper_types:length(), proper_vterm:options()) -> proper_types:type().
vterm_proper_list(0, _Opts) ->
    vterm_nil_ext();
vterm_proper_list(Len, Opts) when is_integer(Len) andalso Len > 0 ->
    ?LET(
        {Elements, Tail},
        {vector(Len, vterm(Opts)), vterm_nil_ext()},
        vterm_list_ext:new(Len, Elements, Tail)
    ).

-spec vterm_reference() -> proper_types:type().
vterm_reference() ->
    vterm_reference(#{}).

-spec vterm_reference(proper_vterm:options()) -> proper_types:type().
vterm_reference(Opts) ->
    frequency([
        {1, vterm_reference_ext(Opts)},
        {1, vterm_new_reference_ext(Opts)},
        {10, vterm_newer_reference_ext(Opts)}
    ]).

-spec vterm_reference_ext() -> proper_types:type().
vterm_reference_ext() ->
    vterm_reference_ext(#{}).

-spec vterm_reference_ext(proper_vterm:options()) -> proper_types:type().
vterm_reference_ext(Opts) ->
    ?LET({Node, Id, Creation}, {vterm_atom(Opts), u32(), integer(0, 3)}, vterm_reference_ext:new(Node, Id, Creation)).

-spec vterm_small_atom_ext() -> proper_types:type().
vterm_small_atom_ext() ->
    ?LET({Len, Name}, ?LET(Size, integer(0, 255), {exactly(Size), binary(Size)}), vterm_small_atom_ext:new(Len, Name)).

-spec vterm_small_atom_utf8_ext() -> proper_types:type().
vterm_small_atom_utf8_ext() ->
    MaxAtomSize = 255,
    ?LET(
        Name,
        ?LET(
            Size,
            integer(0, MaxAtomSize),
            ?SUCHTHAT(AtomText, proper_unicode:utf8(Size), byte_size(AtomText) =< MaxAtomSize)
        ),
        vterm_small_atom_utf8_ext:new(byte_size(Name), Name)
    ).

-spec vterm_small_big_ext() -> proper_types:type().
vterm_small_big_ext() ->
    ?LET(
        {N, Sign, D}, ?LET({N, Sign}, {u8(), oneof([0, 1])}, {N, Sign, binary(N)}), vterm_small_big_ext:new(N, Sign, D)
    ).

-spec vterm_small_integer_ext() -> proper_types:type().
vterm_small_integer_ext() ->
    ?LET(Value, integer(0, 255), vterm_small_integer_ext:new(Value)).

-spec vterm_small_tuple_ext() -> proper_types:type().
vterm_small_tuple_ext() ->
    vterm_small_tuple_ext(#{}).

-spec vterm_small_tuple_ext(proper_vterm:options()) -> proper_types:type().
vterm_small_tuple_ext(Opts) ->
    ?LET(
        {Arity, Elements},
        ?LET(Arity, mostly(integer(0, 4), u8()), {Arity, vector(Arity, vterm(Opts))}),
        vterm_small_tuple_ext:new(Arity, Elements)
    ).

-spec vterm_string_ext() -> proper_types:type().
vterm_string_ext() ->
    ?LET(
        {Len, Characters},
        ?LET(Len, mostly(integer(0, 128), u16()), {Len, binary(Len)}),
        vterm_string_ext:new(Len, Characters)
    ).

-spec vterm_v4_port_ext() -> proper_types:type().
vterm_v4_port_ext() ->
    vterm_v4_port_ext(#{}).

-spec vterm_v4_port_ext(proper_vterm:options()) -> proper_types:type().
vterm_v4_port_ext(Opts) ->
    ?LET({Node, Id, Creation}, {vterm_atom(Opts), u64(), u32()}, vterm_v4_port_ext:new(Node, Id, Creation)).

-spec check_for_map_bug(dynamic()) -> boolean().
check_for_map_bug([]) ->
    true;
check_for_map_bug([H | T]) ->
    case check_for_map_bug(H) of
        true ->
            check_for_map_bug(T);
        false ->
            false
    end;
check_for_map_bug(M) when is_map(M) ->
    Ks = maps:keys(M),
    check_for_map_bug(Ks, M);
check_for_map_bug(T) when is_tuple(T) ->
    check_for_map_bug(erlang:tuple_to_list(T));
check_for_map_bug(_) ->
    true.

-spec check_for_map_bug(list(), map()) -> boolean().
check_for_map_bug([], _M) ->
    true;
check_for_map_bug([K | Ks], M) ->
    case check_for_map_bug(K) of
        true ->
            try maps:get(K, M) of
                V ->
                    case check_for_map_bug(V) of
                        true ->
                            check_for_map_bug(Ks, M);
                        false ->
                            false
                    end
            catch
                error:{badkey, _} ->
                    false
            end;
        false ->
            false
    end.

-spec simplify_map_key(vterm:t()) -> vterm:t().
simplify_map_key(VT0) ->
    {VT1, undefined} = vterm:xform(VT0, undefined, fun simplify_map_key/2),
    vterm:simplify(VT1).

-spec simplify_map_key(vterm:t(), undefined) -> vterm:xform_result(vterm:t(), undefined).
simplify_map_key(#vterm_atom_cache_ref{index = Index}, Acc) ->
    IndexAtom = erlang:list_to_atom("ATOM_CACHE_REF:" ++ erlang:integer_to_list(Index)),
    {cont, vterm:expand(IndexAtom), Acc};
simplify_map_key(_VTerm, _Acc) ->
    cont.
