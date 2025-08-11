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
-module(vterm).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("erldist_filter/include/erldist_filter.hrl").

%% API
-export([
    expand/1,
    expand_atom/1,
    expand_fixed_integer/1,
    expand_small_integer/1,
    is_improper_list/1,
    is_string/1,
    resolve_atoms/2,
    simplify/1,
    xform/3
]).

%% Primitive Types
-type i32() :: -2147483648..2147483647.
-type u8() :: 0..255.
-type u16() :: 0..65535.
-type u32() :: 0..4294967295.
-type u64() :: 0..18446744073709551615.

-export_type([
    i32/0,
    u8/0,
    u16/0,
    u32/0,
    u64/0
]).

%% Types
-type t() ::
    vterm_small_integer_ext:t()
    | vterm_integer_ext:t()
    | vterm_float_ext:t()
    | vterm_atom_ext:t()
    | vterm_small_atom_ext:t()
    | vterm_reference_ext:t()
    | vterm_new_reference_ext:t()
    | vterm_newer_reference_ext:t()
    | vterm_port_ext:t()
    | vterm_new_port_ext:t()
    | vterm_new_float_ext:t()
    | vterm_pid_ext:t()
    | vterm_new_pid_ext:t()
    | vterm_small_tuple_ext:t()
    | vterm_large_tuple_ext:t()
    | vterm_nil_ext:t()
    | vterm_string_ext:t()
    | vterm_list_ext:t()
    | vterm_binary_ext:t()
    | vterm_bit_binary_ext:t()
    | vterm_small_big_ext:t()
    | vterm_large_big_ext:t()
    | vterm_new_fun_ext:t()
    | vterm_export_ext:t()
    | vterm_map_ext:t()
    | vterm_atom_utf8_ext:t()
    | vterm_small_atom_utf8_ext:t()
    | vterm_v4_port_ext:t()
    | vterm_atom_cache_ref:t()
    | vterm_atom_cache_ref_resolved:t()
    | vterm_nif_term:t()
    | vterm_lazy_term:t().
-type atom_t() ::
    vterm_atom_ext:t()
    | vterm_small_atom_ext:t()
    | vterm_atom_utf8_ext:t()
    | vterm_small_atom_utf8_ext:t()
    | vterm_atom_cache_ref:t()
    | vterm_atom_cache_ref_resolved:t()
    | vterm_nif_term:t()
    | vterm_lazy_term:t().
-type fixed_integer_t() ::
    vterm_small_integer_ext:t() | vterm_integer_ext:t() | vterm_nif_term:t() | vterm_lazy_term:t().
-type integer_t() ::
    vterm_small_integer_ext:t()
    | vterm_integer_ext:t()
    | vterm_small_big_ext:t()
    | vterm_large_big_ext:t()
    | vterm_nif_term:t()
    | vterm_lazy_term:t().
-type nil_t() :: vterm_nil_ext:t().
-type pid_t() :: vterm_pid_ext:t() | vterm_new_pid_ext:t() | vterm_nif_term:t() | vterm_lazy_term:t().
-type port_t() :: vterm_port_ext:t() | vterm_new_port_ext:t() | vterm_v4_port_ext:t() | vterm_lazy_term:t().
-type reference_t() ::
    vterm_reference_ext:t()
    | vterm_new_reference_ext:t()
    | vterm_newer_reference_ext:t()
    | vterm_nif_term:t()
    | vterm_lazy_term:t().
-type small_integer_t() :: vterm_small_integer_ext:t().
-type the_non_value_t() :: vterm_the_non_value:t().
-type tuple_t() :: vterm_small_tuple_ext:t() | vterm_large_tuple_ext:t() | vterm_lazy_term:t().
-type xform_action() :: cont | skip.
-type xform_result(VT, Acc) :: xform_action() | {xform_action(), Acc} | {xform_action(), VT, Acc}.
-type xform_func(VT, Acc) :: xform_func(VT, Acc, VT, Acc).
-type xform_func(VT0, Acc0, VT1, Acc1) :: fun((VT0, Acc0) -> xform_result(VT1, Acc1)).

-export_type([
    t/0,
    atom_t/0,
    fixed_integer_t/0,
    integer_t/0,
    nil_t/0,
    pid_t/0,
    port_t/0,
    reference_t/0,
    small_integer_t/0,
    the_non_value_t/0,
    tuple_t/0,
    xform_action/0,
    xform_result/2,
    xform_func/2,
    xform_func/4
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec expand(T) -> VT when T :: vterm:t() | dynamic(), VT :: vterm:t().
expand(VT) when ?is_vterm_t(VT) ->
    VT;
expand(T) ->
    case T of
        _ when is_integer(T) andalso ?is_u8(T) ->
            vterm_small_integer_ext:new(T);
        _ when is_integer(T) andalso ?is_i32(T) ->
            vterm_integer_ext:new(T);
        _ when is_float(T) ->
            vterm_new_float_ext:new(<<T:64/float>>);
        _ when is_atom(T) ->
            Name = erlang:atom_to_binary(T, utf8),
            case byte_size(Name) of
                Len when Len =< 255 ->
                    vterm_small_atom_utf8_ext:new(Len, Name);
                Len ->
                    vterm_atom_utf8_ext:new(Len, Name)
            end;
        _ when is_reference(T) ->
            Node = expand_atom(erlang:node(T)),
            case vterm_decode:external_binary_to_vterm(vterm_encode:external_term_to_binary(T)) of
                {ok, VT = #vterm_reference_ext{}, <<>>} ->
                    VT#vterm_reference_ext{node = Node};
                {ok, VT = #vterm_new_reference_ext{}, <<>>} ->
                    VT#vterm_new_reference_ext{node = Node};
                {ok, VT = #vterm_newer_reference_ext{}, <<>>} ->
                    VT#vterm_newer_reference_ext{node = Node}
            end;
        _ when is_port(T) ->
            Node = expand_atom(erlang:node(T)),
            case vterm_decode:external_binary_to_vterm(vterm_encode:external_term_to_binary(T)) of
                {ok, VT = #vterm_port_ext{}, <<>>} ->
                    VT#vterm_port_ext{node = Node};
                {ok, VT = #vterm_new_port_ext{}, <<>>} ->
                    VT#vterm_new_port_ext{node = Node};
                {ok, VT = #vterm_v4_port_ext{}, <<>>} ->
                    VT#vterm_v4_port_ext{node = Node}
            end;
        _ when is_pid(T) ->
            Node = expand_atom(erlang:node(T)),
            case vterm_decode:external_binary_to_vterm(vterm_encode:external_term_to_binary(T)) of
                {ok, VT = #vterm_pid_ext{}, <<>>} ->
                    VT#vterm_pid_ext{node = Node};
                {ok, VT = #vterm_new_pid_ext{}, <<>>} ->
                    VT#vterm_new_pid_ext{node = Node}
            end;
        _ when is_tuple(T) andalso tuple_size(T) =< 255 ->
            Elements = [expand(Element) || Element <- erlang:tuple_to_list(T)],
            Arity = length(Elements),
            vterm_small_tuple_ext:new(Arity, Elements);
        _ when is_tuple(T) ->
            Elements = [expand(Element) || Element <- erlang:tuple_to_list(T)],
            Arity = length(Elements),
            vterm_large_tuple_ext:new(Arity, Elements);
        [] ->
            vterm_nil_ext:new();
        _ when is_list(T) ->
            case is_improper_list(T) of
                true ->
                    expand_improper_list(0, T, []);
                false ->
                    case is_string(T) of
                        true ->
                            Characters = erlang:list_to_binary(T),
                            Len = byte_size(Characters),
                            vterm_string_ext:new(Len, Characters);
                        false ->
                            Elements = [expand(Element) || Element <- T],
                            Len = length(Elements),
                            Tail = expand([]),
                            vterm_list_ext:new(Len, Elements, Tail)
                    end
            end;
        _ when is_binary(T) ->
            Len = byte_size(T),
            vterm_binary_ext:new(Len, T);
        _ when is_bitstring(T) ->
            Len = byte_size(T),
            Pad = (Len * 8) - bit_size(T),
            Bits = 8 - Pad,
            Data = <<T/bits, 0:Pad>>,
            vterm_bit_binary_ext:new(Len, Bits, Data);
        _ when is_integer(T) ->
            case vterm_decode:external_binary_to_vterm(vterm_encode:external_term_to_binary(T)) of
                {ok, VT = #vterm_small_big_ext{}, <<>>} ->
                    VT;
                {ok, VT = #vterm_large_big_ext{}, <<>>} ->
                    VT
            end;
        _ when is_function(T) ->
            case erlang:fun_info(T, type) of
                {type, external} ->
                    {module, Module} = erlang:fun_info(T, module),
                    {name, Name} = erlang:fun_info(T, name),
                    {arity, Arity} = erlang:fun_info(T, arity),
                    vterm_export_ext:new(expand_atom(Module), expand_atom(Name), expand_small_integer(Arity));
                {type, local} ->
                    case vterm_decode:external_binary_to_vterm(vterm_encode:external_term_to_binary(T)) of
                        {ok, VT = #vterm_new_fun_ext{}, <<>>} ->
                            VT
                    end
            end;
        _ when is_map(T) ->
            Arity = map_size(T),
            Iterator = maps:iterator(T),
            Pairs = expand_map_pairs(Arity, Iterator, []),
            vterm_map_ext:new(Arity, Pairs)
    end.

-spec expand_atom(T) -> VT when T :: vterm:atom_t() | atom(), VT :: vterm:atom_t().
expand_atom(VT) when ?is_vterm_atom_t(VT) ->
    VT;
expand_atom(T) when is_atom(T) ->
    case expand(T) of
        VT when ?is_vterm_atom_t(VT) ->
            VT
    end.

-spec expand_fixed_integer(T) -> VT when T :: vterm:fixed_integer_t() | vterm:i32(), VT :: vterm:fixed_integer_t().
expand_fixed_integer(VT) when ?is_vterm_fixed_integer_t(VT) ->
    VT;
expand_fixed_integer(T) when ?is_i32(T) ->
    case expand(T) of
        VT when ?is_vterm_fixed_integer_t(VT) ->
            VT
    end.

-spec expand_small_integer(T) -> VT when T :: vterm:small_integer_t() | vterm:u8(), VT :: vterm:small_integer_t().
expand_small_integer(VT) when ?is_vterm_small_integer_t(VT) ->
    VT;
expand_small_integer(T) when ?is_u8(T) ->
    case expand(T) of
        VT when ?is_vterm_small_integer_t(VT) ->
            VT
    end.

-spec is_improper_list(T) -> boolean() when T :: dynamic().
is_improper_list(L) when is_list(L) ->
    case L of
        _ when length(L) >= 0 ->
            false;
        _ ->
            true
    end;
is_improper_list(_) ->
    false.

-spec is_string(T) -> boolean() when T :: dynamic().
is_string(L) when is_list(L) andalso ?is_u16(length(L)) ->
    Predicate = fun(C) -> ?is_u8(C) end,
    lists:all(Predicate, L);
is_string(_) ->
    false.

-spec resolve_atoms(Atoms, VT) -> VT when Atoms :: vedf:atoms_tuple(), VT :: t().
resolve_atoms(Atoms, VTerm0) ->
    {VTerm1, Atoms} = vterm:xform(VTerm0, Atoms, fun xform_resolve_atoms/2),
    VTerm1.

-spec simplify(VT) -> T when VT :: vterm:t(), T :: dynamic().
simplify(VT) when ?is_vterm_t(VT) ->
    Module = element(1, VT),
    Module:simplify(VT).

-spec xform(VT, Acc, Fun) -> {VT, Acc} when VT :: vterm:t(), Acc :: dynamic(), Fun :: xform_func(VT, Acc).
xform(VT0, Acc0, Fun) when ?is_vterm_t(VT0) andalso is_function(Fun, 2) ->
    % VT0 = xform_lazy(VT),
    case xform_normalize(VT0, Acc0, Fun(VT0, Acc0)) of
        {cont, VT1, Acc1} ->
            case VT1 of
                #vterm_export_ext{module = Module0, function = Function0, arity = Arity0} ->
                    {Module1, Acc2} = xform_expect_atom(Module0, Acc1, Fun),
                    {Function1, Acc3} = xform_expect_atom(Function0, Acc2, Fun),
                    {Arity1, Acc4} = xform_expect_small_integer(Arity0, Acc3, Fun),
                    VT2 = VT1#vterm_export_ext{module = Module1, function = Function1, arity = Arity1},
                    {VT2, Acc4};
                #vterm_new_fun_ext{
                    module = Module0, old_index = OldIndex0, old_uniq = OldUniq0, pid = Pid0, free_vars = FreeVars0
                } ->
                    {Module1, Acc2} = xform_expect_atom(Module0, Acc1, Fun),
                    {OldIndex1, Acc3} = xform_expect_fixed_integer(OldIndex0, Acc2, Fun),
                    {OldUniq1, Acc4} = xform_expect_fixed_integer(OldUniq0, Acc3, Fun),
                    {Pid1, Acc5} = xform_expect_pid(Pid0, Acc4, Fun),
                    {FreeVars1, Acc6} = xform_elements(FreeVars0, Acc5, Fun, []),
                    VT2 = VT1#vterm_new_fun_ext{
                        module = Module1, old_index = OldIndex1, old_uniq = OldUniq1, pid = Pid1, free_vars = FreeVars1
                    },
                    {VT2, Acc6};
                #vterm_list_ext{elements = Elements0, tail = Tail0} ->
                    {Elements1, Acc2} = xform_elements(Elements0, Acc1, Fun, []),
                    {Tail1, Acc3} = xform(Tail0, Acc2, Fun),
                    VT2 = VT1#vterm_list_ext{elements = Elements1, tail = Tail1},
                    {VT2, Acc3};
                #vterm_map_ext{pairs = Pairs0} ->
                    {Pairs1, Acc2} = xform_pairs(Pairs0, Acc1, Fun, []),
                    VT2 = VT1#vterm_map_ext{pairs = Pairs1},
                    {VT2, Acc2};
                #vterm_pid_ext{node = Node0} ->
                    {Node1, Acc2} = xform_expect_atom(Node0, Acc1, Fun),
                    VT2 = VT1#vterm_pid_ext{node = Node1},
                    {VT2, Acc2};
                #vterm_new_pid_ext{node = Node0} ->
                    {Node1, Acc2} = xform_expect_atom(Node0, Acc1, Fun),
                    VT2 = VT1#vterm_new_pid_ext{node = Node1},
                    {VT2, Acc2};
                #vterm_port_ext{node = Node0} ->
                    {Node1, Acc2} = xform_expect_atom(Node0, Acc1, Fun),
                    VT2 = VT1#vterm_port_ext{node = Node1},
                    {VT2, Acc2};
                #vterm_new_port_ext{node = Node0} ->
                    {Node1, Acc2} = xform_expect_atom(Node0, Acc1, Fun),
                    VT2 = VT1#vterm_new_port_ext{node = Node1},
                    {VT2, Acc2};
                #vterm_v4_port_ext{node = Node0} ->
                    {Node1, Acc2} = xform_expect_atom(Node0, Acc1, Fun),
                    VT2 = VT1#vterm_v4_port_ext{node = Node1},
                    {VT2, Acc2};
                #vterm_reference_ext{node = Node0} ->
                    {Node1, Acc2} = xform_expect_atom(Node0, Acc1, Fun),
                    VT2 = VT1#vterm_reference_ext{node = Node1},
                    {VT2, Acc2};
                #vterm_new_reference_ext{node = Node0} ->
                    {Node1, Acc2} = xform_expect_atom(Node0, Acc1, Fun),
                    VT2 = VT1#vterm_new_reference_ext{node = Node1},
                    {VT2, Acc2};
                #vterm_newer_reference_ext{node = Node0} ->
                    {Node1, Acc2} = xform_expect_atom(Node0, Acc1, Fun),
                    VT2 = VT1#vterm_newer_reference_ext{node = Node1},
                    {VT2, Acc2};
                #vterm_small_tuple_ext{elements = Elements0} ->
                    {Elements1, Acc2} = xform_elements(Elements0, Acc1, Fun, []),
                    VT2 = VT1#vterm_small_tuple_ext{elements = Elements1},
                    {VT2, Acc2};
                #vterm_large_tuple_ext{elements = Elements0} ->
                    {Elements1, Acc2} = xform_elements(Elements0, Acc1, Fun, []),
                    VT2 = VT1#vterm_large_tuple_ext{elements = Elements1},
                    {VT2, Acc2};
                %% non-container types
                #vterm_atom_cache_ref_resolved{} ->
                    {VT1, Acc1};
                #vterm_atom_cache_ref{} ->
                    {VT1, Acc1};
                #vterm_atom_ext{} ->
                    {VT1, Acc1};
                #vterm_atom_utf8_ext{} ->
                    {VT1, Acc1};
                #vterm_binary_ext{} ->
                    {VT1, Acc1};
                #vterm_bit_binary_ext{} ->
                    {VT1, Acc1};
                #vterm_float_ext{} ->
                    {VT1, Acc1};
                #vterm_integer_ext{} ->
                    {VT1, Acc1};
                #vterm_large_big_ext{} ->
                    {VT1, Acc1};
                #vterm_new_float_ext{} ->
                    {VT1, Acc1};
                #vterm_nif_term{} ->
                    {VT1, Acc1};
                #vterm_nil_ext{} ->
                    {VT1, Acc1};
                #vterm_small_atom_ext{} ->
                    {VT1, Acc1};
                #vterm_small_atom_utf8_ext{} ->
                    {VT1, Acc1};
                #vterm_small_big_ext{} ->
                    {VT1, Acc1};
                #vterm_small_integer_ext{} ->
                    {VT1, Acc1};
                #vterm_string_ext{} ->
                    {VT1, Acc1};
                #vterm_the_non_value{} ->
                    {VT1, Acc1};
                %% lazy types
                #vterm_lazy_term{} ->
                    {VT1, Acc1}
            end;
        {skip, VT1, Acc1} ->
            {VT1, Acc1}
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-spec expand_improper_list(
    Len :: integer(),
    List :: dynamic(),
    Elements :: list()
) -> vterm:t().
expand_improper_list(Len, [Head | Tail], Elements) ->
    expand_improper_list(Len + 1, Tail, [expand(Head) | Elements]);
expand_improper_list(Len, Tail, Elements) ->
    vterm_list_ext:new(Len, lists:reverse(Elements), expand(Tail)).

-spec expand_map_pairs(
    Arity :: integer(),
    Iterator :: maps:iterator(),
    Pairs :: [{vterm:t(), vterm:t()}]
) -> [{vterm:t(), vterm:t()}].
expand_map_pairs(0, Iterator, Pairs) ->
    none = maps:next(Iterator),
    lists:reverse(Pairs);
expand_map_pairs(Arity, Iterator, Pairs) when is_integer(Arity) andalso Arity > 0 ->
    {Key, Value, NextIterator} = maps:next(Iterator),
    expand_map_pairs(Arity - 1, NextIterator, [{expand(Key), expand(Value)} | Pairs]).

-spec xform_elements(
    Elements,
    Acc,
    Fun :: xform_func(VT, Acc, VT, Acc),
    Elements
) -> {Elements, Acc :: dynamic()} when Elements :: [vterm:t()], VT :: vterm:t(), Acc :: dynamic().
xform_elements([Element0 | In], Acc0, Fun, Out) ->
    {Element1, Acc1} = xform(Element0, Acc0, Fun),
    xform_elements(In, Acc1, Fun, [Element1 | Out]);
xform_elements([], Acc, _Fun, Out) ->
    {lists:reverse(Out), Acc}.

-spec xform_expect_atom(VT0, Acc, Fun) -> {VT1, Acc} when
    VT0 :: vterm:t(),
    VT1 :: vterm:atom_t(),
    Acc :: dynamic(),
    Fun :: xform_func(VT0, Acc, VT1, Acc) | dynamic().
xform_expect_atom(VT0, Acc0, Fun) ->
    case xform(VT0, Acc0, Fun) of
        Result = {VT1, _Acc1} when ?is_vterm_atom_t(VT1) ->
            Result
    end.

-spec xform_expect_fixed_integer(VT0, Acc, Fun) -> {VT1, Acc} when
    VT0 :: vterm:t(),
    VT1 :: vterm:fixed_integer_t(),
    Acc :: dynamic(),
    Fun :: xform_func(VT0, Acc, VT1, Acc) | dynamic().
xform_expect_fixed_integer(VT0, Acc0, Fun) ->
    case xform(VT0, Acc0, Fun) of
        Result = {VT1, _Acc1} when ?is_vterm_fixed_integer_t(VT1) ->
            Result
    end.

-spec xform_expect_pid(VT0, Acc, Fun) -> {VT1, Acc} when
    VT0 :: vterm:t(),
    VT1 :: vterm:pid_t(),
    Acc :: dynamic(),
    Fun :: xform_func(VT0, Acc, VT1, Acc) | dynamic().
xform_expect_pid(VT0, Acc0, Fun) ->
    case xform(VT0, Acc0, Fun) of
        Result = {VT1, _Acc1} when ?is_vterm_pid_t(VT1) ->
            Result
    end.

-spec xform_expect_small_integer(VT0, Acc, Fun) -> {VT1, Acc} when
    VT0 :: vterm:t(),
    VT1 :: vterm:small_integer_t(),
    Acc :: dynamic(),
    Fun :: xform_func(VT0, Acc, VT1, Acc) | dynamic().
xform_expect_small_integer(VT0, Acc0, Fun) ->
    case xform(VT0, Acc0, Fun) of
        Result = {VT1, _Acc1} when ?is_vterm_small_integer_t(VT1) ->
            Result
    end.

% %% @private
% xform_lazy(#vterm_lazy_term{slice = Slice}) ->
%     {ok, VT, <<>>} = vterm_decode:internal_binary_to_vterm(Slice),
%     VT;
% xform_lazy(VT) ->
%     VT.

-spec xform_normalize(OldVTerm, AccIn, Action) -> {Action, NewVTerm, AccOut} when
    OldVTerm :: vterm:t(),
    NewVTerm :: dynamic(),
    AccIn :: dynamic(),
    AccOut :: dynamic(),
    Action :: xform_result(vterm:t(), dynamic()).
xform_normalize(OldVTerm, AccIn, Action) when is_atom(Action) ->
    {Action, OldVTerm, AccIn};
xform_normalize(OldVTerm, _AccIn, {Action, AccOut}) when is_atom(Action) ->
    {Action, OldVTerm, AccOut};
xform_normalize(_OldVTerm, _AccIn, {Action, NewVTerm, AccOut}) when is_atom(Action) ->
    {Action, NewVTerm, AccOut}.

-spec xform_pairs(
    [{K, V}],
    Acc,
    Fun :: xform_func(K, Acc, K, Acc),
    [{K, V}]
) -> {[{K, V}], Acc} when K :: vterm:t(), V :: vterm:t(), Acc :: dynamic().
xform_pairs([{Key0, Value0} | In], Acc0, Fun, Out) ->
    {Key1, Acc1} = xform(Key0, Acc0, Fun),
    {Value1, Acc2} = xform(Value0, Acc1, Fun),
    xform_pairs(In, Acc2, Fun, [{Key1, Value1} | Out]);
xform_pairs([], Acc, _Fun, Out) ->
    {lists:reverse(Out), Acc}.

-spec xform_resolve_atoms(VT, Atoms) -> xform_result(vterm:t(), dynamic()) when
    VT :: vterm:t(),
    Atoms :: dynamic().
xform_resolve_atoms(#vterm_atom_cache_ref{index = Index}, Atoms) ->
    Atom = element(Index + 1, Atoms),
    {cont, vterm_atom_cache_ref_resolved:new(Index, Atom), Atoms};
xform_resolve_atoms(VT, Atoms) ->
    {cont, VT, Atoms}.
