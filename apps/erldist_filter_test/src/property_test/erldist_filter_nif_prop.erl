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
%%% Created :  19 May 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(erldist_filter_nif_prop).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").
-compile(nowarn_missing_spec).

-include_lib("proper/include/proper.hrl").

-include_lib("erldist_filter/include/erldist_filter.hrl").

%% Helpers
-export([
    vdist_test_vectors_derive_atoms/1,
    vterm_report_not_equal/2,
    vterm_reference_atoms/1,
    vterm_resolve_atoms/2,
    vterm_test_vectors_resolve_atoms/1
]).
%% Properties
-export([
    prop_dist_ext_to_vdist_2/1,
    prop_dist_ext_to_vterm_2/1,
    prop_dist_ext_to_vterm_3/1,
    prop_dist_int_to_vdist_2/1,
    prop_dist_int_to_vterm_2/1,
    prop_dist_int_to_vterm_3/1
]).

%% Macros
-ifdef(WHENFAIL).
-undef(WHENFAIL).
-endif.
% eqWAlizer gets angry about `fun(() -> boolean())` not being a subtype of `fun(() -> proper:test())`
-define(WHENFAIL(Action, Prop), proper:whenfail(?DELAY(Action), dynamic_cast(?DELAY(Prop)))).
-define(VTERM_EQUALS(A, B), ?WHENFAIL(vterm_report_not_equal(A, B), A =:= B)).

%%%=============================================================================
%%% Helpers
%%%=============================================================================

vdist_derive_atoms(VDistNoAtoms) ->
    VTerm0 = vdist_dop:dop_to_control_message_vterm(VDistNoAtoms),
    {Atoms, VTerm1} = vterm_reference_atoms(VTerm0),
    VTerm2 = vterm_resolve_atoms(Atoms, VTerm1),
    VDistWithAtoms = vdist_dop:control_message_vterm_to_dop(VTerm2),
    {Atoms, VDistWithAtoms}.

vdist_test_vectors_derive_atoms([VDistNoAtoms | TestVectors]) ->
    {Atoms, VDistWithAtoms} = vdist_derive_atoms(VDistNoAtoms),
    [{VDistNoAtoms, {Atoms, VDistWithAtoms}} | vdist_test_vectors_derive_atoms(TestVectors)];
vdist_test_vectors_derive_atoms([]) ->
    [].

vterm_report_not_equal(A, B) ->
    io:format("Expected:~n~0tp~nActual:~n~0tp~n", [A, B]).

vterm_reference_atoms(VTerm0) ->
    {VTerm1, AtomMap} = vterm:xform(VTerm0, maps:new(), fun vterm_reference_atoms_xform/2),
    {VTerm2, undefined} = vterm:xform(VTerm1, undefined, fun vterm_repair_xform/2),
    Atoms = erlang:list_to_tuple([
        Atom
     || {_Index, Atom} <- lists:sort([{Index, Atom} || {Atom, Index} <- maps:to_list(AtomMap)])
    ]),
    {Atoms, VTerm2}.

vterm_resolve_atoms(Atoms, VTerm0) ->
    {VTerm1, Atoms} = vterm:xform(VTerm0, Atoms, fun vterm_resolve_atoms_xform/2),
    VTerm1.

vterm_test_vectors_resolve_atoms([{Atoms, VTerm} | TestVectors]) ->
    ExpectedVTerm = vterm_resolve_atoms(Atoms, VTerm),
    [{Atoms, VTerm, ExpectedVTerm} | vterm_test_vectors_resolve_atoms(TestVectors)];
vterm_test_vectors_resolve_atoms([]) ->
    [].

%%%=============================================================================
%%% Properties
%%%=============================================================================

prop_dist_ext_to_vdist_2(_Config) ->
    ?FORALL(
        {VDistNoAtoms, {Atoms, VDistWithAtoms}},
        gen_vdist_test_vector(),
        begin
            ExternalNoAtomsBinary = vterm_encode:external_vterm_to_binary(
                vdist_dop:dop_to_control_message_vterm(VDistNoAtoms), #{}
            ),
            ExternalWithAtomsBinary = vterm_encode:external_vterm_to_binary(
                vdist_dop:dop_to_control_message_vterm(VDistWithAtoms), #{allow_atom_cache_refs => true}
            ),
            conjunction([
                {vedf_without_atom_cache_refs,
                    ?VTERM_EQUALS(VDistNoAtoms, vedf:dist_ext_to_vdist({}, ExternalNoAtomsBinary))},
                {vedf_with_atom_cache_refs,
                    ?VTERM_EQUALS(
                        VDistWithAtoms, vedf:dist_ext_to_vdist(Atoms, ExternalWithAtomsBinary)
                    )},
                {vdist_without_atom_cache_refs,
                    ?VTERM_EQUALS(VDistNoAtoms, erldist_filter_nif:dist_ext_to_vdist({}, ExternalNoAtomsBinary))},
                {vdist_with_atom_cache_refs,
                    ?VTERM_EQUALS(
                        VDistWithAtoms, erldist_filter_nif:dist_ext_to_vdist(Atoms, ExternalWithAtomsBinary)
                    )}
            ])
        end
    ).

prop_dist_ext_to_vterm_2(_Config) ->
    ?FORALL(
        {VTerm, Atoms, VTermWithAtomCacheRefs, VTermResolved},
        gen_vterm_test_vector(),
        begin
            VTermExternalBinary = vterm_encode:external_vterm_to_binary(VTerm, #{}),
            VTermWithAtomCacheRefsExternalBinary = vterm_encode:external_vterm_to_binary(VTermWithAtomCacheRefs, #{
                allow_atom_cache_refs => true
            }),
            conjunction([
                {term_without_atom_cache_refs,
                    ?VTERM_EQUALS(
                        erts_debug:dist_ext_to_term({}, VTermExternalBinary),
                        vterm:simplify(erldist_filter_nif:dist_ext_to_vterm({}, VTermExternalBinary))
                    )},
                {term_with_atom_cache_refs,
                    ?VTERM_EQUALS(
                        erts_debug:dist_ext_to_term(Atoms, VTermWithAtomCacheRefsExternalBinary),
                        vterm:simplify(
                            erldist_filter_nif:dist_ext_to_vterm(Atoms, VTermWithAtomCacheRefsExternalBinary)
                        )
                    )},
                {vterm_without_atom_cache_refs,
                    ?VTERM_EQUALS(VTerm, erldist_filter_nif:dist_ext_to_vterm({}, VTermExternalBinary))},
                {vterm_with_atom_cache_refs,
                    ?VTERM_EQUALS(
                        VTermResolved, erldist_filter_nif:dist_ext_to_vterm(Atoms, VTermWithAtomCacheRefsExternalBinary)
                    )}
            ])
        end
    ).

prop_dist_ext_to_vterm_3(_Config) ->
    ?FORALL(
        {{VTerm, Atoms, VTermWithAtomCacheRefs, _VTermResolved}, Limit},
        {gen_vterm_test_vector(), integer(-1, inf)},
        begin
            VTermExternalBinary = vterm_encode:external_vterm_to_binary(VTerm, #{}),
            VTermWithAtomCacheRefsExternalBinary = vterm_encode:external_vterm_to_binary(VTermWithAtomCacheRefs, #{
                allow_atom_cache_refs => true
            }),
            LimitVTerm = vedf:dist_ext_to_vterm({}, VTermExternalBinary, Limit),
            LimitVTermResolved = vedf:dist_ext_to_vterm(Atoms, VTermWithAtomCacheRefsExternalBinary, Limit),
            conjunction([
                {vterm_without_atom_cache_refs,
                    ?VTERM_EQUALS(LimitVTerm, erldist_filter_nif:dist_ext_to_vterm({}, VTermExternalBinary, Limit))},
                {vterm_with_atom_cache_refs,
                    ?VTERM_EQUALS(
                        LimitVTermResolved,
                        erldist_filter_nif:dist_ext_to_vterm(Atoms, VTermWithAtomCacheRefsExternalBinary, Limit)
                    )}
            ])
        end
    ).

prop_dist_int_to_vdist_2(_Config) ->
    ?FORALL(
        {VDistNoAtoms, {Atoms, VDistWithAtoms}},
        gen_vdist_test_vector(),
        begin
            InternalNoAtomsBinary = vterm_encode:internal_vterm_to_binary(
                vdist_dop:dop_to_control_message_vterm(VDistNoAtoms), #{}
            ),
            InternalWithAtomsBinary = vterm_encode:internal_vterm_to_binary(
                vdist_dop:dop_to_control_message_vterm(VDistWithAtoms), #{allow_atom_cache_refs => true}
            ),
            conjunction([
                {vedf_without_atom_cache_refs,
                    ?VTERM_EQUALS(VDistNoAtoms, vedf:dist_int_to_vdist({}, InternalNoAtomsBinary))},
                {vedf_with_atom_cache_refs,
                    ?VTERM_EQUALS(
                        VDistWithAtoms, vedf:dist_int_to_vdist(Atoms, InternalWithAtomsBinary)
                    )},
                {vdist_without_atom_cache_refs,
                    ?VTERM_EQUALS(VDistNoAtoms, erldist_filter_nif:dist_int_to_vdist({}, InternalNoAtomsBinary))},
                {vdist_with_atom_cache_refs,
                    ?VTERM_EQUALS(
                        VDistWithAtoms, erldist_filter_nif:dist_int_to_vdist(Atoms, InternalWithAtomsBinary)
                    )}
            ])
        end
    ).

prop_dist_int_to_vterm_2(_Config) ->
    ?FORALL(
        {VTerm, Atoms, VTermWithAtomCacheRefs, VTermResolved},
        gen_vterm_test_vector(),
        begin
            VTermInternalBinary = vterm_encode:internal_vterm_to_binary(VTerm, #{}),
            VTermWithAtomCacheRefsInternalBinary = vterm_encode:internal_vterm_to_binary(VTermWithAtomCacheRefs, #{
                allow_atom_cache_refs => true
            }),
            conjunction([
                {vterm_without_atom_cache_refs,
                    ?VTERM_EQUALS(VTerm, erldist_filter_nif:dist_int_to_vterm({}, VTermInternalBinary))},
                {vterm_with_atom_cache_refs,
                    ?VTERM_EQUALS(
                        VTermResolved, erldist_filter_nif:dist_int_to_vterm(Atoms, VTermWithAtomCacheRefsInternalBinary)
                    )}
            ])
        end
    ).

prop_dist_int_to_vterm_3(_Config) ->
    ?FORALL(
        {{VTerm, Atoms, VTermWithAtomCacheRefs, _VTermResolved}, Limit},
        {gen_vterm_test_vector(), integer(-1, inf)},
        begin
            VTermInternalBinary = vterm_encode:internal_vterm_to_binary(VTerm, #{}),
            VTermWithAtomCacheRefsInternalBinary = vterm_encode:internal_vterm_to_binary(VTermWithAtomCacheRefs, #{
                allow_atom_cache_refs => true
            }),
            LimitVTerm = vedf:dist_int_to_vterm({}, VTermInternalBinary, Limit),
            LimitVTermResolved = vedf:dist_int_to_vterm(Atoms, VTermWithAtomCacheRefsInternalBinary, Limit),
            conjunction([
                {vterm_without_atom_cache_refs,
                    ?VTERM_EQUALS(LimitVTerm, erldist_filter_nif:dist_int_to_vterm({}, VTermInternalBinary, Limit))},
                {vterm_with_atom_cache_refs,
                    ?VTERM_EQUALS(
                        LimitVTermResolved,
                        erldist_filter_nif:dist_int_to_vterm(Atoms, VTermWithAtomCacheRefsInternalBinary, Limit)
                    )}
            ])
        end
    ).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

-spec dynamic_cast(term()) -> eqwalizer:dynamic().
dynamic_cast(X) -> X.

%% @private
gen_vdist_test_vector() ->
    ?LET(
        VDistNoAtoms,
        oneof([
            proper_vdist:vdist_any_dop_with_payload(),
            proper_vdist:vdist_any_dop_without_payload()
        ]),
        begin
            {Atoms, VDistWithAtoms} = vdist_derive_atoms(VDistNoAtoms),
            {VDistNoAtoms, {Atoms, VDistWithAtoms}}
        end
    ).

%% @private
gen_vterm_test_vector() ->
    ?LET(
        VTerm,
        proper_vterm:vterm(),
        begin
            {Atoms, VTermWithAtomCacheRefs} = vterm_reference_atoms(VTerm),
            VTermResolved = vterm_resolve_atoms(Atoms, VTermWithAtomCacheRefs),
            {VTerm, Atoms, VTermWithAtomCacheRefs, VTermResolved}
        end
    ).

%% @private
vterm_repair_xform(VTerm0 = #vterm_new_fun_ext{}, Acc) ->
    VTerm1 = vterm_new_fun_ext:repair_derived_size(VTerm0),
    {cont, VTerm1, Acc};
vterm_repair_xform(VTerm, Acc) ->
    {cont, VTerm, Acc}.

%% @private
vterm_reference_atoms_xform(VTerm, AtomMap0) when ?is_vterm_atom_t(VTerm) ->
    Atom = vterm:simplify(VTerm),
    case maps:find(Atom, AtomMap0) of
        {ok, Index} ->
            {cont, vterm_atom_cache_ref:new(Index), AtomMap0};
        error when map_size(AtomMap0) < 255 ->
            Index = map_size(AtomMap0),
            AtomMap1 = maps:put(Atom, Index, AtomMap0),
            {cont, vterm_atom_cache_ref:new(Index), AtomMap1};
        error ->
            {cont, VTerm, AtomMap0}
    end;
vterm_reference_atoms_xform(VTerm, AtomMap) ->
    {cont, VTerm, AtomMap}.

%% @private
vterm_resolve_atoms_xform(#vterm_atom_cache_ref{index = Index}, Atoms) ->
    Atom = element(Index + 1, Atoms),
    {cont, vterm_atom_cache_ref_resolved:new(Index, Atom), Atoms};
vterm_resolve_atoms_xform(VT, Atoms) ->
    {cont, VT, Atoms}.
