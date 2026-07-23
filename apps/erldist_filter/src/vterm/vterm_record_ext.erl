%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(vterm_record_ext).
-compile(warn_missing_spec_all).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-behaviour(vterm_encode).

-include_lib("erldist_filter/include/erldist_filter.hrl").
-include_lib("erldist_filter/include/erldist_filter_erts_external.hrl").

%% API
-export([
    new/6,
    internal_vterm_to_binary/2
]).

%% Types
-type t() :: #vterm_record_ext{}.

-export_type([
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(NumFields, Exported, Module, Name, FieldNames, Values) -> T when
    NumFields :: vterm:u32(),
    Exported :: boolean(),
    Module :: vterm:atom_t(),
    Name :: vterm:atom_t(),
    FieldNames :: [vterm:atom_t()],
    Values :: [vterm:t()],
    T :: t().
new(NumFields, Exported, Module, Name, FieldNames, Values) when
    ?is_u32(NumFields) andalso
        is_boolean(Exported) andalso
        ?is_vterm_atom_t(Module) andalso
        ?is_vterm_atom_t(Name) andalso
        is_list(FieldNames) andalso
        is_list(Values) andalso
        NumFields =:= length(FieldNames) andalso
        NumFields =:= length(Values)
->
    #vterm_record_ext{
        num_fields = NumFields,
        exported = Exported,
        module = Module,
        name = Name,
        field_names = FieldNames,
        values = Values
    }.

-spec internal_vterm_to_binary(T, Opts) -> binary() when T :: t(), Opts :: term().
internal_vterm_to_binary(
    #vterm_record_ext{
        num_fields = NumFields,
        exported = Exported,
        module = Module0,
        name = Name0,
        field_names = FieldNames0,
        values = Values0
    },
    Opts
) when
    ?is_u32(NumFields) andalso NumFields =:= length(FieldNames0) andalso
        NumFields =:= length(Values0)
->
    Flags =
        case Exported of
            true -> 1;
            false -> 0
        end,
    Module = vterm_encode:internal_vterm_to_binary(Module0, Opts),
    Name = vterm_encode:internal_vterm_to_binary(Name0, Opts),
    FieldNames = vterm_encode:internal_vterm_elements_to_binary(FieldNames0, Opts),
    Values = vterm_encode:internal_vterm_elements_to_binary(Values0, Opts),
    <<?RECORD_EXT, NumFields:32, Flags:8, Module/bytes, Name/bytes, FieldNames/bytes,
        Values/bytes>>.
