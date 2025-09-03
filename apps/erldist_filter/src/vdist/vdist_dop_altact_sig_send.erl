%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(vdist_dop_altact_sig_send).
-moduledoc """
""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2025-08-14", modified => "2025-08-14"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

-include_lib("erldist_filter/include/erldist_filter.hrl").
-include_lib("erldist_filter/include/erldist_filter_erts_dist.hrl").

%% API
-export([
    new/3,
    new/4,
    flags_as_integer/1,
    flags_as_list/1,
    flags_as_vterm/1,
    has_payload/1,
    has_token/1,
    into_control_message_vterm/1,
    sequence_id/1
]).

%% Types
-type flag_name() :: prio | token | alias | name | exit.
-type flag_name_list() :: [flag_name()].
-type flags() :: erldist_filter_nif_types:flag() | flag_name_list() | vterm:fixed_integer_t().
-type t() :: #vdist_dop_altact_sig_send{}.

-export_type([
    flag_name/0,
    flag_name_list/0,
    flags/0,
    t/0
]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

-spec new(Flags, SenderPid, To) -> T when
    Flags :: vterm:fixed_integer_t(),
    SenderPid :: vterm:pid_t(),
    To :: vterm:pid_t() | vterm:reference_t() | vterm:atom_t(),
    T :: t().
new(Flags, SenderPid, To) when
    ?is_vterm_fixed_integer_t(Flags) andalso ?is_vterm_pid_t(SenderPid) andalso
        (?is_vterm_pid_t(To) orelse ?is_vterm_atom_t(To) orelse ?is_vterm_reference_t(To))
->
    #vdist_dop_altact_sig_send{flags = Flags, sender_pid = SenderPid, to = To, token = none}.

-spec new(Flags, SenderPid, To, Token) -> T when
    Flags :: vterm:fixed_integer_t(),
    SenderPid :: vterm:pid_t(),
    To :: vterm:pid_t() | vterm:reference_t() | vterm:atom_t(),
    Token :: vterm:t(),
    T :: t().
new(Flags, SenderPid, To, Token) when
    ?is_vterm_fixed_integer_t(Flags) andalso ?is_vterm_pid_t(SenderPid) andalso
        (?is_vterm_pid_t(To) orelse ?is_vterm_atom_t(To) orelse ?is_vterm_reference_t(To)) andalso ?is_vterm_t(Token)
->
    #vdist_dop_altact_sig_send{flags = Flags, sender_pid = SenderPid, to = To, token = {some, Token}}.

-spec flags_as_integer(Flags) -> FlagsInteger when
    Flags :: flags(), FlagsInteger :: non_neg_integer().
flags_as_integer(FlagsInteger) when is_integer(FlagsInteger) andalso FlagsInteger >= 0 ->
    FlagsInteger;
flags_as_integer(FlagsVTerm) when ?is_vterm_fixed_integer_t(FlagsVTerm) ->
    FlagsInteger = vterm:simplify(FlagsVTerm),
    FlagsInteger;
flags_as_integer([prio | FlagNameList]) ->
    flags_as_integer(FlagNameList) bor ?ERTS_DOP_ALTACT_SIG_FLG_PRIO;
flags_as_integer([token | FlagNameList]) ->
    flags_as_integer(FlagNameList) bor ?ERTS_DOP_ALTACT_SIG_FLG_TOKEN;
flags_as_integer([alias | FlagNameList]) ->
    flags_as_integer(FlagNameList) bor ?ERTS_DOP_ALTACT_SIG_FLG_ALIAS;
flags_as_integer([name | FlagNameList]) ->
    flags_as_integer(FlagNameList) bor ?ERTS_DOP_ALTACT_SIG_FLG_NAME;
flags_as_integer([exit | FlagNameList]) ->
    flags_as_integer(FlagNameList) bor ?ERTS_DOP_ALTACT_SIG_FLG_EXIT;
flags_as_integer([]) ->
    0.

-spec flags_as_list(Flags) -> FlagsList when
    Flags :: flags(), FlagsList :: flag_name_list().
flags_as_list(FlagsInteger) when is_integer(FlagsInteger) andalso FlagsInteger >= 0 ->
    FlagNameList1 = [],
    FlagNameList2 =
        case (FlagsInteger band ?ERTS_DOP_ALTACT_SIG_FLG_EXIT) =:= ?ERTS_DOP_ALTACT_SIG_FLG_EXIT of
            true -> [exit | FlagNameList1];
            false -> FlagNameList1
        end,
    FlagNameList3 =
        case (FlagsInteger band ?ERTS_DOP_ALTACT_SIG_FLG_NAME) =:= ?ERTS_DOP_ALTACT_SIG_FLG_NAME of
            true -> [name | FlagNameList2];
            false -> FlagNameList2
        end,
    FlagNameList4 =
        case (FlagsInteger band ?ERTS_DOP_ALTACT_SIG_FLG_ALIAS) =:= ?ERTS_DOP_ALTACT_SIG_FLG_ALIAS of
            true -> [alias | FlagNameList3];
            false -> FlagNameList3
        end,
    FlagNameList5 =
        case (FlagsInteger band ?ERTS_DOP_ALTACT_SIG_FLG_TOKEN) =:= ?ERTS_DOP_ALTACT_SIG_FLG_TOKEN of
            true -> [token | FlagNameList4];
            false -> FlagNameList4
        end,
    FlagNameList6 =
        case (FlagsInteger band ?ERTS_DOP_ALTACT_SIG_FLG_PRIO) =:= ?ERTS_DOP_ALTACT_SIG_FLG_PRIO of
            true -> [prio | FlagNameList5];
            false -> FlagNameList5
        end,
    FlagNameList6;
flags_as_list(Flags) ->
    flags_as_list(flags_as_integer(Flags)).

-spec flags_as_vterm(Flags) -> FlagsVTerm when
    Flags :: flags(), FlagsVTerm :: vterm:fixed_integer_t().
flags_as_vterm(FlagsVTerm) when ?is_vterm_fixed_integer_t(FlagsVTerm) ->
    FlagsVTerm;
flags_as_vterm(Flags) ->
    vterm:expand_fixed_integer(flags_as_integer(Flags)).

-spec has_payload(T) -> boolean() when T :: t().
has_payload(#vdist_dop_altact_sig_send{}) ->
    true.

-spec has_token(T) -> boolean() when T :: t().
has_token(#vdist_dop_altact_sig_send{token = none}) -> false;
has_token(#vdist_dop_altact_sig_send{token = {some, _}}) -> true.

-spec into_control_message_vterm(T) -> vterm:tuple_t() when T :: t().
into_control_message_vterm(#vdist_dop_altact_sig_send{flags = Flags, sender_pid = SenderPid, to = To, token = none}) ->
    vterm_small_tuple_ext:new(4, [vterm_small_integer_ext:new(?DOP_ALTACT_SIG_SEND), Flags, SenderPid, To]);
into_control_message_vterm(#vdist_dop_altact_sig_send{
    flags = Flags, sender_pid = SenderPid, to = To, token = {some, Token}
}) ->
    vterm_small_tuple_ext:new(5, [vterm_small_integer_ext:new(?DOP_ALTACT_SIG_SEND), Flags, SenderPid, To, Token]).

-spec sequence_id(T) -> vdist:sequence_id() when T :: t().
sequence_id(#vdist_dop_altact_sig_send{sender_pid = SenderPid}) when ?is_vterm_pid_t(SenderPid) ->
    vterm_pid:sequence_id(SenderPid).
