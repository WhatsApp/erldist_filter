%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%-----------------------------------------------------------------------------
-module(erldist_filter_test).
-moduledoc """

""".
-moduledoc #{author => ["Andrew Bennett <potatosaladx@meta.com>"]}.
-moduledoc #{created => "2022-09-29", modified => "2025-08-18"}.
-moduledoc #{copyright => "Meta Platforms, Inc. and affiliates."}.
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").

%% Public API
-export([
    dynamic_cast/1
]).

%% Types
-type ct_config() :: [{Key :: atom(), Value :: term()}].
-type ct_groupname() :: atom().
-type ct_status() :: ok | skipped | failed.
-type ct_testname() :: atom().

-type ct_group_props() :: [
    parallel
    | sequence
    | shuffle
    | {shuffle, Seed :: {integer(), integer(), integer()}}
    | {ct_group_repeat_type(), ct_test_repeat()}
].
-type ct_group_props_ref() :: ct_group_props() | default.
-type ct_group_repeat_type() ::
    repeat
    | repeat_until_all_ok
    | repeat_until_all_fail
    | repeat_until_any_ok
    | repeat_until_any_fail.
-type ct_test_repeat() :: integer() | forever.
-type ct_subgroups_def() ::
    {ct_groupname(), ct_group_props_ref()}
    | {ct_groupname(), ct_group_props_ref(), ct_subgroups_def()}.
-type ct_group_ref() ::
    {group, ct_groupname()}
    | {group, ct_groupname(), ct_group_props_ref()}
    | {group, ct_groupname(), ct_group_props_ref(), ct_subgroups_def()}.
-type ct_testcase_repeat_prop() :: [
    {repeat, ct_test_repeat()}
    | {repeat_until_ok, ct_test_repeat()}
    | {repeat_until_fail, ct_test_repeat()}
].
-type ct_testcase_ref() :: {testcase, ct_testname(), ct_testcase_repeat_prop()}.
-type ct_group_def() ::
    {ct_groupname(), ct_group_props(), [
        ct_testname()
        | ct_group_def()
        | {group, ct_groupname()}
        | ct_testcase_ref()
    ]}.
-type ct_test_def() :: ct_testname() | ct_group_ref() | ct_testcase_ref().
-type ct_info_timetrap() ::
    timeout()
    | {seconds, integer()}
    | {minutes, integer()}
    | {hours, integer()}
    | {Mod :: atom(), Func :: atom(), Args :: list()}
    | fun().
-type ct_info_required_subkeys() :: atom() | [atom()].
-type ct_info_required() ::
    atom()
    | {atom(), ct_info_required_subkeys()}
    | {atom(), atom()}
    | {atom(), atom(), ct_info_required_subkeys()}.
-type ct_hooks() :: [
    module()
    | {module(), term()}
    | {module(), term(), integer()}
].
-type ct_info() ::
    {timetrap, ct_info_timetrap()}
    | {require, ct_info_required()}
    | {require, Name :: atom(), ct_info_required()}
    | {userdata, UserData :: term()}
    | {silent_connections, Conns :: [atom()]}
    | {stylesheet, CSSFile :: string()}
    | {ct_hooks, ct_hooks()}.

-type all() ::
    [TestDef :: ct_test_def()]
    | {skip, Reason :: term()}.
-type end_per_group() ::
    term()
    | {return_group_result, Status :: ct_status()}.
-type end_per_suite() ::
    term()
    | {save_config, SaveConfig :: ct_config()}.
-type end_per_testcase() ::
    term()
    | {fail, Reason :: term()}
    | {save_config, SaveConfig :: ct_config()}.
-type groups() :: [GroupDef :: ct_group_def()].
-type init_per_group() ::
    NewConfig ::
    ct_config()
    | {skip, Reason :: term()}.
-type init_per_suite() ::
    NewConfig ::
    ct_config()
    | {skip, Reason :: term()}
    | {skip_and_save, Reason :: term(), SaveConfig :: ct_config()}.
-type init_per_testcase() ::
    NewConfig ::
    ct_config()
    | {fail, Reason :: term()}
    | {skip, Reason :: term()}.
-type suite() ::
    [Info :: ct_info() | {doc, string()}].
-type testcase() ::
    term()
    | {skip, Reason :: term()}
    | {fail, Reason :: term()}
    | {comment, Comment :: string()}
    | {save_config, SaveConfig :: ct_config()}
    | {skip_and_save, Reason :: term(), SaveConfig :: ct_config()}.
-type testcase_info() ::
    [Info :: ct_info() | {doc, string()}].

-export_type([
    all/0,
    ct_config/0,
    ct_group_def/0,
    ct_groupname/0,
    ct_info/0,
    ct_status/0,
    ct_test_def/0,
    ct_testname/0,
    end_per_group/0,
    end_per_suite/0,
    end_per_testcase/0,
    groups/0,
    init_per_group/0,
    init_per_suite/0,
    init_per_testcase/0,
    suite/0,
    testcase/0,
    testcase_info/0
]).

%%%=============================================================================
%%% Public API functions
%%%=============================================================================

-spec dynamic_cast(term()) -> dynamic().
dynamic_cast(X) -> X.
