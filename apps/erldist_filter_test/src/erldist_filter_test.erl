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
-type all() ::
    [TestDef :: ct_suite:ct_test_def()]
    | {skip, Reason :: term()}.
-type end_per_group() ::
    term()
    | {return_group_result, Status :: ct_suite:ct_status()}.
-type end_per_suite() ::
    term()
    | {save_config, SaveConfig :: ct_suite:ct_config()}.
-type end_per_testcase() ::
    term()
    | {fail, Reason :: term()}
    | {save_config, SaveConfig :: ct_suite:ct_config()}.
-type groups() :: [GroupDef :: ct_suite:ct_group_def()].
-type init_per_group() ::
    NewConfig ::
    ct_suite:ct_config()
    | {skip, Reason :: term()}.
-type init_per_suite() ::
    NewConfig ::
    ct_suite:ct_config()
    | {skip, Reason :: term()}
    | {skip_and_save, Reason :: term(), SaveConfig :: ct_suite:ct_config()}.
-type init_per_testcase() ::
    NewConfig ::
    ct_suite:ct_config()
    | {fail, Reason :: term()}
    | {skip, Reason :: term()}.
-type suite() ::
    [Info :: ct_suite:ct_info() | {doc, string()}].
-type testcase() ::
    term()
    | {skip, Reason :: term()}
    | {fail, Reason :: term()}
    | {comment, Comment :: string()}
    | {save_config, SaveConfig :: ct_suite:ct_config()}
    | {skip_and_save, Reason :: term(), SaveConfig :: ct_suite:ct_config()}.
-type testcase_info() ::
    [Info :: ct_suite:ct_info() | {doc, string()}].

-export_type([
    all/0,
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
