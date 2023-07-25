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
%%% Created :  16 Dec 2022 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
-module(erldist_filter_test_dummy_prop).
-author("potatosaladx@meta.com").
-oncall("whatsapp_clr").

-include_lib("proper/include/proper.hrl").

%% Properties
-export([
    prop_noop/1
]).

%%%=============================================================================
%%% Properties
%%%=============================================================================

prop_noop(_Config) ->
    ?FORALL(
        Integer,
        integer(),
        begin
            Integer =:= Integer
        end
    ).
