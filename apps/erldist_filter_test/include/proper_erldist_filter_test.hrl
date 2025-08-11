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
%%% Created :  16 Nov 2023 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
%%% % @format
%% @oncall whatsapp_clr
-ifndef(PROPER_ERLDIST_FILTER_TEST_HRL).
-define(PROPER_ERLDIST_FILTER_TEST_HRL, 1).

-ifndef(PROPER_NO_TRANS).
-define(PROPER_NO_TRANS, true).
-endif.
-ifndef(PROPER_NO_IMPORT_PARSE).
-define(PROPER_NO_IMPORT_PARSE, true).
-endif.

-include_lib("proper/include/proper.hrl").

-define(DYNAMIC(X), erldist_filter_test:dynamic_cast(X)).

%%%-----------------------------------------------------------------------------
%%% Test generation macros
%%%-----------------------------------------------------------------------------

-ifdef(EXISTS).
-undef(EXISTS).
-endif.
-define(EXISTS(X, RawType, Prop), proper:exists(RawType, ?DYNAMIC(fun(X) -> Prop end), false)).

-ifdef(FORALL).
-undef(FORALL).
-endif.
-define(FORALL(X, RawType, Prop), proper:forall(RawType, ?DYNAMIC(fun(X) -> Prop end))).

-ifdef(FORALL_TARGETED).
-undef(FORALL_TARGETED).
-endif.
-define(FORALL_TARGETED(X, RawType, Prop), proper:targeted(RawType, ?DYNAMIC(fun(X) -> Prop end))).

-ifdef(IMPLIES).
-undef(IMPLIES).
-endif.
-define(IMPLIES(Pre, Prop), proper:implies(Pre, ?DYNAMIC(?DELAY(Prop)))).

-ifdef(NOT_EXISTS).
-undef(NOT_EXISTS).
-endif.
-define(NOT_EXISTS(X, RawType, Prop), proper:exists(RawType, ?DYNAMIC(fun(X) -> Prop end), true)).

-ifdef(SETUP).
-undef(SETUP).
-endif.
-define(SETUP(SetupFun, Prop), proper:setup(SetupFun, Prop)).

-ifdef(TIMEOUT).
-undef(TIMEOUT).
-endif.
-define(TIMEOUT(Limit, Prop), proper:timeout(Limit, ?DYNAMIC(?DELAY(Prop)))).

-ifdef(TRAPEXIT).
-undef(TRAPEXIT).
-endif.
-define(TRAPEXIT(Prop), proper:trapexit(?DYNAMIC(?DELAY(Prop)))).

-ifdef(WHENFAIL).
-undef(WHENFAIL).
-endif.
% eqWAlizer gets angry about `fun(() -> boolean())` not being a subtype of `fun(() -> proper:test())`
-define(WHENFAIL(Action, Prop), proper:whenfail(?DELAY(Action), ?DYNAMIC(?DELAY(Prop)))).

%%%-----------------------------------------------------------------------------
%%% Generator macros
%%%-----------------------------------------------------------------------------

-ifdef(LET).
-undef(LET).
-endif.
-define(LET(X, RawType, Gen), proper_types:bind(RawType, ?DYNAMIC(fun(X) -> Gen end), false)).

-ifdef(LETSHRINK).
-undef(LETSHRINK).
-endif.
-define(LETSHRINK(Xs, RawType, Gen), proper_types:bind(RawType, ?DYNAMIC(fun(Xs) -> Gen end), true)).

-ifdef(SHRINK).
-undef(SHRINK).
-endif.
-define(SHRINK(Gen, AltGens), proper_types:shrinkwith(?DYNAMIC(?DELAY(Gen)), ?DYNAMIC(?DELAY(AltGens)))).

-ifdef(SIZED).
-undef(SIZED).
-endif.
-define(SIZED(SizeArg, Gen), proper_types:sized(?DYNAMIC(fun(SizeArg) -> Gen end))).

-ifdef(SUCHTHAT).
-undef(SUCHTHAT).
-endif.
-define(SUCHTHAT(X, RawType, Condition), proper_types:add_constraint(RawType, ?DYNAMIC(fun(X) -> Condition end), true)).

-ifdef(SUCHTHATMAYBE).
-undef(SUCHTHATMAYBE).
-endif.
-define(SUCHTHATMAYBE(X, RawType, Condition),
    proper_types:add_constraint(RawType, ?DYNAMIC(fun(X) -> Condition end), false)
).

%%%-----------------------------------------------------------------------------
%%% Function imports
%%%-----------------------------------------------------------------------------

% -ifndef(PROPER_NO_IMPORTS).

% -import(proper_erldist_filter_test, [
%     complex/1,
%     complexity/0,
%     minsized/1,
%     mostly/2,
%     mostly_small_list/1,
%     mostly_small_size/0,
%     option/1,
%     option_unlikely/1,
%     with_complexity/1
% ]).

% -endif.

-endif.
