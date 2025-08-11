%%% % @format
%%%-----------------------------------------------------------------------------
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%% Copyright (c) WhatsApp LLC
%%%
%%% This source code is licensed under the MIT license found in the
%%% LICENSE.md file in the root directory of this source tree.
%%%
%%% Created :  20 Sep 2022 by Andrew Bennett <potatosaladx@meta.com>
%%%-----------------------------------------------------------------------------
-module(erldist_filter).
-compile(warn_missing_spec_all).
-oncall("whatsapp_clr").
-author("Andrew Bennett <potatosaladx@meta.com>").

%% Public API
-export([
    handler_get/0,
    handler_set/1
]).

%% Internal API
-export([
    nif_dir/0,
    priv_dir/0
]).

%%%=============================================================================
%%% Public API functions
%%%=============================================================================

-spec handler_get() -> undefined | module().
handler_get() ->
    case persistent_term:get(erldist_filter_handler, undefined) of
        Handler when is_atom(Handler) ->
            Handler;
        _ ->
            undefined
    end.

-spec handler_set(Handler) -> Handler when Handler :: undefined | module().
handler_set(Handler) when is_atom(Handler) ->
    OldHandler = handler_get(),
    ok = persistent_term:put(erldist_filter_handler, Handler),
    OldHandler.

%%%=============================================================================
%%% Internal API functions
%%%=============================================================================

-spec nif_dir() -> file:filename_all().
nif_dir() ->
    PrivDir = priv_dir(),
    FbpkgDir = filename:join([PrivDir, "wa_erldist_filter_nif"]),
    case filelib:is_dir(FbpkgDir) of
        true ->
            FbpkgDir;
        false ->
            PrivDir
    end.

-spec priv_dir() -> file:filename_all().
priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    filename:join([filename:dirname(Filename), "../priv"]);
                _ ->
                    "../priv"
            end;
        Dir ->
            Dir
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
