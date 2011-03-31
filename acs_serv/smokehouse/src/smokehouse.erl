%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc smokehouse.

-module(smokehouse).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the smokehouse server.
start() ->
    smokehouse_deps:ensure(),
    ensure_started(crypto),
    application:start(smokehouse).


%% @spec stop() -> ok
%% @doc Stop the smokehouse server.
stop() ->
    application:stop(smokehouse).
