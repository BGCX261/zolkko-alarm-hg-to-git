%% @author Alex Anisimov aka lx <zolkko@gmail.com>
%% @copyright 2011 Alex Anisimov.

%% @doc Smoke House application

-module(smokehouse).
-author('Alex Anisimov aka lx <zolkko@gmail.com>').
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
    Res = application:stop(smokehouse),
    %% TODO: ensure database backend stopped
    application:stop(crypto),
    Res.
