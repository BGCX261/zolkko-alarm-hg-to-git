%% @author Alex Anisimov <zolkko@gmail.com>
%% @copyright 2011 Alex Anisimov.

%% @doc Callbacks for the smokehouse application.

-module(smokehouse_app).
-author('Alex Anisimov <zolkko@gmail.com>').

-behaviour(application).
-export([start/2, stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for smokehouse.
start(_Type, _StartArgs) ->
    markserv_deps:ensure(),
    markserv_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for smokehouse.
stop(_State) ->
    ok.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
