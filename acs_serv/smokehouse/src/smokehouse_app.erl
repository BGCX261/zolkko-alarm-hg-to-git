%% @author Mochi Media <dev@mochimedia.com>
%% @copyright smokehouse Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the smokehouse application.

-module(smokehouse_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for smokehouse.
start(_Type, _StartArgs) ->
    smokehouse_deps:ensure(),
    smokehouse_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for smokehouse.
stop(_State) ->
    ok.
