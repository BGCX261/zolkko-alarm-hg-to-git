%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the sntp service.

-module(sntp_app).
-author('author <author@example.com>').
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    sntp_sup:start_link().

stop(_State) ->
    ok.

