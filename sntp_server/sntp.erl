%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the sntp service.

-module(sntp).
-author('author <author@example.com>').
-export([start/0, stop/0]).

%%
%%
start() ->
    case application:start(sntp) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.

%%
%%
stop() ->
    application:stop(sntp).

