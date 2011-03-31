%% @author Alex Anisimov <zolkko@gmail.com>
%% @copyright 2011 Alex Anisimov.

%% @doc Database interface.

-module(db_service).

-export([behaviour_info/1]).
-export([start/1, stop/1]).

%% @spec behaviour_info(callbacks) -> InterfaceDescriptor .
%% @doc Declare behaviour interface
behaviour_info(callbacks) ->
    [{start, 0},
     {stop, 0}];

behaviour_info(_) ->
    undefined.

%% @spec new (DbModule) -> {failed, Reason} | {ok, DbModuleHandle}.
%% @doc Create specified database interface
start (DbModule) ->
    case DbModule:start() of
        ok -> DbModule;
        _ -> failed
    end.

%% @doc Stops database interface module
stop (DbModule) ->
    DbModule:stop().
