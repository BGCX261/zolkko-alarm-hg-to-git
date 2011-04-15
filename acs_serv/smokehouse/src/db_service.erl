%% @author Alex Anisimov <zolkko@gmail.com>
%% @copyright 2011 Alex Anisimov.

%% @doc Database interface.

-module(db_service).

-export([behaviour_info/1]).
-export([start/1, stop/1, add_node/5, remove_node/2, list_nodes/1]).

%% @spec behaviour_info(callbacks) -> InterfaceDescriptor .
%% @doc Declare behaviour interface
behaviour_info(callbacks) ->
    [{start, 0},
     {stop, 0},
	 {list_nodes, 0},
	 {add_node, 4},
	 {remove_node, 1}];

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

%% @spec list_nodes(DbModule) -> [{Name:string(), Address:string()}] .
%% @doc Lists all smoke house nodes available in database.
list_nodes(DbModule) ->
	DbModule:list_nodes().

%% @spec add(DbModule, Name, Password, Address, Port) -> {ok, Node:record()} | {failed, Reason} .
%% @doc Add new smokehouse node.
add_node(DbModule, Name, Password, Address, Port) ->
	DbModule:add_node(Name, Password, Address, Port).

%% @spec remove_node(DbModule, NodeName) -> {ok, Node:node()} | {failed, Reason} .
%% @doc Remove smokehouse node form watch list.
remove_node(DbModule, NodeName) ->
	DbModule:remove_node(NodeName).

