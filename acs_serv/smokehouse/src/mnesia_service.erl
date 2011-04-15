%% @author Alex Anisimov <zolkko@gmail.com>
%% @copyright 2011 Alex Anisimov.

%% @doc Mnesia DB backend (implementation of DB behaviour)

-module(mnesia_service).

-behaviour(db_service).

-include_lib("stdlib/include/qlc.hrl").
-include("smokehouse.hrl").

-export([start/0, stop/0, list_nodes/0, add/2, remove/2]).

-export([select/1, first/1]).


%% @spec start() -> ok | failed.
%% @doc Starting mnesia sample db backend
start() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    ensure_table(node, 
        fun () ->
            mnesia:create_table(node, [
                {type, set},
                {disc_copies, [node()]},
                {attributes, record_info(fields, node)}
            ]),
            mnesia:transaction(
                fun () ->
                    mnesia:write(#node{name = <<"node_1">>, password = <<"pass">>, address = {127, 0, 0, 1}, port = 1234}),
                    mnesia:write(#node{name = <<"node_2">>, password = <<"pass">>, address = {127, 0, 0, 1}, port = 2345}),
                    mnesia:write(#node{name = <<"node_3">>, password = <<"pass">>, address = {127, 0, 0, 1}, port = 3456})
                end)
        end),
	%ensure_table(node, [
	%	{type, set},
	%	{ram_copies, [node()]},
	%	{attributes, record_info(fields, node)}
	%]),
    case mnesia:wait_for_tables([node], 1000) of
        {timeout, _} -> failed;
        _ -> ok
    end.

%% @spec  list_nodes() -> [X#node] | {failed, Reason} .
%% @doc Select all nodes from node table
list_nodes() ->
	case mnesia:transaction(fun () -> select(qlc:q([X || X <- mnesia:table(node)])) end) of
		{atomic, Result} -> Result;
		{aborted, Reason} -> {failed, Reason}
	end.

%% @spec add (Name, {Address, Port}) -> {failed, Reason} | Result .
%% @doc Adds record to database
add (Name, {Address, Port}) ->
	case mnesia:transaction(fun () ->
		case first(qlc:q([X || X <- mnesia:table(node)])) of
			[] -> mnesia:write(#node{name=Name, address=Address, port=Port});
			_  -> {failed, "Already exists"}
		end
	end) of
		{aborted, Reason} -> {failed, Reason};
		{atomic, {failed, Reason}} -> {failed, Reason};
		{atomic, Result} -> Result
	end.

%% @spec remove(Name, {Address, Port}} -> Result | {failed, Reason} .
%% @doc Removes record from database.
remove(_, {_, _}) ->
	%case mnesia:transaction(fun () -> mnesia:delete({node, #{} }) end) of
	%		{aborted, Reason} -> {failed, Reason};
	%	{atomic, Result} -> Result
	%end.
	failed.

%% @spec stop () -> ok | failed .
%% @doc Stop database module
stop() ->
    mnesia:stop().

%%
%% @doc Helper function
select(Q) ->
    Fn = fun () -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(Fn),
    Val.

%% 
%%
first(Q) ->
    case select(Q) of
        [] -> [];
        Result -> hd(Result)
    end.


%%
%% @doc Create table if it does not exists.
ensure_table(Table, Options) when is_atom(Table) andalso (is_list(Options) orelse is_tuple(Options)) ->
    ensure_table(Table,
        fun () ->
            mnesia:create_table(Table, Options)
        end);

%%
%%
ensure_table(Table, DoFunc) when is_atom(Table) andalso is_function(DoFunc) ->
    Tables = mnesia:system_info(tables),
    case lists:member(Table, Tables) of
        true ->
            ok;
        false ->
            DoFunc(),
            ok
    end.
