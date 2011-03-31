%% @author Alex Anisimov <zolkko@gmail.com>
%% @copyright 2011 Alex Anisimov.

%% @doc Mnesia DB backend (implementation of DB behaviour)

-module(mnesia_service).

-behaviour(db_service).

-include_lib("stdlib/include/qlc.hrl").
-include("smokehouse.hrl").

-export([start/0, stop/0]).

-export([select/1, first/1]).


%% @spec start() -> ok | failed.
%% @doc Starting mnesia sample db backend
start() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    ensure_table(auth, [
        {type, set},
        {ram_copies, [node()]},
        {attributes, record_info(fields, auth)}
    ]),
    ensure_table(device, [
        {type, set},
        {disc_copies, [node()]},
        {attributes, record_info(fields, device)}
    ]),
    case mnesia:wait_for_tables([auth, device], 1000) of
        {timeout, _} ->
            failed;
        _ ->
            ok
    end.

%%
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

