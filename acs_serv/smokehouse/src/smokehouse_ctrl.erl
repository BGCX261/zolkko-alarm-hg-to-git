%% @author Alex Anisimov aka lx <zolkko@gmail.com>
%% @copyright Alex Anisimov, 2011

%% @doc SmokeHouse controlling service

-module(smokehouse_ctrl).

-behaviour(gen_server).

%% global exports
-export([start/1, start_link/1]).

%% gen_server behavour
-export([init/1,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-define(UDP_OPTIONS, [binary, inet, {active, true}]).

-record(ctrl_svc, {socket, db}).

%% @doc starting standalon service process
start(Args) ->
    gen_server:start(?MODULE, Args, []).

%% @doc starting supervisored service process
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% @spec init([DbServiceModule, Port] = Args) -> {ok, Socket}
%% @doc Initialize udp controlling service by creating listening socket and
%%      database backend.
init([DbServiceModule, Port]) when is_number(Port) ->
    case db_service:start(DbServiceModule) of 
        failed -> {error, "Failed to load database service"};
        DbService ->
            error_logger:info_msg("Database backend ~p has been started for controlling service.", [DbServiceModule]),
            case gen_udp:open(Port, ?UDP_OPTIONS) of
                {ok, Socket} ->
                    error_logger:info_msg("Controlling service is started at port ~p.", [Port]),
                    {ok, #ctrl_svc{socket = Socket, db = DbService}};
                
                {error, Reason} ->
                    {stop, Reason}
            end
    end.

%% @doc handle async calls
handle_cast(_Cast, _State) ->
    {noreply, _State}.

%% @doc handle sync calls
handle_call(_State, _Caller, _Port) ->
    {noreply, _State}.

%% @spec handle_info(Udp, Socket) -> {noreply, Socket} | {stop, Reason, NewSocket}.
%% @doc Handle UDP datagrams.
handle_info({udp, _ClientSocket, ClientAddr, ClientPort, _RawData}, State) when is_record(State, ctrl_svc) ->
    error_logger:info_msg("UDP datagram received from udp://~p:~p on socket ~p.", [ClientAddr, ClientPort, State#ctrl_svc.socket]),
    {noreply, State}.

%% @spec terminate(normal | shutdown | {shutdown,term()} | term() = _Reason, State) -> ignored.
%% @doc Close udp socket and free up database backend.
terminate(_Reason, State) when is_record(State, ctrl_svc) ->
    error_logger:info_msg("Closing controlling service at socket ~p.", [State#ctrl_svc.socket]),
    gen_udp:close(State#ctrl_svc.socket),
    error_logger:info_msg("Stopping controlling service's database backend ~p.", [State#ctrl_svc.db]),
    db_interface:stop(State#ctrl_svc.db).

%% @doc Service code is changed
code_change(_OldVersion, Library, _Extra) ->
    {ok, Library}.

