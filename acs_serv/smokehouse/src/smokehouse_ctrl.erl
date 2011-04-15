%% @author Alex Anisimov aka lx <zolkko@gmail.com>
%% @copyright Alex Anisimov, 2011

%% @doc SmokeHouse controlling service

-module(smokehouse_ctrl).

-behaviour(gen_server).

%% External API
-export([list_nodes/0, add_node/4, remove_node/1]).

%% gen_server behavour
-export([start/1, start_link/1]).

-export([init/1,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-define(UDP_OPTIONS, [binary, inet, {active, true}]).
-define(SERVER, ?MODULE).

-record(ctrl_svc, {socket, db}).

%%
%% External API implementation
%%

%% @spec list_nodes() -> [#node{}] | {failed, Reason}.
%% @doc List watched nodes.
list_nodes() ->
    gen_server:call(?SERVER, list_nodes).

%% @spec add_node(Name, Password, Address, Port) ->
%% @doc Add smokehouse node.
add_node(Name, Password, Address, Port) ->
    gen_server:call(?SERVER, {add_node, Name, Password, Address, Port}).

%% @spec remove_node(Name) -> {ok, Node:record} | {failed, Reason}.
%% @doc Remove smokehouse node.
remove_node(Name) ->
    gen_server:call(?SERVER, {remove_node, Name}).

%%
%% Behaviour implementation
%%

%% @doc starting standalon service process
start(Args) ->
    gen_server:start({local, ?SERVER}, ?MODULE, Args, []).

%% @doc starting supervisored service process
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

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
handle_call(Request, _Caller, #ctrl_svc{socket = _Socket, db = DbService} = State) ->
    case Request of
        list_nodes ->
            error_logger:info_msg("list_nodes"),
            {reply, list_nodes_impl(DbService), State};
        
        {add_node, {Name, Password, Address, Port}} ->
            error_logger:info_msg("add_node"),
            {reply, add_node_impl(DbService, Name, Password, Address, Port), State};
        
        {remove_node, NodeName} ->
            error_logger:info_msg("remove_node"),
            {reply, remove_node_impl(DbService, NodeName), State};
        
        [] ->
            {stop, "Unable to handle empty messages", State};
        
        _ ->
            {noreply, State}
    end.

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
    db_service:stop(State#ctrl_svc.db).

%% @doc Service code is changed
code_change(_OldVersion, Library, _Extra) ->
    {ok, Library}.

%%
%% Internal API implementation.
%%

%% @spec list_nodes_impl(DbService) -> [#node{}] | {failed, Reason}.
%% @doc List nodes registered in the system.
list_nodes_impl(DbService) ->
    db_service:list_nodes(DbService).

%% @spec add_node_impl(DbService, Name, Password, Address, Port) -> {ok, Node:node()} | {failed, Reason} .
%% @doc Add new smokehouse node.
add_node_impl(DbService, NodeName, Password, Address, Port) ->
    db_service:add_node(DbService, NodeName, Password, Address, Port).

%% @spec remove_node(DbService, NodeName) -> {ok, Node:node()} | {failed, Reason} .
%% @doc Remove smokehouse node from watch.
remove_node_impl(DbService, NodeName) ->
    db_service:remove_node(DbService, NodeName).

