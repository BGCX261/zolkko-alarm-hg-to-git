%% @author Alex Anisimov <zolkko@gmail.com>
%% @copyright 2011 Alex Anisimov <zolkko@gmail.com>

%% @doc Supervisor for the smokehouse application.

-module(smokehouse_sup).
-author("Alex Anisimov <zolkko@gmail.com>").

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),
    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),
    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Ctrl = ctrl_specs(smokehouse_ctrl),
    Web = web_specs(smokehouse_web),
    Processes = [Ctrl, Web],
    Strategy = {one_for_one, 10, 10},
    {ok, {Strategy, lists:flatten(Processes)}}.

ctrl_specs(Mod) ->
    Port = case application:get_env(smokehouse, ctrl_port) of
        {ok, CtrlPort} -> CtrlPort;
        undefined -> 8181
    end,
    DbServiceModule = case application:get_env(smokehouse, db_service_module) of
        {ok, DbSvcModule} -> list_to_atom(DbSvcModule);
        undefined -> mnesia_service
    end,
    {Mod,
        {Mod, start_link, [[DbServiceModule, Port]]},
        permanent, 5000, worker, dynamic}.

web_specs(Mod) ->
    Port = case application:get_env(smokehouse, web_port) of
        {ok, WebPort} -> WebPort;
        undefined -> 8080
    end,
    WebConfig = [{ip, {0,0,0,0}},
                 {port, Port},
                 {docroot, smokehouse_deps:local_path(["priv", "www"])}],
    {Mod,
     {Mod, start, [WebConfig]},
     permanent, 5000, worker, dynamic}.

