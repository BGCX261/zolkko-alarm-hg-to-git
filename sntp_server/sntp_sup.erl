%%
%% SNTP Server supervisor
%%

-module(sntp_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(FileName) ->
    SupSpec = {one_for_one, 100, 1000},
    ChildSpec = {sntp, {sntp_serv, start_link, []}, permanent, 2000, worker, [sntp_serv]},
    {ok, {SupSpec, [ChildSpec]}}.

