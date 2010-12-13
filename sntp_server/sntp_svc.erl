%% SNTP Server

-module(sntp_serv).

-behaviour(gen_server).

-export([start_link/0, stop/0]).

start_link() ->
    {ok, ServerSocket} = gen_udp:open(123, [binary]),
    ServerSocket.

stop(SocketServer) ->
    gen_udp:close(SocketServer).

