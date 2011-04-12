-module(smokehouse_ctrl).

-behaviour(gen_server).

init([]) ->
    {ok, Socket} = gen_udp:open(111, [binary, inet]),
    {ok, 0}.

handle_call(Request, From, State) ->
    {noreplay, State}.

handle_cast(_Msg, State) ->
    {noreplay, State}.

handle_info({udp, Socket, ClientIp, ClientPort, Packet}, State) ->
    ok = gen_udp:send(Socket, ClientIp, ClientPort, Port)
    {noreplay, State};

handle_info(_Info, State) ->
    {noreplay, State}.

terminate(_Reason, N) ->
    ok.

code_change(_OldVsn, N, _Extra) ->
    {ok, N}.

