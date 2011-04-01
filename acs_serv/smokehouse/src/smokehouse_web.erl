%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for smokehouse.

-module(smokehouse_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/3]).

-define(SERVER_HEAD, "Server").
-define(SERVER_NAME, "ACS Smoke House Server v0.0.1").

%% External API

start(Options) ->
    DbService = db_service:start(mnesia_service),
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) -> ?MODULE:loop(Req, DocRoot, DbService) end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    db_service:stop(mnesia_service),
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot, DbService) ->
    try
        case smokehouse_route:route(Req) of
            {get, "status"} ->
                return_status(Req);
            
            {get, Url} ->
                Req:serve_file(Url, DocRoot);
            
            {post, "update_time", NewTime} ->
                update_time(Req, NewTime);
            
            _ ->
                Req:respond({500, [{"Content-Type", "text/plain"} | response_headers()], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Req:get(path)},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            Req:respond({500, [{?SERVER_HEAD, ?SERVER_NAME}, {"Content-Type", "text/plain"}], "request failed"})
    end.

%% Internal API
get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%% @spec uptime() -> Response .
%% @doc TODO: update time on linked device
update_time(Req, NewTime) ->
    Req:respond({500,
        [{"Content-Type", "text/plain"} | response_headers()],
        "Failed to update device time."}).

%% @spec response_headers() -> [ResponseHeaders::tuple()] .
response_headers() ->
    [{"Server", "ACS Smoke House v0.0.1"}].

%% @spec return_status(Request) -> Response .
%% @doc Returns JSONed device status.
return_status(Req) ->
    A1 = {struct,
        [{<<"finished">>, true},
        {<<"name">>, <<"Сушка">>},
        {<<"duration">>, <<"20">>},
        {<<"start_stamp">>, <<"236123761">>},
        {<<"temperature">>, {struct, [{<<"value">>, <<"95">>}, {<<"dynamic">>, false}]}},
        {<<"smog">>, false}]},
    A2 = {struct,
        [{<<"finished">>, true},
        {<<"name">>, <<"Прогонка">>},
        {<<"duration">>, <<"10">>},
        {<<"start_stamp">>, <<"4131231232">>},
        {<<"temperature">>, {struct, [{<<"value">>, <<"40">>}, {<<"dynamic">>, false}]}},
        {<<"smog">>, true}]},
    A3 = {struct,
        [{<<"finished">>, false},
        {<<"name">>, <<"Остывание">>},
        {<<"duration">>, <<"10">>},
        {<<"start_stamp">>, <<"432423423">>},
        {<<"temperature">>, {struct, [{<<"value">>, <<"30">>}, {<<"dynamic">>, true}]}},
        {<<"smog">>, false}]},
    A4 = {struct,
        [{<<"finished">>, false},
        {<<"name">>, <<"Повторное нагревание">>},
        {<<"duration">>, <<"10">>},
        {<<"temperature">>, {struct, [{<<"value">>, <<"65">>}, {<<"dynamic">>, true}]}},
        {<<"smog">>, false}]},
    StatusData = mochijson2:encode({array, [A1, A2, A3, A4]}),
    Req:ok({"text/json", response_headers(), StatusData}).

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
