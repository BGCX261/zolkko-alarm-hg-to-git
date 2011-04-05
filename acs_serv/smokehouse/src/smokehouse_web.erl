%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for smokehouse.

-module(smokehouse_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/3]).

-include("smokehouse.hrl").

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
                response_status(Req);
			
			{get, "psy_table_version"} ->
				response_psytable_version(Req);
			
			{get, "sensor"} ->
				response_sensor_data(Req);
			
			{get, "nodes"} ->
				response_nodes(Req, DbService);
			
            {get, Url} ->
                Req:serve_file(Url, DocRoot);
            
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

%% @spec response_nodes(Req, DbService) -> Response .
%% @doc Returns list of smoke house nodes.
response_nodes(Req, DbService) ->
	case db_service:list(DbService) of
		{failed, Reason} -> Req:respond({500, [{"Content-Type", "text/json"} | response_headers()], mochijson2:encode({struct, [{<<"error">>, Reason}]})});
		List ->
			JsonData = {array,
							[{struct,
									[{<<"name">>, X#node.name},
									 {<<"address">>, X#node.address},
									 {<<"port">>, X#node.port}]}
								|| X <- List]},
			Req:ok({"text/json", response_headers(), mochijson2:encode(JsonData)})
	end.

%% @spec response_headers() -> [ResponseHeaders::tuple()] .
response_headers() ->
    [{"Server", "ACS Smoke House v0.0.1"}].

%% @spec return_psy_table_version(Request) -> Respose .
%% @doc Returns	 psy table version from device
response_psytable_version(Req) ->
	Req:ok({"text/json", response_headers(), mochijson2:encode({struct, [{<<"version">>, <<"qwe7613hg1236teaw">>}]})}).

%% @spec return_status(Request) -> Response .
%% @doc Returns JSONed device status.
response_status(Req) ->
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

%% @spec return_sensor() -> Response .
%% @doc Returns data for each sensor
response_sensor_data(Req) ->
	{_, Minute, Second} = time(),
	% External sensor
	Sensor1 = {struct, [{<<"id">>, 1}, {<<"value">>, (random:uniform(80) + random:uniform(100) / 10) + 20}, {<<"time">>,  (Minute * 60 + Second)}]},
	% Hot internal sensor
	Sensor2 = {struct, [{<<"id">>, 2}, {<<"value">>, (random:uniform(80) + random:uniform(100) / 10) + 20}, {<<"time">>,  (Minute * 60 + Second)}]},
	% Dry internal sensor
	Sensor3 = {struct, [{<<"id">>, 3}, {<<"value">>, (random:uniform(80) + random:uniform(100) / 10) + 20}, {<<"time">>,  (Minute * 60 + Second)}]},
	% humidity
	Sensor4 = {struct, [{<<"id">>, 4}, {<<"value">>, random:uniform(75) + random:uniform(100) / 10}, {<<"time">>,  (Minute * 60 + Second)}]},
	Data = {array, [Sensor1, Sensor2, Sensor3, Sensor4]},
	Req:ok({"text/plain", response_headers(), mochijson2:encode(Data)}).

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
