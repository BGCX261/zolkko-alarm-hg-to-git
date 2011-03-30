%% @author Alex Anisimov <zolkko@gmail.com>
%% @copyright 2011 Alex Anisimov.

%% @doc SmokeHouse Web Interface

-module(smokehouse_web).
-author('Alex Anisimov <zolkko@gmail.com>').

-export([start/1, stop/0, loop/3]).

-include("smokehouse.hrl").

-define(RESPONSE_CONTENT_TYPE, "application/json").
-define(SERVER_NAME_KEY, "Server").
-define(SERVER_NAME, "ACS Smoke House 0.1").

%% External API
start(Options) ->
    {DbService, Options1} = get_option(db_service, Options),
    {DocRoot, Options2} = get_option(docroot, Options1),
    Loop = fun (Req) -> ?MODULE:loop(Req, DocRoot, DbService) end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options2]).

%%
get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
stop() ->
    mochiweb_http:stop(?MODULE).

%% 
loop(Req, DocRoot, DbService) ->    
	case smokehouse_route:route(Req) of
		{get, static, Url} ->
			Req:serve_file(Url, DocRoot);
		
		{get, Any} ->
			Req:respond({501, response_headers(), "TODO:"});
		
		Any ->
			Req:not_found()
	end.

%% Internal API
response_headers() ->
    [{?SERVER_NAME_KEY, ?SERVER_NAME}].

%%
%% Res = lists:map(fun (X) when is_record(X, marker) ->
%%   {struct, [
%%       {<<"id">>, X#marker.id},
%%       {<<"device">>, to_js(X#marker.device)},
%%       {<<"latitude">>, X#marker.latitude}
%%   ]}
%%   end, List),
%%   Req:ok({?RESPONSE_CONTENT_TYPE, [{?DEVICE_SESSION_KEY, SessionId}], mochijson2:encode(Res)})
%%
%% to_js(List) ->
%%    case List of
%%        [] -> null;
%%        _ -> list_to_binary(List)
%%    end.
%%

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
