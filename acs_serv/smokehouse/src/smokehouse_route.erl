%% @author Alex Anisimov <zolkko@gmail.com>
%% @copyright 2011 Alex Anisimov.

%% @doc Route request to specific handler.

-module(smokehouse_route).

-export([route/1]).

%% @spec route(Request) -> {get, ....} | {post, ...} | {get, static, Url} | {error, not_found}.
%% @doc Route request to specific handler
route([]) ->
	{error, internal_error};

route(Req) ->
	"/" ++ Url = Req:get(path),
	case Req:get(method) of
		"GET" ->
			case Url of
				"index/" ++ Id ->
					case utils:to_int(Id) of
						false -> {error, not_found};
						Int -> {get, index, Int}
					end;
				
				Id when length(Id) > 0 ->
					case utils:to_int(Id) of
						false -> {error, not_found};
						Int -> {get, index, Int}
					end;
				
				_ ->
					{get, static, Url}
			end;
		
		"POST" ->
			{error, not_found};
		
		"PUT" ->
			{error, not_found};
		
		"DELETE" ->
			{error, not_found};
		
		_ ->
			{error, not_found}
	end.
