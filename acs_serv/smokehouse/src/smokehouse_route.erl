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
    "/" ++ Path = Req:get(path),
    Post = Req:parse_post(),
	case {Req:get(method), Path} of
		{'GET', "status"} -> {get, "status"};
		{'GET', "psy_table_version"} -> {get, "psy_table_version"};
		{'GET', "sensor"} -> {get, "sensor"};
		{'GET', "nodes"} -> {get, "nodes"};
        {'GET', _} -> {get, Path};
        {'POST', "sensor_add"} ->
            case lists:keyfind("name", 1, Post) of
                false -> {error, not_found};
                {_, Name} ->
                    case lists:keyfind("address", 1, Post) of
                        false -> {error, not_found};
                        {_, Url} -> {post, "sensor_add", Name, Url}
                    end
            end;
        {'POST', "update_time"} ->
            case lists:keyfind("time", 1, Post) of
                {"time", NewValue} -> {post, "update_time", NewValue};
                _ -> {error, not_found}
            end;
		_ ->
			{error, not_found}
	end.
