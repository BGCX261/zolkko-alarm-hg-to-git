%% @author Alex Anisimov <zolkko@gmail.com>
%% @copyright 2011 Alex Anisimov.

%% @doc SmokeHouse service utils

-module(utils).

-export([to_int/1]).


%% @spec to_int(Value) -> false | Int.
%% @doc Convert string to integer. If converstion  failed, than returns false atom.
to_int([]) ->
    false;

to_int(Str) ->
    case re:run(Str, "^[0-9]+$") of
        nomatch -> false;
        _ ->
            case string:to_integer(Str) of
                {Int, _} -> Int;
                _ -> false
            end
    end.
