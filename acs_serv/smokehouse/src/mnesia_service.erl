%% @author Alex Anisimov <zolkko@gmail.com>
%% @copyright 2011 Alex Anisimov.

%% @doc Mnesia DB backend (implementation of DB behaviour)

-module(mnesia_service).

-behaviour(db_service).

-include_lib("stdlib/include/qlc.hrl").
-include("smokehouse.hrl").

-export([start/0, stop/0]).

%% @spec start() -> ok | failed.
%% @doc Starting mnesia sample db backend
start() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    ensure_table(auth, [
        {type, set},
        {ram_copies, [node()]},
        {attributes, record_info(fields, auth)}
    ]),
    ensure_table(device, [
        {type, set},
        {disc_copies, [node()]},
        {attributes, record_info(fields, device)}
    ]),
    ensure_table(marker,
        fun() ->
            mnesia:create_table(marker, [
                {type, set},
                {disc_copies, [node()]},
                {attributes, record_info(fields, marker)}
            ]),
            mnesia:transaction(
                fun() ->
                    mnesia:write(#device{uid="test-uid", password=markserv_util:bin_to_hexstr(crypto:md5("pass"))}),
                    mnesia:write(#device{uid="secd-uid", password=markserv_util:bin_to_hexstr(crypto:md5("pass"))}),
                    
                    mnesia:write(#marker{id=1, device="test-uid",
                        latitude=10.0, longitude=10.0,
                        contact="Contact Info", image="http://image1.neostru.com/img1",
                        timestamp=markserv_util:timestamp()}),
                    
                    mnesia:write(#marker{id=2, device="test-uid",
                        latitude=10.1, longitude=10.0,
                        contact="Contact Info", image="http://image2.neostru.com/img2",
                        timestamp=markserv_util:timestamp()}),
                    
                    mnesia:write(#marker{id=3, device="test-uid",
                        latitude=10.0, longitude=10.1,
                        contact="Contact Info", image="http://image3.neostru.com/img3",
                        timestamp=markserv_util:timestamp()}),
                    
                    mnesia:write(#marker{id=4, device="secd-uid",
                        latitude=11.0, longitude=11.0,
                        contact=[], image=[],
                        timestamp=markserv_util:timestamp()}),
                    
                    mnesia:write(#marker{id=5, device="secd-uid",
                        latitude=11.1, longitude=11.1,
                        contact="Contact Info", image="http://image5.neostru.com/img5",
                        timestamp=markserv_util:timestamp()})
                end)
        end),
    case mnesia:wait_for_tables([auth, device, marker], 1000) of
        {timeout, _} ->
            failed;
        _ ->
            ok
    end.

%%
%% Stop database module
%%
stop() ->
    mnesia:stop().

%%
%% Select Password by given UID
%%
password_by_uid(Uid) ->
    first(qlc:q([X#device.password || X <- mnesia:table(device),
        X#device.uid == Uid])).

%%
%% Register new device using Uid, HexPassword, SessionId and Unix Timestamp
%%
register_device(Uid, HexPassword, SessionId, Timestamp) ->
    mnesia:transaction(fun() ->
        mnesia:write(#device{uid=Uid, password=HexPassword}),
        mnesia:write(#auth{uid=Uid, session_id=SessionId, last_seen=Timestamp})
    end).

%%
%%
renew_session(Uid, SessionId, Timestamp) ->
    mnesia:transaction(fun() ->
        mnesia:write(#auth{uid=Uid, session_id=SessionId, last_seen=Timestamp})
    end).

%%
%% Return not expired session_id for given UID
%%
not_expired_session_id(Uid, Timestamp) ->
    {_, SessionExpire} = application:get_env(markserv, session_expire),
    first(qlc:q([X#auth.session_id || X <- mnesia:table(auth),
        X#auth.uid == Uid,
        (Timestamp - X#auth.last_seen) > SessionExpire])).

%%
%% List Markers in region, than sort it by distance, date and owner-device
%%
list_markers(Region) when is_record(Region, region) ->
    Me = Region#region.center,
    Ts = markserv_util:timestamp(),
    
    Lat1 = Me#point.latitude - (Region#region.span)#delta.latitudeDelta,
    Lon1 = Me#point.longitude - (Region#region.span)#delta.longitudeDelta,
    
    Lat2 = Me#point.latitude + (Region#region.span)#delta.latitudeDelta,
    Lon2 = Me#point.longitude + (Region#region.span)#delta.longitudeDelta,

    select(qlc:q([X || X <- mnesia:table(marker)])).
    %%select(qlc:sort(qlc:q([X || X <- mnesia:table(marker),
    %%                                Lat1 < X#marker.latitude,
    %%                                Lon1 < X#marker.longitude,
    %%                                X#marker.latitude < Lat2,
    %%                                X#marker.longitude < Lon2]),
    %%        {order, fun(A, B) ->
    %%            error_logger:info_msg("Sorting ME:~p, TS: ~p", [Me, Ts]),
    %%            get_distance(Me#point.latitude, Me#point.longitude, Ts,
    %%                A#marker.latitude, A#marker.longitude, A#marker.timestamp) =<
    %%            get_distance(Me#point.latitude, Me#point.longitude, Ts,
    %%                B#point.latitude, B#point.longitude, B#marker.timestamp)
    %%        end})).

%%%
%% Adds new marker to the DB
%%      Returns {ok, NewMarkerId} on success and {failed, Reason} on failure.
%%
new_marker(DeviceId, Latitude, Longitude, Contact, ImageUrl) ->
    case mnesia:transaction(
        fun() ->
            LastId = new_marker_id(),
            mnesia:write(#marker{id = LastId, device = DeviceId, latitude = Latitude, longitude = Longitude,
                         contact = Contact, image = ImageUrl, timestamp = markserv_util:timestamp()}), LastId end) of
        
        {aborted, Reason} ->
            error_logger:info_msg("Failed to create new marker for device ~p. The reason is:~p.", [DeviceId, Reason]),
            {failed, Reason};
        
        {atomic, MarkerId} ->
            error_logger:info_msg("New marker ~p is created for device ~p.", [MarkerId, DeviceId]),
            {ok, MarkerId};
        
        _ ->
            error_logger:info_msg("Failed to create new marker for device ~p. Unknown error.", [DeviceId]),
            {failed, "Unknown error"}
    end.

%%
%% Returns last marker id
%%
new_marker_id() ->
    case select(qlc:sort(qlc:q([X || X <- mnesia:table(marker)]), {order, fun(A, B) -> A > B end})) of
        [] -> 1;
        Value -> Value + 1
    end.

%%
%% Deletes marker
%% Only owners could delete their markers
%%
%%      @DeviceID - device that wants to delete marker
%%      @MarkerId - marker identifier to delete
%%
delete_marker(DeviceId, MarkerId) ->
    failed.

%
%% Return distance between point(X1, Y2) and point(X2, Y2), Zi - is timestamp
%%
get_distance(X1, Y1, Z1, X2, Y2, Z2) ->
    math:sqrt(math:pow(distance_trunc(X1) - distance_trunc(X2), 2) +
              math:pow(distance_trunc(Y1) - distance_trunc(Y2), 2) +
              math:pow(Z1 - Z2, 2)).

distance_trunc(Value) ->
    trunc(Value * 1000) / 1000.

%%
%% Helper function
%%
select(Q) ->
    Fn = fun () -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(Fn),
    Val.

%%
%% 
%%
first(Q) ->
    case select(Q) of
        [] -> [];
        Result -> hd(Result)
    end.


%%
%% Create table if it does not exists.
%%
%% @Table
%% @Options
%%
ensure_table(Table, Options) when is_atom(Table) andalso (is_list(Options) orelse is_tuple(Options)) ->
    ensure_table(Table,
        fun () ->
            mnesia:create_table(Table, Options)
        end);

%%
%% DONE: mnesia:table_info('table_name', type)
%%
ensure_table(Table, DoFunc) when is_atom(Table) andalso is_function(DoFunc) ->
    Tables = mnesia:system_info(tables),
    case lists:member(Table, Tables) of
        true ->
            ok;
        false ->
            DoFunc(),
            ok
    end.


