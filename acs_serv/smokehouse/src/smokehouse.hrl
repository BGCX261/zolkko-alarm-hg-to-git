-record(auth, {
    uid,
    session_id,
    last_seen
}).

%% Authenticated device
-record(device, {
    uid,
    password
}).

%% Placed markers
-record(marker, {
    id,
    device,
    latitude,
    longitude,
    contact,
    image,
    image_mime_type,
    timestamp
}).

%% Point definition
-record(point, {
    latitude = -90.0,
    longitude = -180.0
}).

%% Delta definition
-record(delta, {
    latitudeDelta = 1.0,
    longitudeDelta = 1.0
}).

%% Region definition
-record(region, {
    center,
    span,
    location
}).

