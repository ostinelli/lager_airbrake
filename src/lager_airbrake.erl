-module(lager_airbrake).

%% API
-export([start/0, stop/0]).


%% ===================================================================
%% API
%% ===================================================================
-spec start() -> ok.
start() ->
    ok = ensure_started(crypto),
    ok = ensure_started(public_key),
    ok = ensure_started(ssl),
    ok = ensure_started(idna),
    ok = ensure_started(hackney),
    ok = ensure_started(lager_airbrake).

-spec stop() -> ok.
stop() ->
    ok = application:stop(lager_airbrake),
    ok = application:stop(hackney),
    ok = application:stop(idna),
    ok = application:stop(ssl),
    ok = application:stop(public_key),
    ok = application:stop(crypto).

%% ===================================================================
%% Private
%% ===================================================================
-spec ensure_started(App :: atom()) -> ok.
ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.

%% ===================================================================
%% Internal
%% ===================================================================
