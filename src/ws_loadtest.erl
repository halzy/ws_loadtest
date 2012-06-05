-module(ws_loadtest).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ws_loadtest_sup:start_link().

stop(_State) ->
    ok.

start() ->
    application:start(ws_loadtest).
