%%%-------------------------------------------------------------------
%% @doc iotserver public API
%% @end
%%%-------------------------------------------------------------------

-module(iotserver_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    iotserver_sup:start_link().

stop(_State) ->
    ok.