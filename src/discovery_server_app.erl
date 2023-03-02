-module(discovery_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  io:fwrite("e"),
  discovery_server:start_link().


stop(_State) -> ok.
