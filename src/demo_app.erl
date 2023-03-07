-module(demo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Response = discovery_server:start(),
  Egor = mpeer:join("Egor"),
  Andrew = mpeer:join("Andrew"),
  Klim = mpeer:join("Klim"),
  Evgeniy = mpeer:join("Evgeniy"),
  client:send_message(Egor, "Privet"),
  timer:sleep(2000),
  client:send_message(Andrew, "Hello"),
  timer:sleep(2000),
  client:send_message(Klim, "I'm from London"),
  timer:sleep(2000),
  client:send_message(Evgeniy, "I'm from Russia"),
  Response.

stop(_State) -> ok.
