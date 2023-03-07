-module(test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->

  Response = discovery_server:start(),
  io:fwrite("~w", [node()]),
  Egor = mpeer:join("Egor"),
  Andrew = mpeer:join("Andrew"),
  Klim = mpeer:join("Klim"),
  Evgeniy = mpeer:join("Evgeniy"),
  client:send_message(Egor, "Privet"),
  timer:sleep(1000),
  client:send_message(Andrew, "Hello"),
  timer:sleep(1000),
  client:send_message(Klim, "I'm from London"),
  timer:sleep(1000),
  client:send_message(Evgeniy, "I'm from Russia"),

  Response.
