-module(test).

-include_lib("eunit/include/eunit.hrl").

-define(output_len, 621).

simple_test() ->
  discovery_server:start(),
  Egor = mpeer:join("Egor"),
  Andrew = mpeer:join("Andrew"),
  Klim = mpeer:join("Klim"),
  Evgeniy = mpeer:join("Evgeniy"),
  client:send_message(Egor, "Privet"),
  timer:sleep(1100),
  client:send_message(Andrew, "Hello"),
  timer:sleep(1100),
  client:send_message(Klim, "I'm from London"),
  timer:sleep(1100),
  client:send_message(Evgeniy, "I'm from Russia"),
  timer:sleep(1100),
  ?assertEqual(?output_len, string:len(?capturedOutput)).
