-module(test).

-export([main/0]).

main() ->
  discovery_server:start_link(),
  Egor = mpeer:join("Egor"),
  mpeer:join("Klim"),
  Max = mpeer:join("Max"),
  mpeer:join("Andrew"),
  mpeer:join("Misha"),
  mpeer:join("Anya"),
  mpeer:join("User"),
  John = mpeer:join("John"),
  mpeer:join("Jake"),
  Danila = mpeer:join("Danila"),
  client:send_message(Egor, "Hello"),
  timer:sleep(1000),
  client:send_message(Max, "Privet"),
  timer:sleep(1000),
  client:send_message(John, "Today is a good day"),
  timer:sleep(1000),
  client:send_message(Danila, "I'm from Alaska"),
  timer:sleep(1000).
