-module(test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  Response = net_kernel:start(['discovery@127.0.0.1', longnames]),

  spawn('discovery@127.0.0.1', fun() ->
      discovery_server:start(),
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
      client:send_message(Evgeniy, "I'm from Russia")
    end),

  Response.
