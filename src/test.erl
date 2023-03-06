-module(test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->

  Response = net_kernel:start(['discovery@127.0.0.1', longnames]),
  net_kernel:start(['client@127.0.0.1', longnames]),
  net_kernel:start(['client2', shortnames]),

  io:format("~w", [node()]),

  spawn('discovery@127.0.0.1', fun() ->
    discovery_server:start()
    end),

  spawn('client@127.0.0.1', fun() ->
    mpeer:join("e")
    end),

  spawn('client2', fun() ->
    mpeer:join("a")
    end),

  Response.