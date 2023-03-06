-module(demo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  test:simple_test().

stop(_State) -> ok.
