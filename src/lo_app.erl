-module(lo_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  seed(),
  lo_sup:start_link().

stop(_State) ->
  ok.

seed() ->
  {A, B, C} = now(),
  random:seed(A, B, C).
