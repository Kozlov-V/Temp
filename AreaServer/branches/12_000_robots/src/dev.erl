-module(dev).

-include("obj.hrl").

-export([
    create_robots/1
    ]).


create_robots(Nr) ->
    create_robots(Nr, []).

create_robots(0, Acc) ->
    Acc;

create_robots(Nr, Acc) ->
    {ok, P} = obj_sup:start(robot),
    create_robots(Nr - 1, [P | Acc]).

