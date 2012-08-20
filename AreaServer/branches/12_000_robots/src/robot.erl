-module(robot).

-include("obj.hrl").
-include("vec.hrl").

-define(NEXT_TICK, random:uniform(5000) + 5000.

-import(obj, [call_self/2, call_self/3]).

-export([
    init/1,
    post_init/1,
    change_dir/2
    ]).

init(State) ->
    NewState = State#obj{parents=[movable, obj]},
    X = util:rand_int(10000),
    Z = util:rand_int(10000),
    {ok, _Reply, NewState2} = call_self(set_pos, 
        [#vec{x=X, y=0, z=Z}], NewState),
    {ok, _Reply, NewState3} = call_self(set_speed, [1], NewState2),
    {ok, _Reply, NewState4} = call_self(set_property, 
        [mesh, "robot.mesh"], NewState3),
    Vec = #vec{x=1},
    {ok, _Reply, NewState5} = call_self(set_dir, [Vec], NewState4),
    {ok, _Reply, NewState6} = call_self(do_anim, ["Walk"], NewState5),
    movable:init(NewState6).

post_init(State) ->
    callout(?NEXT_TICK),
    {ok, State}.

change_dir(_From, State) ->
    Time = ?NEXT_TICK,
    %X = random:uniform(),
    %Z = random:uniform(),
    {ok, Vec, _} = call_self(get_dir, State),
    %error_logger:info_report([{vec, Vec}]),
    %X = util:rand_float(),
    %Z = util:rand_float(),
    %NewVec = #vec{x=X, z=Z},
    NewVec = #vec{x=-Vec#vec.x},
    {ok, _Reply, NewState} = obj:call_self(set_dir, [NewVec], State),
    %timer:apply_after(Time, obj, async_call, [self(), change_dir]),
    callout(Time),
    {noreply, NewState}.


callout(Time) ->
    erlang:send_after(Time, self(), {execute, {from, self()}, 
        {call, change_dir}, {args, []}}).
