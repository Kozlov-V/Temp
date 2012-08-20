%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodihn.se>
%% @copyright Chrisitan Flodihn
%% @doc
%% This module implements a object with the possiblity to have a position
%% in the world and functions to move.
%% @end
%%----------------------------------------------------------------------
-module(movable).

%% @headerfile "obj.hrl"
-include("obj.hrl").

%% @headerfile "vec.hrl"
-include("vec.hrl").

-import(obj, [
    call_self/2,
    call_self/3,
    async_call/3
    ]).

% The checkpoint value decides how often check for new quad is made.
-define(CHECKPOINT, 10).

-export([
    init/1,
    post_init/1,
    query_entity/2,
    update_pos/2,
    set_dir/3,
    get_dir/2,
    get_pos/2,
    set_pos/3,
    set_speed/3,
    get_speed/2
    ]).

%%----------------------------------------------------------------------
%% spec init(State) -> ok
%% where 
%%      State = obj()
%% @doc
%% Initiates the object for moving.
%% @end
%%----------------------------------------------------------------------
init(State) ->
    {ok, NewState} = obj:init(State),
    {ok, noreply, NewState2} = call_self(quadtree_assign, NewState),
    {ok, NewState2}.

post_init(State) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% spec set_dir(From, Vec, State) -> {noreply, State}
%% where 
%%      From = pid(),
%%      Vec = vec(),
%%      State = obj()
%% @doc
%% Moves the object in the direction of the vector Vec.
%% @end
%%----------------------------------------------------------------------
set_dir(_From, #vec{x=0, y=0, z=0} = Vec, State) ->
    NewState = stop_timer(State),
    {ok, _Reply, NewState2} = call_self(set_property, [dir, Vec], 
        NewState),
    {noreply, NewState2};

set_dir(_From, Vec, #obj{id=Id} = State) ->
    NewState = State,
    %NewState = start_timer(State),
    % Maybe this function should already assume a normalized vector?
    NormVec = util:normalize(Vec),
    call_self(event, [obj_dir, [Id, NormVec]], State),
    case call_self(get_property, [dir], State) of
        {ok, undefined, _NewState} ->
            {ok, _Reply, NewState2} = call_self(set_property, 
                [dir, NormVec], NewState),
            {noreply, NewState2};
        {ok, _Value, _NewState} ->
            {ok, _Reply, NewState2} = call_self(set_property, 
                [dir, NormVec], NewState),
            {noreply, NewState2}
    end.

get_dir(_From, State) ->
    {ok, Reply, _State} = call_self(get_property, [dir], State),
    {reply, Reply, State}.
 

set_speed(_From, Speed, State) ->
    {ok, _Reply, NewState} = call_self(set_property, [speed, Speed], 
        State),
    {noreply, NewState}.

get_speed(_From, State) ->
    case call_self(get_property, [speed], State) of
        {ok, undefined, _NewState} ->
            {reply, 1, State};
        {ok, Speed, _NewState} ->
            {reply, Speed, State}
    end.

%% @private
update_pos(_From, #obj{id=_Id} = State) ->
    case call_self(get_property, [dir], State) of
        {ok, undefined, _State} ->
            %error_logger:info_report([{update_pos_undef, self()}]),
            {noreply, State};
        {ok, Dir, _State} ->
            {ok, Pos, _State} = call_self(get_pos, State),
            {ok, Speed, _State} = call_self(get_speed, State),
            Vec = util:vector_mult(Dir, Speed),
            NewPos = util:vector_add(Pos, Vec),
            %libtree.srv:event(obj_pos, [Id, NewPos]),
            {ok, _Reply, NewState} = call_self(set_pos, [NewPos], 
                State),
            %error_logger:info_report([{update_pos_vec, Vec, pos, NewPos}]),
            update_checkpoint(NewPos, NewState)
    end.

update_checkpoint(_NewPos, State) ->
    {noreply, State}.

%% @private 
set_checkpoint(Pos, State) ->
    {ok, _Reply, NewState} = call_self(set_property, 
        [pos_checkpoint, Pos], State),
    NewState.

%% @private
start_timer(State) ->
    case call_self(get_property, [tref], State) of
        {ok, undefined, _State} ->
            %error_logger:info_report([{starting_timer, self()}]),
            {ok, TimerRef} = timer:apply_interval(1000, obj, async_call, 
                [self(), update_pos]),
            {ok, _Reply, NewState} = call_self(set_property, 
                [tref, TimerRef], State),
            NewState;
        {ok, _TimerRef, _State} ->
           State 
    end.

stop_timer(State) ->
    %error_logger:info_report([{stopping_timer, self()}]),
    {ok, TimerRef, _State} = call_self(get_property, [tref], State),
    timer:cancel(TimerRef),
    {ok, _Reply, NewState} = call_self(set_property, [tref, undefined], 
        State),
    NewState.

get_pos(_From, State) ->
    {ok, Reply, _State} = call_self(get_property, [pos], State),
    {reply, Reply, State}.

set_pos(_From, Pos, State) ->
   {ok, _Reply, NewState} = call_self(set_property, [pos, Pos], State),
   {noreply, NewState}.

query_entity(From, #obj{id=Id} = State) ->
    case call_self(get_pos, State) of
        {ok, undefined, _} ->
            %error_logger:info_report([{?MODULE, query_entity, no_dir}]),
            pass;
        {ok, Pos, _State} ->
            async_call(From, queried_entity, [{id, Id}, {key, pos},
                {value, Pos}])
    end,
    case call_self(get_dir, State) of
        {ok, undefined, _} ->
            %error_logger:info_report([{?MODULE, query_entity, no_dir}]),
            pass;
        {ok, Dir, _} ->
            async_call(From, queried_entity, [{id, Id}, {key, dir},
                {value, Dir}])
    end,
    % If speed is undefined, get_speed returns the integer 1.
    {ok, Speed, _} = call_self(get_speed, State),
    async_call(From, queried_entity, 
        [{id, Id}, {key, speed}, {value, Speed}]),
    apply(obj, query_entity, [From, State]).

