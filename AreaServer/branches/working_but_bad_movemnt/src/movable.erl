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
    heart_beat/2,
    query_entity/2,
    update_pos/2,
    set_checkpoint/3,
    get_checkpoint/2,
    set_dir/3,
    get_dir/2,
    get_pos/2,
    set_pos/3,
    set_speed/3,
    get_speed/2,
    quadtree_assign/2,
    get_quad/2,
    set_quad/3
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
    obj:init(State).

post_init(State) ->
    {ok, State}.

heart_beat(From, State) ->
    {ok, _Reply, NewState} = call_self(update_pos, State),
    obj:heart_beat(From, NewState).

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
    {ok, _Reply, NewState} = call_self(set_property, [dir, Vec], 
        State),
    error_logger:info_report([{State#obj.id, Vec}]),
    {noreply, NewState};

set_dir(_From, Vec, #obj{id=Id} = State) ->
    call_self(log, [{set_dir, Vec}], State),
    NewState = State,
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
            {reply, 0, State};
        {ok, Speed, _NewState} ->
            {reply, Speed, State}
    end.

%% @private
update_pos(From, State) ->
    {ok, Dir, _State} = call_self(get_dir, State),
    {ok, Pos, _State} = call_self(get_pos, State),
    {ok, Speed, _State} = call_self(get_speed, State),
    if 
        Dir == undefined; Pos == undefined; Speed == undefined ->
            call_self(log, [{update_pos, aborted}], State),
            {noreply, State};
        true ->
            Traveled = util:vector_mult(Dir, Speed),
            NewPos = util:vector_add(Pos, Traveled),
            call_self(log, [{update_pos, NewPos}], State),
            call_self(log, [{update_pos, NewPos}], State), 
            {ok, _Reply, NewState} = call_self(set_pos, [NewPos], State),
            update_checkpoint(From, NewState)
    end.

update_checkpoint(From, State) ->
    {ok, Pos, _State} = call_self(get_pos, State),
    case call_self(get_checkpoint, State) of
        {ok, undefined, _State} ->
            {ok, _Reply, NewState} = call_self(set_checkpoint, [Pos], 
                State),
            {noreply, NewState};
        {ok, CheckPoint, _State} ->
            case util:vector_diff(Pos, CheckPoint) of
                Diff when Diff > 10 ->
                    %error_logger:info_report(["Assigning for new tree"]),
                    {ok, _Reply, NewState} = call_self(set_checkpoint,
                        [Pos], State),
                    quadtree_assign(From, NewState);
                _Diff  ->
                    %error_logger:info_report(["Diff not > 10"]),
                    {noreply, State}
            end
    end.

set_checkpoint(_From, Pos, State) ->
    {ok, _Reply, NewState} = call_self(set_property, [checkpoint, Pos],
        State),
    {noreply, NewState}.

get_checkpoint(_From, State) ->
    {ok, CheckPoint, _State} = call_self(get_property, [checkpoint],
        State),
    {reply, CheckPoint, State}.

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

quadtree_assign(_From, #obj{id=Id} = State) ->
    {ok, CurrentQuad, _State} = call_self(get_quad, State),
    case call_self(get_pos, State) of
        {ok, undefined, _State} ->
            Quad = libtree.srv:assign(Id, self(), #vec{}, CurrentQuad),
            {ok, _Reply, NewState} = call_self(set_quad, 
                [Quad], State),
            error_logger:info_report([{"What What quadtree_assign on object without position???", quadtree_assign, Quad}]), 
            {noreply, NewState};
        {ok, Pos, _State} ->
            NewQuad = libtree.srv:assign(Id, self(), Pos, CurrentQuad),
            case CurrentQuad == NewQuad of
                true ->
                    %error_logger:info_report("Same Quad"),
                    {noreply, State};
                false ->
                    %error_logger:info_report("New Quad"),
                    {ok, _Reply, NewState} = call_self(set_quad, 
                        [NewQuad], State),
                    %call_self(event, [obj_leave, [Id]], State),
                    {noreply, NewState}
            end
    end.

get_quad(_From, State) ->
    {ok, Reply, _State} = call_self(get_property, [quad], State),
    {reply, Reply, State}.

set_quad(_From, Quad, State) ->
    {ok, _Reply, NewState} = call_self(set_property, [quad, Quad], State),
    {noreply, NewState}.

