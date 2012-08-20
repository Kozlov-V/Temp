%%--------------------------------------------------------------------- 
%% @author Christian Flodihn <christian@flodihn.se>
%% @copyright Christian Flodihn
%% @doc 
%% This is the generic object, extended by all objects. It contain
%% functions for get/set properties and area migration.
%%
%% Other objects can call functions the object through the event or
%% async_call function.
%%
%% All functions in an object must return either {reply, Reply, NewState}
%% or {noreply, NewState}, if the object was called throgh the call
%% function the and the function return {reply, Reply, NewState} the tuple
%% {ok, Reply} is sent back to the caller. 
%%
%% If the object was called with the call function but returned 
%% {noreply, NewState} a tuple {ok, noreply} is sent back to the caller.
%% If the object is called through the async_call function the retuned
%% value is ignored.
%% 
%% In all three cases the object continues is execution with the new state.
%%
%% The object can call itself with the function call_self which wraps
%% the values to return {ok, Value, NewState}.
%% @end
%%--------------------------------------------------------------------- 

-module(obj).
-author("christian@flodihn.se").

%% @headerfile "obj.hrl"
-include("obj.hrl").

%% @headerfile "vec.hrl"
-include("vec.hrl").

-import(obj_loop, [loop_init/2]).
-import(error_logger).
-import(dict).
-import(io).

-import(obj_loop, [has_fun/3]).

% Interface.
-export([
    init/1,
    post_init/1,
    async_call/2,
    async_call/3,
    async_call/4,
    call/2,
    call/3,
    call_self/2,
    call_self/3
    ]).

% Functions used through inheritance from other modules.
-export([
    event/3,
    event/4,
    set_property/4,
    get_property/3,
    get_id/2,
    is/3,
    transform/3,
    query_entity/2,
    do_anim/3,
    do_anim/4,
    stop_anim/3,
    obj_created/3,
    obj_pos/4,
    obj_dir/4,
    obj_leave/3,
    obj_enter/3,
    obj_anim/5,
    obj_stop_anim/4,
    test/2,
    test/3,
    get_group/2,
    set_group/3,
    set_heart_beat/3,
    disable_heart_beat/2,
    enable_logging/2,
    disable_logging/2,
    log/3,
    heart_beat/2,
    set_last_heart_beat/2,
    get_last_heart_beat/2
    ]).

%%----------------------------------------------------------------------
%% @spec init(State) -> {ok, NewState}
%% where
%%      State = obj(),
%%      NewState = obj()
%% @doc
%% Initiates the object state.
%% @end
%%----------------------------------------------------------------------
init(#obj{id=undefined, type=Type} = State) ->
    {ok, Id} = libid.srv:generate_id(),
    libstd.srv:register_obj(Id, self()),
    NewState = State#obj{id=Id},
    {ok, NewState2} = Type:post_init(NewState), 
    {ok, NewState2}.

post_init(State) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% @spec async_call(To, Event) -> ok
%% where
%%      To = pid(),
%%      Event = atom()
%% @doc
%% Same as async_call/4 but with an empty list as arguments and the pid
%% From as self().
%% @end
%%----------------------------------------------------------------------
async_call(To, Event) ->
    async_call(self(), To, Event, []).

%%----------------------------------------------------------------------
%% @spec async_call(From, To, Event) -> ok
%% where
%%      From = pid(),
%%      To = pid(),
%%      Event = atom()
%% @doc
%% When From and To is a pid, this function calls async_call/4 with an 
%% empy list as argument.
%% @end
%%----------------------------------------------------------------------
async_call(From, To, Event) when is_pid(From) and is_pid(To) ->
    async_call(From, To, Event, []);

%%----------------------------------------------------------------------
%% @spec async_call(To, Event, Args) -> ok
%% where
%%      To = pid(),
%%      Event = atom()
%%      Args = list()
%% @doc
%% When To is a pid, this will call sync_call/4 with From pid as self().
%% @end
%%----------------------------------------------------------------------
async_call(To, Fun, Args) ->
    async_call(self(), To, Fun, Args).

%%----------------------------------------------------------------------
%% @spec async_call(From, To, Event, Args) -> ok
%% where
%%      To = pid(),
%%      Event = atom(),
%%      Args = list()
%% @doc
%% Execute an asynchronus call in the object P, calls the function Event 
%% with the arguments Args.
%% @end
%%----------------------------------------------------------------------
async_call(From, To, Fun, Args) ->
    To ! {execute, {from, From}, {call, Fun}, {args, Args}},
    ok.


%%----------------------------------------------------------------------
%% @spec call(To, Event) -> {ok, Result} | {error, Reason}
%% where
%%      To = pid(),
%%      Event = atom()
%% @doc
%% Same as sync_call/3 but with an empty list as argument.
%% @end
%%----------------------------------------------------------------------
call(To, Event) ->
    call(To, Event, []).   

%%----------------------------------------------------------------------
%% @spec call(To, Event, Args) -> {ok, Result} | {error, Reason}
%% where
%%      To = pid(),
%%      Event = atom(),
%%      Args = list()
%% @doc
%% Executes the function Event with the arguemtns Args in the object To 
%% and returns the result.
%% @end
%%----------------------------------------------------------------------
call(To, Fun, Args) ->
    EventId = make_ref(),
    To ! {execute, {from, self()}, {call, Fun}, {args, Args}, 
        {event_id, EventId}},
    receive 
        {EventId, Result} ->
            {ok, Result}
    after 10000 ->
        {error, timeout}
    end.

%%----------------------------------------------------------------------
%% @spec call_self(Fun, State) -> {ok, Reply, NewState} | 
%%      {ok, NewState} |{error, Reason}
%% where
%%      Fun = atom(),
%%      Args = list()
%% @doc
%% Sane as call_self/3 but with an empty list as argument
%% @end
%%----------------------------------------------------------------------
call_self(Fun, State) ->
    call_self(Fun, [], State).
 
%%----------------------------------------------------------------------
%% @spec call_self(Event, Args, State) -> {ok, Reply, NewState} | 
%%      {ok, NewState} |{error, Reason}
%% where
%%      Event = atom(),
%%      Args = list()
%% @doc
%% Used by objects to call to a function in itself.
%% @end
%%----------------------------------------------------------------------
% This function has too many nested cases, fix later.
call_self(Fun, Args, #obj{type=Type} = State) ->
    ArgLen = length(Args) + 2,
    case cache:fetch({Fun, ArgLen}) of
        undefined ->
            case has_fun(Type:module_info(exports), Fun, ArgLen) of
                true ->
                    cache:cache({Fun, ArgLen}, Type),
                    case apply(Type, Fun, [self()] ++ Args ++ [State]) of
                        {reply, Reply, NewState} ->
                            {ok, Reply, NewState};
                        {noreply, NewState} ->
                            {ok, noreply, NewState}
                    end;
                false ->
                    call_self(State#obj.parents, Fun, Args, State)
            end;
        CachedType ->
            case apply(CachedType, Fun, [self()] ++ Args ++ [State]) of
                {reply, Reply, NewState} ->
                    {ok, Reply, NewState};
                {noreply, NewState} ->
                    {ok, noreply, NewState}
            end
    end.        
    
call_self([Parent | Parents], Fun, Args, State) ->
    ArgLen = length(Args) + 2,
    case has_fun(Parent:module_info(exports), Fun, ArgLen) of
        true ->
            cache:cache({Fun, ArgLen}, Parent),
            case apply(Parent, Fun, [self()] ++ Args ++ [State]) of
                {reply, Reply, NewState} ->
                    {ok, Reply, NewState};
                {noreply, NewState} ->
                    {ok, noreply, NewState}
            end;
        false ->
            call_self(Parents, Fun, Args, State)
   end.

event(From, Fun, State) ->
    event(From, Fun, [], State).

event(_From, Fun, Args, State) ->
    case call_self(get_property, [quad], State) of
        {ok, no_quad, _State} ->
            error_logger:info_report([{obj, event_error, "No quad"}]);
        {ok, Quad, _State} ->
            libtree.srv:event(Quad, Fun, Args)
    end,
    {noreply, State}.

%%----------------------------------------------------------------------
%% @spec set_property(From, Property, Value, State) -> {noreply, State}
%% where
%%      From = pid(),
%%      Property = atom(),
%%      Value = any(),
%%      State = obj()
%% @doc
%% Saves a property Property with the value Value in the object.
%% From is the pid of the process making the call.
%% State is the objects current state.
%% @end
%%----------------------------------------------------------------------
set_property(From, Property, Value, #obj{properties=undefined} = State) ->
    set_property(From, Property, Value, State#obj{properties=dict:new()});

set_property(_From, Property, Value, #obj{properties=Properties} = State) ->
    NewProperties = dict:store(Property, Value, Properties),
    {noreply, State#obj{properties=NewProperties}}.

%%----------------------------------------------------------------------
%% @spec get_property(From, Property, State) -> {reply, Value, State}
%% where
%%      From = pid(),
%%      Property = atom(),
%%      State = obj(),
%%      Value = any()
%
%% @doc
%% Retreives the property Property in the object, returns either the 
%% the property value or if not found, the atom false.
%% @end
%%----------------------------------------------------------------------
get_property(From, Property, #obj{properties=undefined} = State) ->
    get_property(From, Property, State#obj{properties=dict:new()});

get_property(_From, Property, #obj{properties=Properties} = State) ->
    case dict:is_key(Property, Properties) of
        true ->
            Value = dict:fetch(Property, Properties),
            {reply, Value, State};
        false ->
            {reply, undefined, State}
    end.

%%----------------------------------------------------------------------
%% @spec get_id(From, State) -> {reply, Id, State}
%% where
%%      From = pid(),
%%      State = obj()
%% @doc
%% Retrieves the id of the object.
%% @end
%%----------------------------------------------------------------------
get_id(_From, State) ->
    {reply, State#obj.id, State}.

%%----------------------------------------------------------------------
%% @spec is(From, Type, State) -> {reply, true, State} | 
%%  {reply, false, State}
%% where
%%      From = pid(),
%%      Type = atom(),
%%      State = obj()
%% @doc
%% Query if the object is a certain type. This is used to determine what
%% type of module an object is extending.
%% For example a player which inherits movable but not god, will return 
%% true for the type player and movable but false for god.
%% @end
%%----------------------------------------------------------------------
is(_From, obj, State) ->
    {reply, true, State};

is(_From, _Type, State) ->
    {reply, false, State}.

%%----------------------------------------------------------------------
%% @spec transform(From, NewType, State) -> {noreply, NewState}
%% where
%%      From = pid(),
%%      Type = atom(),
%%      State = obj()
%% @doc
%% Transforms the object into an object of type Type.
%% @end
%%----------------------------------------------------------------------
transform(_From, NewType, State) ->
    error_logger:info_report([{transforming, State#obj.type, NewType}]),
    {noreply, State#obj{type=NewType}}.

%%----------------------------------------------------------------------
%% @spec query_entity(From, State) -> {noreply, NewState}
%% where
%%      From = pid(),
%%      State = obj()
%% @doc
%% Queries the object for graphical bound properties, such as mesh, 
%% billboard, position and direction. Sends one asyncronus message back 
%% the caller for each property.
%% @end
%%----------------------------------------------------------------------
query_entity(From, #obj{id=Id} = State) ->  
    %error_logger:info_report([{query_entity, from, From}]),
    case call_self(get_property, [mesh], State) of
        {ok, undefined, _} ->
            pass;
        {ok, Mesh, _} ->
            async_call(From, queried_entity, [{id, Id}, {key, mesh}, 
                {value, Mesh}])
    end,
    case call_self(get_property, [pos], State) of
        {ok, undefined, _} ->
            pass;
        {ok, Pos, _State} ->
            async_call(From, queried_entity, [{id, Id}, {key, pos}, 
                {value, Pos}])
    end,
    case call_self(get_property, [billboard], State) of
        {ok, undefined, _} ->
            pass;
        {ok, Billboard, _} ->
            async_call(From, queried_entity, [{id, Id}, 
                {key, billboard}, {value, Billboard}])
    end,
    case call_self(get_property, [anim], State) of
        {ok, undefined, _} ->
            pass;
        {ok, Anim, _} ->
            async_call(From, queried_entity, [{id, Id}, 
                {key, anim}, {value, Anim}])
    end,

    {noreply, State}.

do_anim(From, Anim, State) ->
    do_anim(From, Anim, 0, State).

do_anim(_From, Anim, 0, #obj{id=Id} = State) ->
    {ok, _Reply, NewState} = call_self(set_property, 
        [anim, Anim], State),
    call_self(event, [obj_anim, [Id, Anim, 0]], State),
    {noreply, NewState};

do_anim(_From, Anim, Nr, #obj{id=Id} = State) ->
    call_self(event, [obj_anim, [Id, Anim, Nr]], State),
    {noreply, State}.

stop_anim(_From, Anim, #obj{id=Id} = State) ->
    call_self(event, [obj_stop_anim, [Id, Anim]], State),
    {noreply, State}.

%%----------------------------------------------------------------------
%% @spec obj_created(From, Id, State) -> {noreply, NewState}
%% where
%%      From = pid(),
%%      From = id(),
%%      State = obj()
%% @doc
%% The default behaviour is to ignore created objects.
%% @end
%%----------------------------------------------------------------------
obj_created(_From, _Id, State) ->
    %error_logger:info_report([{obj_created, ignored, Id}]),
    {noreply, State}.

obj_pos(_From, _Id, _Pos, State) ->
    %error_logger:info_report([{obj_dir, ignored, Id, Vec}]),
    {noreply, State}.


%%----------------------------------------------------------------------
%% @spec obj_dir(From, Id, Vec, State) -> {noreply, NewState}
%% where
%%      From = pid(),
%%      From = id(),
%%      State = obj()
%% @doc
%% The default behaviour is to ignore created objects.
%% @end
%%----------------------------------------------------------------------
obj_dir(_From, _Id, _Vec, State) ->
    %error_logger:info_report([{obj_dir, ignored, Id, Vec}]),
    {noreply, State}.

obj_leave(_From, _Id, State) ->
    {noreply, State}.

obj_enter(_From, _Id, State) ->
    {noreply, State}.

obj_anim(_From, _Id, _Nr, _Anim, State) ->
    {noreply, State}.

obj_stop_anim(_From, _Id, _Anim, State) ->
    {noreply, State}.

test(_From, State) ->
    io:format("Test function called successfully.~n"),
    {noreply, State}.

test(_From, Arg, State) ->
    io:format("Test function called successfully with arg: ~p.~n", 
        [Arg]),
    {noreply, State}.

get_group(_From, State) ->
    {ok, Group, _State} = call_self(get_property, [group], State),
    {reply, Group, State}.

set_group(_From, Group, State) ->
    {ok, _Reply, NewState} = call_self(set_property, [group, Group], State),
    {noreply, NewState}.

set_heart_beat(_From, Time, State) ->
    {ok, _Reply, NewState} = call_self(set_property, [heart_beat, Time],
        State),
    {noreply, NewState}.

disable_heart_beat(_From, State) ->
    {ok, _Reply, NewState} = call_self(set_property, [heart_beat, infinity],
        State),
    {noreply, NewState}.

enable_logging(_From, State) ->
    {ok, _Reply, NewState} = call_self(set_property, [logging, true], 
        State),
    {noreply, NewState}.

disable_logging(_From, State) ->
    {ok, _Reply, NewState} = call_self(set_property, [logging, undefined], 
        State),
    {noreply, NewState}.


log(From, Line, State) ->
    case is_logging(From, State) of
        true ->
            error_logger:info_report(Line);
        false ->
            pass
    end,
    {noreply, State}.

is_logging(_From, State) ->
    case call_self(get_property, [logging], State) of   
        {ok, undefined, _State} ->
            false;
        {ok, true, _State} ->
            true
    end.

heart_beat(From, State) ->
    set_last_heart_beat(From, State).

set_last_heart_beat(_From, State) ->
    {ok, _Reply, NewState} = call_self(set_property, 
        [last_heart_beat, now()], State),
    {noreply, NewState}.

get_last_heart_beat(_From, State) ->
    {ok, LastTime, _State} = call_self(get_property, [last_heart_beat], 
        State),
    MilliSeconds = timer:now_diff(now(), LastTime) / 10000,
    {reply, MilliSeconds, State}.

