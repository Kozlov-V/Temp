%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodihn.se>
%% @copyright Christian Flodihn
%% @doc
%% This module contains the object loop that every object executes.
%% @end
%%----------------------------------------------------------------------
-module(obj_loop).
-author("christian@flodihn.se").

%% @headerfile "obj.hrl"
-include("obj.hrl").

%% Exports for fitting into a supervisor tree
-export([
    start_link/2
    ]).

% Interal exports
-export([
    loop_init/1,
    loop_init/2,
    loop/1
    ]).

% Exports used by other modules.
-export([
    has_fun/3
    ]).

%%----------------------------------------------------------------------
%% @spec start_link(Arg, Type) -> {ok, Pid}
%% where
%%      Arg = any()
%%      Type = any()   
%% @doc
%% Starts a new object with a new state. Arg should either be new_state or 
%% {existing_state, State}, Type should be a tuple {type, Mod} where Mod
%% is the implementation module of the object, for example obj, player or 
%% animal.
%% @end
%%----------------------------------------------------------------------

start_link(new_state, {type, Type}) ->
    Pid = spawn_link(?MODULE, loop_init, [Type]),
    {ok, Pid};

start_link({existing_state, State}, {type, Type}) ->
    Pid = spawn_link(?MODULE, loop_init, [Type, State]),
    {ok, Pid}.

%%----------------------------------------------------------------------
%% @spec loop_init(Type) -> ok
%% where
%%      Type = atom()    
%% @doc
%% Starts the object loop with a new state.
%% @end
%%----------------------------------------------------------------------
loop_init(Type) ->
    State = #obj{type=Type},
    {ok, InitState} = Type:init(State),
    loop(InitState).

%%----------------------------------------------------------------------
%% @spec loop_init(Type, State) -> ok
%% where
%%      Type = atom(),
%%      State = obj()
%% @doc
%% Starts the object loop with an existing state.
%% @end
%%----------------------------------------------------------------------
loop_init(_Type, State) ->
    loop(State).

%%----------------------------------------------------------------------
%% @spec loop(State) -> ok
%% where
%%      State = obj()
%% @doc
%% Object loop for all types of objects.
%% @end
%%----------------------------------------------------------------------
loop(State) ->
    receive
        % An execute call without an event id calls apply_fun/4
        % which does not send anything back to the calling process "From".
        {execute, {from, From}, {call, Fun}, {args, Args}} ->
            {ok, NewState} = apply_async_inheritance(
                From, Fun, Args, State),
            loop(NewState);
        % An execute call with an event id will try to send back
        % the result to the calling process "From".
        {execute, {from, From}, {call, Fun}, {args, Args}, 
            {event_id, EventId}} ->
            {ok, NewState} = apply_sync_inheritance(
                From, Fun, Args, State, EventId),
            loop(NewState);
        % Implement some sort of upgrade
        % {upgrade, From} ->
        %   ?MODULE:loop(State);
        Other ->
            error_logger:info_report([{unexpected_message, Other}]),
            loop(State)
    end.

apply_sync_inheritance(From, Fun, Args, #obj{type=Type} = State, EventId) ->
    ArgLen = length(Args) + 2,
    case cache:fetch({Fun, ArgLen}) of
        undefined ->
            case has_fun(Type:module_info(exports), Fun, ArgLen) of
                true ->
                    cache:cache({Fun, ArgLen}, Type),
                    apply_fun(Type, From, Fun, Args, State, EventId);
                false ->
                    apply_sync_inheritance(
                        State#obj.parents, From, Fun, Args, State, EventId)
            end;
        CachedType ->
            apply_fun(CachedType, From, Fun, Args, State, EventId)
    end.
            

apply_sync_inheritance([], _From, Fun, Args, #obj{type=Type} = State, 
    _EventId) ->
    error_logger:error_report([{Type, undef, Fun, Args}]),
    {ok, State};

apply_sync_inheritance([Parent | Parents], From, Fun, Args, State, 
    EventId) ->
    ArgLen = length(Args) + 2,
    case has_fun(Parent:module_info(exports), Fun, ArgLen) of
        true ->
            cache:cache({Fun, ArgLen}, Parent),
            apply_fun(Parent, From, Fun, Args, State, EventId);
        false ->
            apply_sync_inheritance(
                Parents, From, Fun, Args, State, EventId)
    end.

apply_async_inheritance(From, Fun, Args, #obj{type=Type} = State) ->
    ArgLen = length(Args) + 2,
    case cache:fetch({Fun, ArgLen}) of
        undefined ->
            case has_fun(Type:module_info(exports), Fun, ArgLen) of
                true ->
                    cache:cache({Fun, ArgLen}, Type),
                    apply_fun(Type, From, Fun, Args, State);
                false ->
                    apply_async_inheritance(
                        State#obj.parents, From, Fun, Args, State)
            end;
        CachedType ->
            apply_fun(CachedType, From, Fun, Args, State)
    end.
        

apply_async_inheritance([], _From, Fun, Args, #obj{type=Type} = State) ->
    error_logger:error_report([{Type, undef, Fun, Args}]),
    {ok, State};

apply_async_inheritance([Parent | Parents], From, Fun, Args, State) ->
    ArgLen = length(Args) + 2,
    case has_fun(Parent:module_info(exports), Fun, ArgLen) of
        true ->
            cache:cache({Fun, ArgLen}, Parent),
            apply_fun(Parent, From, Fun, Args, State);
        false ->
            apply_async_inheritance(Parents, From, Fun, Args, State)
    end.

%%----------------------------------------------------------------------
%% @private
%% @spec apply_fun(Type, From, Event, Args, State) -> {ok, NewState}
%% where
%%      Type = atom(),
%%      From = pid,
%%      Event = atom(),
%%      Args = list(),
%%      State = obj()
%% @doc
%% Applies the function Event, with the arguments Args in itself. Ignores
%% the result.
%% @end
%%----------------------------------------------------------------------
apply_fun(Type, From, Event, Args, State)->
    case apply(Type, Event, [From] ++ Args ++ [State]) of
        {noreply, NewState} ->
            {ok, NewState};
        {reply, _Reply, NewState} ->
            {ok, NewState}
    end.

%%----------------------------------------------------------------------
%% @spec apply_fun(Type, From, Event, Args, State, EventId) -> {ok, NewState}
%% where
%%      Type = atom(),
%%      From = pid,
%%      Event = atom(),
%%      Args = list(),
%%      State = obj(),
%%      EventId = ref()
%% @doc
%% Applies a function in the object type and sends back the result to the
%% calling process From.
%% @end
%%----------------------------------------------------------------------
apply_fun(Type, From, Fun, Args, State, EventId)->
    case apply(Type, Fun, [From] ++ Args ++ [State]) of
        {noreply, NewState} ->
            From ! {EventId, noreply},
            {ok, NewState};
        {reply, Reply, NewState} ->
            From ! {EventId, Reply},
            {ok, NewState}
    end.

has_fun([], _Fun, _NrArgs) ->
    false;

has_fun([{Fun, NrArgs} | _Rest], Fun, NrArgs) ->
    true;

has_fun([{_Fun, _NrArgs} | Rest], Fun, NrArgs) ->
    has_fun(Rest, Fun, NrArgs).
