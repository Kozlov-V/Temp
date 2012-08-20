%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodihn.se>
%% @copyright Christian Flodihn
%% @doc
%% This module implements the player object which allows communication
%% with the players connection process in the connection server.
%% This object receives commands from the player connection process and
%% decides what to send back to the player.
%% @end
%%----------------------------------------------------------------------
-module(player).

%% @docfile "doc/id.edoc"
%% @headerfile "obj.hrl"
-include("obj.hrl").
-include("vec.hrl").

-import(error_logger).


-import(libplayer).
-import(obj, [async_call/2, call_self/3]).

-export([
    init/1,
    post_init/1,
    heart_beat/2,
    is/3,
    logout/2,
    pulse/2,
    pulse/3,
    queried_entity/5,
    query_env/2,
    save/2,
    obj_created/3,
    obj_pos/4,
    obj_dir/4,
    obj_leave/3,
    obj_enter/3,
    obj_anim/5,
    increase_speed/2,
    decrease_speed/2,
    set_dir/3
    ]).


%%----------------------------------------------------------------------
%% @spec init(State) ->{ok, NewState}
%% where
%%      State = obj()
%% @doc
%% Initiates the player. 
%% @end
%%----------------------------------------------------------------------
init(State) ->
    NewState = State#obj{parents=[movable, obj]},
    movable:init(NewState).

post_init(State) ->
    {ok, State}.

heart_beat(From, State) ->
    %error_logger:info_report([{player, heart_beat}]),
    movable:heart_beat(From, State).

%%----------------------------------------------------------------------
%% @spec is(From, Type, State) ->{ok, true | false, State}
%% where
%%      From = pid(),
%%      Type = atom(),
%%      State = obj()
%% @doc
%% @see obj:is/3
%% @end
%%----------------------------------------------------------------------
is(_From, player, State) ->
    {reply, true, State};

is(From, Other, State) ->
    apply(movable, is, [From, Other, State]).

%%----------------------------------------------------------------------
%% @spec logout(From, State) -> ok
%% where
%%      From = pid(),
%%      State = obj()
%% @doc
%% Logout a player at once.
%% @end
%%----------------------------------------------------------------------
logout(_From, #obj{id=Id}) ->
    % This should be standard in the base obj.erl, also make objects exit
    % by sending stop msg to obj_loop.
    %libstd.srv:unregister_obj(Id),
    error_logger:info_report([{player, Id, logout}]),
    exit(normal).

%%----------------------------------------------------------------------
%% @spec pulse(From, State) -> ok
%% where
%%      From = pid(),
%%      State = obj()
%% @doc
%% Send a pulse to all nearby objects, objects that are "hit" send their
%% graphical representation and position back to the object.
%% See also @see queried_entity/5
%% @end
%%----------------------------------------------------------------------
pulse(_From, State) ->
    %error_logger:info_report(["Pulse"]),
    call_self(event, [query_entity], State),
    {noreply, State}.

pulse(_From, Id, State) ->
    %error_logger:info_report([{"Pulsing object", Id}]),
    {ok, Id, Pid} = libstd.srv:get_obj(Id),
    async_call(Pid, query_entity),
    {noreply, State}.

%----------------------------------------------------------------------
%% @spec queried_entity(From, {id, Id}, {key, Key}, {value, Value}, State) 
%% -> ok
%% where
%%      From = pid(),
%%      Id = id(),
%%      Key = string(),
%%      Value = any(),
%%      State = obj()
%% @doc
%% When an pulse hit an object it send makes an asyncronus call back to 
%% this object with this function, containging the queried properties. 
%% The properties are sent back to the connection.
%% @end
%%----------------------------------------------------------------------
queried_entity(_From, {id, Id}, {key, pos}, {value, Pos}, State) ->
    {ok, Conn, _State} = call_self(get_property, ["conn"], State),
    error_logger:info_report([{queried_entity, Id, "pos", Pos, Conn}]),
    Conn ! {new_pos, [Id, Pos]}, 
    {noreply, State};

queried_entity(_From, {id, Id}, {key, billboard}, {value, Billboard}, 
    State) ->
    {ok, Conn, _State} = call_self(get_property, ["conn"], State),
    error_logger:info_report([{queried_entity, Id, "billboard", Billboard, 
        Conn}]),
    Conn ! {billboard, [Id, Billboard]}, 
    {noreply, State};

queried_entity(_From, {id, Id}, {key, mesh}, {value, Mesh}, State) ->
    {ok, Conn, _State} = call_self(get_property, ["conn"], State),
    error_logger:info_report([{queried_entity, Id, "mesh", Mesh, Conn}]),
    Conn ! {mesh, [Id, Mesh]}, 
    {noreply, State};

queried_entity(_From, {id, Id}, {key, dir}, {value, Dir}, State) ->
    {ok, Conn, _State} = call_self(get_property, ["conn"], State),
    error_logger:info_report([{queried_entity, Id, "dir", Dir, Conn}]),
    Conn ! {obj_dir, {id, Id}, {dir, Dir}}, 
    {noreply, State};

queried_entity(_From, {id, Id}, {key, speed}, {value, Speed}, State) ->
    {ok, Conn, _State} = call_self(get_property, ["conn"], State),
    error_logger:info_report([{queried_entity, Id, "key", Speed, Conn}]),
    Conn ! {obj_speed, {id, Id}, {speed, Speed}}, 
    {noreply, State};

queried_entity(_From, {id, Id}, {key, anim}, {value, Anim}, State) ->
    {ok, Conn, _State} = call_self(get_property, ["conn"], State),
    error_logger:info_report([{queried_entity, Id, "anim", Anim, Conn}]),
    Conn ! {obj_anim, {id, Id}, {anim, Anim}, {repeat, 0}},
    %Conn ! {obj_anim, {id, Id}, {speed, Speed}}, 
    {noreply, State}.


%%----------------------------------------------------------------------
%% @spec query_env(From, State) -> ok
%% where
%%      From = pid(),
%%      State = obj()
%% @doc
%% Query the skybox and terrain, returns the result to the players 
%% connection in the connection server.
%% @end
%%----------------------------------------------------------------------
query_env(_From, State) ->
    {ok, Conn, _State} = call_self(get_property, ["conn"], State),
    case libenv.srv:get_skybox() of
        {skybox, Skybox} ->
            Conn ! {skybox, Skybox};
        {error, _} ->
            pass
    end,
    case libenv.srv:get_terrain() of
        {terrain, Terrain} ->
            Conn ! {terrain, Terrain};
        {error, _} ->
            pass
    end,
    {noreply, State}.

save(_From, #obj{id=Id} = State) ->
    Result = libplayer.srv:save(Id, State),
    {reply, Result, State}.

obj_created(_From, Id, State) ->
    error_logger:info_report(obj_created),
    case call_self(get_property, [mesh], State) of
        {ok, undefined, _State} ->
            % There is no idea letting a client now about an object if
            % it isn't visible.
            pass;
        {ok, Mesh, _State} ->
            {ok, Conn, _State} = call_self(get_property, ["conn"], 
                State),
            Conn ! {obj_created, {id, Id}, {mesh, Mesh}}
    end,
    {noreply, State}.

obj_pos(_From, Id, Pos, State) ->
    error_logger:info_report(obj_pos),
    {ok, Conn, _State} = call_self(get_property, ["conn"], State),
    Conn ! {obj_pos, {id, Id}, {pos, Pos}},
    {noreply, State}.

obj_dir(_From, Id, Vec, State) ->
    error_logger:info_report([{State#obj.id, obj_dir, Id}]),
    {ok, Conn, _State} = call_self(get_property, ["conn"], State),
    Conn ! {obj_dir, {id, Id}, {dir, Vec}},
    {noreply, State}.

obj_leave(From, _Id, State) when From == self() ->
    {noreply, State};

obj_leave(_From, Id, State) ->
    error_logger:info_report(obj_leave),
    {ok, Conn, _State} = call_self(get_property, ["conn"], State),
    Conn ! {obj_leave, {id, Id}},
    {noreply, State}.

obj_enter(_From, Id, State) ->
    error_logger:info_report([{obj_enter, Id}]),
    {ok, Conn, _State} = call_self(get_property, ["conn"], State),
    Conn ! {obj_enter, {id, Id}},
    {noreply, State}.

obj_anim(_From, Id, Anim, Nr, State) ->
    error_logger:info_report(obj_anim),
    {ok, Conn, _State} = call_self(get_property, ["conn"], State),
    Conn ! {obj_anim, {id, Id}, {anim, Anim}, {repeat, Nr}},
    {noreply, State}.

%obj_stop_anim(_From, Id, Anim, State)->
%    error_logger:info_report(obj_stop_anim),
%    {ok, Conn, _State} = call_self(get_property, ["conn"], State),
%    Conn ! {obj_stop_anim, {id, Id}, {anim, Anim}},
%    {noreply, State}.

increase_speed(_From, State) ->
    {ok, OldSpeed, _State} = obj:call_self(get_speed, State),
    {ok, _Reply, NewState} = obj:call_self(set_speed, [OldSpeed + 1],
        State),
    error_logger:info_report([{increase_speed, OldSpeed + 1}]),
    %{ok, Conn, _State} = call_self(get_property, ["conn"], State),
    %Conn ! {obj_anim, {id, Id}, {anim, Anim}, {repeat, Nr}},
    {noreply, NewState}.

decrease_speed(_From, State) ->
    error_logger:info_report([{decrease_speed}]),
    %{ok, Conn, _State} = call_self(get_property, ["conn"], State),
    %Conn ! {obj_anim, {id, Id}, {anim, Anim}, {repeat, Nr}},
    {noreply, State}.

set_dir(_From, Dir, State) ->
    error_logger:info_report({set_dir, Dir}),
    {noreply, State}.
