%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% This is the standard implementation for the player library 'libplayer'.
%% The module provides functions to create and login players.
%% @end
%%----------------------------------------------------------------------

%%@docfile "doc/id.edoc"

-module(libplayer.std_impl).

-import(io).
-import(application).
-import(rpc).

-import(obj_sup).
-import(obj).

-include("char.hrl").

% API
-export([
    init/0,
    unregister_events/0
    ]).

% handlers
-export([
    create/1,
    login/1,
    save/2
    ]).

%%----------------------------------------------------------------------
%% @spec init() -> ok
%% @private
%% @doc
%% Initiates the player library.
%% @end
%%----------------------------------------------------------------------
init() ->
    %{ok, AreaNode} = application:get_env(area_node),
    %rpc:call(AreaNode, areasrv, add_handler, 
    %    [login_char, node(), libplayer, event]),
    %areasrv:add_handler(char_login, libchar),
    %ets:new(players, [named_table]),
    ok.

%%----------------------------------------------------------------------
%% @spec create(Conn) -> {ok, {pid, Pid}, {id, Id}}
%% where
%%      Conn = pid(),
%%      Pid = pid(),
%%      Id = id()
%% @doc
%% Creates a new player object and returns its pid and id.
%% @end
%%----------------------------------------------------------------------
create(Conn) ->
    {ok, Pid} = obj_sup:start(player),
    obj:async_call(Pid, set_property, ["conn", Conn]),
    obj:async_call(Pid, query_env),
    {ok, Id} = obj:call(Pid, get_id, []),
    {ok, Pos} = obj:call(Pid, get_pos),
    Conn ! {new_pos, [Id, Pos]},
    {ok, {pid, Pid}, {id, Id}}.
    
%%----------------------------------------------------------------------
%% @doc
%% @spec login(State) ->{ok, Id, Pid}
%% where
%%      State = obj_state(), 
%%      Id = string(),
%%      Pid = pid() 
%% @type obj_state(). An obj_state record.
%% Purpose: Makes a player login.
%% @end
%%----------------------------------------------------------------------
login(_State) ->
    %From = {pid, self()},
    %error_logger:info_report([{login_char, Data#char.conn}]),
    %{ok, Pid} = obj_sup:start({existing_state, State}, {type, player}),
    %{ok, Id} = obj:event(Pid, get_id),
    %receive {id, Id} ->
    %    {ok, Id, Pid}
    %end.
    %EventId = areasrv:event(From, {register, Pid}),
    %CharId = CharState#char.id,
    %case event:gather(EventId) of
    %    {area_id, AreaId} -> 
    %        {ok, char_logged_in, {char_id, CharId}, {area_id, AreaId}};
    %    {error, Reason} ->
    %        error_logger:info_report([{char_functions, error, Reason}]),
    %        {ok, char_logged_in, {char_id, CharId}, {area_id, no_area_id}}
    %end.
    %{ok, Pid}.
    ok.

%% @private
unregister_events() ->
    %areasrv:remove_handler(char_login).
    ok.

save(Id, State) ->
    {ok, CharSrv} = application:get_env(charsrv),
    io:format("Saving player ~p at node ~p.~n", [State, CharSrv]),
    rpc:call(CharSrv, charsrv, save, [Id, State]).


