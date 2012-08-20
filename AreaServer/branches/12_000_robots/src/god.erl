%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodihn.se>
%% @copyright Christian Flodihn
%% @doc
%% This object extends the player object with additional god powers.
%% @end
%%----------------------------------------------------------------------
-module(god).

 %% @headerfile "obj.hrl"
-include("obj.hrl").

-export([
    is/3
    ]).

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

is(_From, god, State) ->
    {reply, true, State};

is(From, Other, State) ->
    apply(player, is, [From, Other, State]).

