-module(libtree.mnesia_quads).

-import(error_logger).
-import(mnesia).
-import(lists).
-import(ets).
-import(io).

-import(util).
-import(obj).

-include("vec.hrl").

-export([
    init/0   
    ]).

-export([
    info/1,
    increase/1,
    decrease/1,
    assign/4,
    event/5
    ]).

% Internal exports
-export([
    send_message/4
    ]).


-record(state, {area_size=10000, tree_size=1, quad_size=10000, quads=1}).

-record(obj, {id, pid}).

init() ->
    mnesia:start(),
    State = #state{},
    increase(State).

info(#state{area_size=AreaSize, quad_size=QuadSize, quads=Quads}) ->
    io:format("~nAreaSize: ~p.~nQuadSize: ~p.~nQuads: ~p.~n",
        [AreaSize, QuadSize, Quads]).

increase(#state{area_size=AreaSize, tree_size=TreeSize} = State) ->
    NewTreeSize = TreeSize * 2,
    QuadSize = AreaSize / NewTreeSize,
    build_tree(NewTreeSize, QuadSize),
    {ok, State#state{tree_size=NewTreeSize, quad_size=QuadSize}}.

decrease(State) ->
    {ok, State}.

build_tree(TreeSize, QuadSize) ->
    build_tree(1, 1, TreeSize, TreeSize, QuadSize).

build_tree(MaxRow, MaxCol, MaxRow, MaxCol, QuadSize) ->
    %{ok, Pid} = quad:start({MaxRow, MaxCol}, QuadSize),
    %ets:insert(TableId, {{MaxRow, MaxCol}, Pid});
    %error_logger:info_report([{creating_quad, MaxRow, MaxCol}]),
    create_quad(MaxRow, MaxCol);

build_tree(MaxRow, Col, MaxRow, MaxCol, QuadSize) ->
    %error_logger:info_report([{creating_quad, MaxRow, Col}]),
    %create_quad(MaxRow, Col, QuadSize, TableId),
    create_quad(MaxRow, Col),
    build_tree(1, Col + 1, MaxRow, MaxCol, QuadSize);

build_tree(Row, Col, MaxRow, MaxCol, QuadSize) ->
    %create_quad(Row, Col, QuadSize, TableId),
    %error_logger:info_report([{creating_quad, Row, Col}]),
    create_quad(Row, Col),
    build_tree(Row + 1, Col, MaxRow, MaxCol, QuadSize).

create_quad(Row, Col) ->
    Name = get_name(Row, Col),
    case lists:member(Name, mnesia:system_info(tables)) of
        true ->
            % Add connecting to existing table later
            error_logger:info_report([{mnesia, table_existing, Name}]),
            pass;
        false ->
            error_logger:info_report([{mnesia, creating_table, Name}]),
            mnesia:create_table(Name, [
                {ram_copies, [node()]}, 
                {record_name, obj},
                {attributes, record_info(fields, obj)}])
    end.

assign(Id, Obj, Pos, #state{quad_size=QuadSize}) ->
    Row = util:ceiling(Pos#vec.x/QuadSize),
    Col = util:ceiling(Pos#vec.z/QuadSize),
    Name = get_name(Row, Col),
    %error_logger:info_report([{write_quad, Name, Id, Obj}]),
    mnesia:dirty_write(Name, #obj{id=Id, pid=Obj}),
    Name.

event(From, Quad, Fun, Args, _TreeState) ->
    spawn(?MODULE, send_message, [From, Quad, Fun, Args]).

get_name(Row, Col) ->
    list_to_atom(integer_to_list(Row) ++ "_" ++ integer_to_list(Col)).

send_message(From, Quad, Fun, Args) ->
    FirstKey = mnesia:dirty_first(Quad),
    send_message(From, Quad, Fun, Args, FirstKey).

send_message(_From, _Quad, _Fun, _Args, '$end_of_table') ->
    done;

send_message(From, Quad, Fun, Args, Key) ->
    [Obj] = mnesia:dirty_read(Quad, Key),
    %error_logger:info_report([{send_message, Obj}]),
    obj:async_call(Obj#obj.pid, Fun, Args),
    NextKey = mnesia:dirty_next(Quad, Key),
    send_message(From, Quad, Fun, Args, NextKey).
