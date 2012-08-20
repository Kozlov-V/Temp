-module(client_listener).

-include("protocol.hrl").

-export([
    start/2
    ]).

% Internal exports
-export([
    %recv_header/2
    wait_for_data/2
    ]).

start(Receiver, Socket) ->
    error_logger:info_report([{starting_receiver_process}]),
    %connection:socket_send(Socket, ?CONN_ESTABLISHED),
    spawn_link(?MODULE, wait_for_data, [Receiver, Socket]).

wait_for_data(Receiver, Socket) ->
    error_logger:info_report([{wait_for_data, Socket, Receiver}]),
    case gen_tcp:recv(Socket, 2, 2000) of
        {ok, <<HeaderSize:16/integer>>} ->
            error_logger:info_report([{got_header_size, HeaderSize}]),
            case gen_tcp:recv(Socket, HeaderSize) of 
                {ok, Data} ->
                    error_logger:info_report([{got_data, Data}]),
                    Receiver ! {client_data, Data},
                    wait_for_data(Receiver, Socket);
                DataError ->
                    error_logger:error_report([tcp_data_error, DataError])
            end;
        HeaderError ->
            error_logger:error_report([tcp_header_error, HeaderError,
                Receiver, Socket])
    end.