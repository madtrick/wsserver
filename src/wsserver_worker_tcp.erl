-module(wsserver_worker_tcp).

-export([start_link/2, stop/1]).
-export([accept/2, send/2]).

start_link(Worker, ListenSocket) ->
  proc_lib:spawn_link(?MODULE, accept, [Worker, ListenSocket]).

stop(WorkerTCP) ->
  WorkerTCP ! stop.

send(WorkerTCP, Data) ->
  WorkerTCP ! {send, Data}.

accept(Worker, ListenSocket) ->
  case gen_tcp:accept(ListenSocket) of
    {ok, Socket} ->
      io:format("Accepted ~w \n", [self()]),
      loop(wsserver_worker_tcp_state_data:new([{worker, Worker}, {socket, Socket}]));
    {error, _Reason} ->
      die("")
  end.

loop(State) ->
  inet:setopts(wsserver_worker_tcp_state_data:socket(State), [{active, once}]),

  receive
    {send, Data} ->
      ok = gen_tcp:send(wsserver_worker_tcp_state_data:socket(State), Data),
      loop(State);
    {tcp_closed, _} ->
      wsserver_worker:handle_connection_close(wsserver_worker_tcp_state_data:worker(State)),
      close(State);
    {tcp, _Socket, Data} ->
      wsserver_worker:handle_connection_data(wsserver_worker_tcp_state_data:worker(State), Data),
      loop(State);
    {tcp_error, _Socket, _Error} ->
      die(State);
    stop ->
      shutdown
  end.

close(_) ->
  io:format("Closing connection").

die(_) ->
  io:format("Error accepting connection. Dying", []).
