-module(wsworker_socket).
-behaviour(gen_server).

-export([start_link/1, add_worker/2, send/2]).
-export([init/1, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    worker,
    socket
  }).

start_link(Socket) ->
  gen_server:start_link(?MODULE, Socket, []).

send(WorkerSocket, Data) ->
  gen_server:cast(WorkerSocket, {send, Data}).

add_worker(WorkerSocket, Worker) ->
  gen_server:cast(WorkerSocket, {add_worker, Worker}).

init(Socket) ->
  {ok, #state{ socket = Socket }}.

handle_info({tcp, _Socket, Data}, State) ->
  wsworker:process(State#state.worker, Data),
  inet:setopts(State#state.socket, [{active, once}]),
  {noreply, State};

handle_info({tcp_closed, _}, State) ->
  {stop, tcp_closed, State};

handle_info({tcp_error, _, _Error}, State) ->
  {stop, tcp_error, State}.

handle_cast({add_worker, Worker}, State) ->
  % Start receiving only where there's a worker
  % or the wsworker:process call will fail
  inet:setopts(State#state.socket, [{active, once}]),
  {noreply, State#state{ worker = Worker }};

handle_cast({send, Data}, State) ->
  gen_tcp:send(State#state.socket, Data),
  {noreply, State}.

terminate(_Reason, State) ->
  stop_worker(State#state.worker),
  die.

stop_worker(Worker) ->
  wsworker:stop(Worker).
