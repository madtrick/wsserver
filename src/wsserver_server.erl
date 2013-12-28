-module(wsserver_server).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_info/2]).

start_link(Options) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%==========
% Behaviour API
%==========

init(Options) ->
  {ok,  WorkersSup} = wsserver_workers_sup:start_link(),
  {ok, ListenSock} = gen_tcp:listen(proplists:get_value(port, Options, 8080), [binary, {active, false}, {reuseaddr, true}, {buffer, 1000}]),

  NumberOfWorkers  = proplists:get_value(number_of_workers, Options),
  WorkerOptions    = proplists:get_value(worker_options, Options),

  WSServerState = wsserver_server_state_data:new([{listen_socket, ListenSock}, {worker_options, WorkerOptions}, {workers_sup, WorkersSup}]),

  [ monitor_worker(start_worker(ListenSock, WorkerOptions, WorkersSup)) || _ <- lists:seq(1, NumberOfWorkers) ],

  {ok, WSServerState}.

handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info}, WSServerState) ->
  monitor_worker(start_worker(listen_socket(WSServerState), worker_options(WSServerState), workers_sup(WSServerState))),
  {noreply, WSServerState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_worker(ListenSock, WorkerOptions, WorkersSup) ->
  {ok, Worker} = wsserver_workers_sup:new_worker(WorkersSup, ListenSock, WorkerOptions),
  Worker.

monitor_worker(Worker) ->
  erlang:monitor(process, Worker).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

worker_options(WSServerState) ->
  wsserver_server_state_data:worker_options(WSServerState).

listen_socket(WSServerState) ->
  wsserver_server_state_data:listen_socket(WSServerState).

workers_sup(WSServerState) ->
  wsserver_server_state_data:workers_sup(WSServerState).