-module(wsserver_state_data).

-export([new/1]).
-export([update/2]).
-export([listen_socket/1, worker_options/1, workers/1]).

-record(wsserver_state_data, {
    listen_socket,
    acceptor,
    worker_options,
    workers
  }).

new(Options) ->
  update(#wsserver_state_data{}, Options).

listen_socket(#wsserver_state_data{ listen_socket = ListenSocket }) -> ListenSocket.
worker_options(#wsserver_state_data{ worker_options = WorkerOptions }) -> WorkerOptions.
workers(#wsserver_state_data{ workers = Workers }) -> Workers.

update(WSServerStateData, Options) ->
  WSServerStateData#wsserver_state_data{
    listen_socket  = proplists:get_value(listen_socket, Options, listen_socket(WSServerStateData)),
    worker_options = proplists:get_value(worker_options, Options, worker_options(WSServerStateData)),
    workers        = proplists:get_value(workers, Options, workers(WSServerStateData))
  }.
