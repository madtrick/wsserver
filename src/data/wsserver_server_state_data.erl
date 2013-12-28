-module(wsserver_server_state_data).

-export([new/1]).
-export([update/2]).
-export([listen_socket/1, worker_options/1, workers_sup/1]).

-record(wsserver_state_data, {
    listen_socket,
    acceptor,
    worker_options,
    workers_sup
  }).

new(Options) ->
  update(#wsserver_state_data{}, Options).

listen_socket(#wsserver_state_data{ listen_socket = ListenSocket }) -> ListenSocket.
worker_options(#wsserver_state_data{ worker_options = WorkerOptions }) -> WorkerOptions.
workers_sup(#wsserver_state_data{ workers_sup = WorkersSup }) -> WorkersSup.

update(WSServerStateData, Options) ->
  WSServerStateData#wsserver_state_data{
    listen_socket  = proplists:get_value(listen_socket, Options, listen_socket(WSServerStateData)),
    worker_options = proplists:get_value(worker_options, Options, worker_options(WSServerStateData)),
    workers_sup    = proplists:get_value(workers_sup, Options, workers_sup(WSServerStateData))
  }.
