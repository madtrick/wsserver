-module(wsserver_worker_tcp_state_data).

-export([new/1]).
-export([worker/1, socket/1]).

-record(wsserver_worker_tcp_state_data, {
    worker,
    socket
  }).

new(Options) ->
  #wsserver_worker_tcp_state_data{
    worker = proplists:get_value(worker, Options),
    socket = proplists:get_value(socket, Options)
  }.

worker(#wsserver_worker_tcp_state_data{ worker = Worker }) -> Worker.
socket(#wsserver_worker_tcp_state_data{ socket = Socket }) -> Socket.
