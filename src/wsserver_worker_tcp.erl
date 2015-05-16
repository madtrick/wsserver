%Copyright [2012] [Farruco Sanjurjo Arcay]

%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at

%       http://www.apache.org/licenses/LICENSE-2.0

%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.
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
      loop(wsserver_worker_tcp_state_data:new([{worker, Worker}, {socket, Socket}]));
    {error, closed} ->
      close;
    {error, _} ->
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
      close;
    {tcp, _Socket, Data} ->
      wsserver_worker:handle_connection_data(wsserver_worker_tcp_state_data:worker(State), Data),
      loop(State);
    {tcp_error, _Socket, _Error} ->
      die(State);
    stop ->
      shutdown
  end.

die(_) ->
  io:format("Error accepting connection. Dying", []).
