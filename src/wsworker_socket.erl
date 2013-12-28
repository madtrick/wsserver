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
