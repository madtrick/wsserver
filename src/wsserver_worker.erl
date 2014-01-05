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
-module(wsserver_worker).

-export([start_link/2]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]).
-export([handle_connection_data/2, handle_connection_close/1]).

start_link(Socket, Options) ->
  gen_server:start_link(?MODULE, [Socket, Options], []).

handle_connection_data(Worker, Data) ->
  gen_server:cast(Worker, {connection_data, Data}).

handle_connection_close(Worker) ->
  gen_server:call(Worker, connection_close).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Socket, Options]) ->
  WorkerTCP = wsserver_worker_tcp:start_link(self(), Socket),
  {ok, wsserver_worker_state_data:new([{worker_tcp, WorkerTCP}, {protocol_module, wsserver_http_protocol}, {protocol_module_state, wsserver_http_protocol:init([])} | Options]) }.

handle_cast({protocol_action, Action, Options}, WorkerState) ->
  handle_action_in_protocol_module(Action, Options, WorkerState);
handle_cast({connection_data, Data}, WorkerState) ->
  handle_connection_data_in_protocol_module(Data, WorkerState).

handle_call(connection_close, _, WorkerState) ->
  {stop, client_connection_close, WorkerState}.

terminate(_Reason, WorkerState) ->
  wsserver_worker_tcp:stop(wsserver_worker_state_data:worker_tcp(WorkerState)),
  shutdown.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_action_in_protocol_module(Action, Options, WorkerState) ->
  evaluate_in_protocol_module(handle_action, [Action | Options], WorkerState).

handle_connection_data_in_protocol_module(Data, WorkerState) ->
  evaluate_in_protocol_module(handle_connection_in, Data, WorkerState).

evaluate_in_protocol_module(Function, Options, WorkerState) ->
  case evaluate_protocol_function(Function, Options, WorkerState) of
    {Responses, NewProtocolModuleState} ->
      evaluate_protocol_responses(
        Responses,
        update_worker_state(WorkerState, [{protocol_module_state, NewProtocolModuleState}])
      );
    {Responses, new_protocol_module, ProtocolModule} ->
      evaluate_protocol_responses(
        Responses,
        update_worker_state(WorkerState, [
            {protocol_module_state, init_protocol_module(ProtocolModule, WorkerState)},
            {protocol_module, ProtocolModule}
          ])
      )
  end.

evaluate_protocol_function(Function, Options, WorkerState) ->
  ((protocol_module(WorkerState)):Function(Options, protocol_module_state(WorkerState))).

evaluate_protocol_responses([], WorkerState) ->
  {noreply, WorkerState};
evaluate_protocol_responses([Response | Tail], WorkerState) ->
  case evaluate_protocol_response(Response, WorkerState) of
    noreply ->
      evaluate_protocol_responses(Tail, WorkerState);
    {stop, Reason} ->
      {stop, Reason, WorkerState}
  end.

evaluate_protocol_response(Response, WorkerState) ->
  case Response of
    {reply, Reply} ->
      wsserver_worker_tcp:send(wsserver_worker_state_data:worker_tcp(WorkerState), Reply),
      noreply;
    {close, Reply} ->
      wsserver_worker_tcp:send(wsserver_worker_state_data:worker_tcp(WorkerState), Reply),
      {stop, server_connection_close};
    noreply ->
        noreply;
    {stop, Reason} ->
      {stop, Reason}
  end.

init_protocol_module(ProtocolModule, WorkerState) ->
  ProtocolModule:init(protocol_module_options(ProtocolModule, WorkerState)).

protocol_module_state(WorkerState) ->
  wsserver_worker_state_data:protocol_module_state(WorkerState).

protocol_module(WorkerState) ->
  wsserver_worker_state_data:protocol_module(WorkerState).

protocol_module_options(ProtocolModule, WorkerState) ->
  wsserver_worker_state_data:protocol_module_options(ProtocolModule, WorkerState).

update_worker_state(WorkerState, Options) ->
  wsserver_worker_state_data:update(WorkerState, Options).
