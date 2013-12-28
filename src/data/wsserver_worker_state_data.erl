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
-module(wsserver_worker_state_data).

-export([new/1]).
-export([update/2]).
-export([worker_tcp/1, protocol_module/1, protocol_module_state/1, protocol_module_options/2]).

-record(wsserver_worker_state_data, {
  worker_tcp,
  protocol_module,
  protocol_module_state,
  protocol_modules_options
  }).

new(Options) ->
  update(#wsserver_worker_state_data{ worker_tcp = proplists:get_value(worker_tcp, Options) }, Options).

worker_tcp(#wsserver_worker_state_data{ worker_tcp = WorkerTCP }) -> WorkerTCP.
protocol_module(#wsserver_worker_state_data{ protocol_module = ProtocolModule }) -> ProtocolModule.
protocol_module_state(#wsserver_worker_state_data{ protocol_module_state = ProtocolModuleState }) -> ProtocolModuleState.
protocol_modules_options(#wsserver_worker_state_data{ protocol_modules_options = ProtocolModulesOptions }) -> ProtocolModulesOptions.
protocol_module_options(ProtocolModule, ProtocolModuleState) ->
  proplists:get_value(ProtocolModule, protocol_modules_options(ProtocolModuleState)).

update(WorkerState, Options) ->
  WorkerState#wsserver_worker_state_data{
    protocol_module       = proplists:get_value(protocol_module, Options, protocol_module(WorkerState)),
    protocol_module_state = proplists:get_value(protocol_module_state, Options, protocol_module_state(WorkerState)),
    protocol_modules_options = proplists:get_value(protocol_modules_options, Options, protocol_modules_options(WorkerState))
  }.
