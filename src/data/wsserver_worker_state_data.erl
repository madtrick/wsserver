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
