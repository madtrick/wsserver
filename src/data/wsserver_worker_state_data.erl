-module(wsserver_worker_state_data).

-export([new/1]).
-export([update/2]).
-export([worker_tcp/1, callback_module/1, callback_module_state/1, callback_module_options/2]).

-record(wsserver_worker_state_data, {
  worker_tcp,
  callback_module,
  callback_module_state,
  callback_modules_options
  }).

new(Options) ->
  update(#wsserver_worker_state_data{ worker_tcp = proplists:get_value(worker_tcp, Options) }, Options).

worker_tcp(#wsserver_worker_state_data{ worker_tcp = WorkerTCP }) -> WorkerTCP.
callback_module(#wsserver_worker_state_data{ callback_module = CallbackModule }) -> CallbackModule.
callback_module_state(#wsserver_worker_state_data{ callback_module_state = CallbackModuleState }) -> CallbackModuleState.
callback_modules_options(#wsserver_worker_state_data{ callback_modules_options = CallbackModulesOptions }) -> CallbackModulesOptions.
callback_module_options(CallbackModule, CallbackModuleState) ->
  proplists:get_value(CallbackModule, callback_modules_options(CallbackModuleState)).

update(WorkerState, Options) ->
  WorkerState#wsserver_worker_state_data{
    callback_module       = proplists:get_value(callback_module, Options, callback_module(WorkerState)),
    callback_module_state = proplists:get_value(callback_module_state, Options, callback_module_state(WorkerState)),
    callback_modules_options = proplists:get_value(callback_modules_options, Options, callback_modules_options(WorkerState))
  }.
