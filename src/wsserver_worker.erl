-module(wsserver_worker).

-export([start_link/2]).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]).
-export([send/2, handle_connection_data/2, handle_connection_close/1]).

start_link(Socket, Options) ->
  gen_server:start_link(?MODULE, [Socket, Options], []).

send(Worker, Data) ->
  gen_server:cast(Worker, {send,Data}).

handle_connection_data(Worker, Data) ->
  gen_server:cast(Worker, {connection_data, Data}).

handle_connection_close(Worker) ->
  gen_server:call(Worker, connection_close).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Socket, Options]) ->
  WorkerTCP = wsserver_worker_tcp:start_link(self(), Socket),
  {ok, wsserver_worker_state_data:new([{worker_tcp, WorkerTCP}, {callback_module, wsserver_http_callback}, {callback_module_state, wsserver_http_callback:init([])} | Options]) }.

handle_cast({send, Data}, WorkerState) ->
  send_data(Data, WorkerState);
handle_cast({connection_data, Data}, WorkerState) ->
  handle_connection_data_in_callback_module(Data, WorkerState).

handle_call(connection_close, _, WorkerState) ->
  {stop, client_connection_close, WorkerState}.

terminate(_Reason, WorkerState) ->
  wsserver_worker_tcp:stop(wsserver_worker_state_data:worker_tcp(WorkerState)),
  shutdown.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_data(Data, WorkerState) ->
  {ok, Message} = (callback_module(WorkerState)):handle_connection_out(Data, callback_module_state(WorkerState)),
  wsserver_worker_tcp:send(wsserver_worker_state_data:worker_tcp(WorkerState), Message),
  {noreply, WorkerState}.

handle_connection_data_in_callback_module(Data, WorkerState) ->
  case ((callback_module(WorkerState)):handle_connection_in(Data, callback_module_state(WorkerState))) of
    {send, Reply, new_callback_module, CallbackModule} ->
      wsserver_worker_tcp:send(wsserver_worker_state_data:worker_tcp(WorkerState), Reply),
      {
        noreply,
        update_worker_state(WorkerState, [{callback_module_state, init_callback_module(CallbackModule, WorkerState)}, {callback_module, CallbackModule}])
      };
    {send, Reply, NewCallbackModuleState} ->
      wsserver_worker_tcp:send(wsserver_worker_state_data:worker_tcp(WorkerState), Reply),
      {
        noreply,
        update_worker_state(WorkerState, [{callback_module_state, NewCallbackModuleState}])
      };
    {close, Reply, _NewCallbackModuleState} ->
      wsserver_worker_tcp:send(wsserver_worker_state_data:worker_tcp(WorkerState), Reply),
      {
        stop,
        server_connection_close
      };
    {do_nothing, NewCallbackModuleState} ->
      {
        noreply,
        update_worker_state(WorkerState, [{callback_module_state, NewCallbackModuleState}])
      }
  end.

init_callback_module(CallbackModule, WorkerState) ->
  CallbackModule:init(callback_module_options(CallbackModule, WorkerState)).

callback_module_state(WorkerState) ->
  wsserver_worker_state_data:callback_module_state(WorkerState).

callback_module(WorkerState) ->
  wsserver_worker_state_data:callback_module(WorkerState).

callback_module_options(CallbackModule, WorkerState) ->
  wsserver_worker_state_data:callback_module_options(CallbackModule, WorkerState).

update_worker_state(WorkerState, Options) ->
  wsserver_worker_state_data:update(WorkerState, Options).
