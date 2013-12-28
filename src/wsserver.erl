-module(wsserver).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_info/2]).

start_link(Options) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%==========
% Behaviour API
%==========

init(Options) ->
  process_flag(trap_exit, true),

  {ok, ListenSock} = gen_tcp:listen(proplists:get_value(port, Options, 8080), [binary, {active, false}, {reuseaddr, true}, {buffer, 1000}]),

  NumberOfWorkers = proplists:get_value(number_of_workers, Options),
  WorkerOptions   = proplists:get_value(worker_options, Options),
  Workers         = [ wsserver_worker:start_link(ListenSock, WorkerOptions) || _ <- lists:seq(1, NumberOfWorkers) ],
  {ok, wsserver_state_data:new([{listen_socket, ListenSock}, {workers, Workers}, {worker_options, WorkerOptions}])}.

handle_info({'EXIT', _From, Reason}, State) ->
  {noreply, State}.
