-module(wsserver_workers_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([new_worker/3]).

-define(CHILD(I, Type, Options), {I, {I, start_link, Options}, temporary, 5000, Type, [I]}).

start_link() ->
  supervisor:start_link(?MODULE, []).

new_worker(Supervisor, ListenSock, WorkerOptions) ->
  supervisor:start_child(Supervisor, [ListenSock, WorkerOptions]).

init([]) ->
  {ok, { {simple_one_for_one, 5, 10}, [?CHILD(wsserver_worker, worker, [])]} }.
