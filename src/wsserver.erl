-module(wsserver).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_info/2]).

-record(state, {
    listen_socket,
    acceptor,
    worker_options
  }).

start_link(Options) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%==========
% Behaviour API
%==========
init(Options) ->
  process_flag(trap_exit, true),

  {ok, ListenSock} = gen_tcp:listen(proplists:get_value(port, Options, 8080), [binary, {active, false}, {reuseaddr, true}, {buffer, 1000}]),
  Acceptor = acceptor:start_link(?MODULE, ListenSock),
  {ok, #state{ listen_socket = ListenSock, acceptor = Acceptor, worker_options = proplists:get_value(worker_options, Options, [])}}.

handle_info({acceptor, accept, Socket}, State) ->
  wsworker:start_link(Socket, State#state.worker_options),
  {noreply, State};

handle_info({'EXIT', _From, Reason}, State) ->
  io:format("Worker exit. Reason: ~w \n", [Reason]),
  {noreply, State}.
