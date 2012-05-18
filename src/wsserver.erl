-module(wsserver).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_info/2]).

-record(state, {
    listen_socket,
    acceptor
  }).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%==========
% Behaviour API
%==========
init(_Args) ->
  process_flag(trap_exit, true),

  {ok, ListenSock} = gen_tcp:listen(8081, [binary, {active, false}]),
  Acceptor = acceptor:start_link(?MODULE, ListenSock),
  {ok, #state{ listen_socket = ListenSock, acceptor = Acceptor}}.

handle_info({acceptor, accept, Socket}, State) ->
  wsworker:start_link(Socket),
  {noreply, State};

handle_info({'EXIT', _From, _Reason}, State) ->
  io:format("Worker exit \n", []),
  {noreply, State}.
