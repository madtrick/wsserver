-module(acceptor).

-export([start_link/2]).
-export([loop/2]).

%===========
% PUBLIC API
%===========
start_link(SockServer, AcceptSock) ->
  spawn_link(?MODULE, loop, [SockServer, AcceptSock]).


%===========
% PROCESS API
%===========
loop(SockServer, AcceptSock) ->
  case gen_tcp:accept(AcceptSock) of
    {ok, Socket} -> SockServer ! {acceptor, accept, Socket};
    {error, Reason} -> SockServer ! {acceptor, error, Reason}
  end,
  loop(SockServer, AcceptSock).
