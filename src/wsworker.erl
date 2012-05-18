-module(wsworker).

-export([start_link/1]).
-export([init/1, loop/1]).

start_link(Socket) ->
  spawn_link(?MODULE, init, [Socket]).

init(Socket) ->
  loop(Socket).

loop(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} -> io:format("Received \n", []);
    {error, Reason} -> io:format("Error ~w \n", [Reason]),
      exit(kabum)
  end,
  loop(Socket).
