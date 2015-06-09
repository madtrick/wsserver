-module(wsserver_mock_handler).
-export([init/1, handle/2]).

init(_) ->
  ok.

handle(_, _) ->
  {noreply, ok}.
