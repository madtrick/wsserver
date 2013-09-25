-module(wsworker_handler).
-include("wsworker.hrl").
-export([init/0, handle/2]).

-record(state,{}).

init() ->
  #state{}.

handle(State, {text, Message}) ->
  io:format("Received: ~s ~n", [Message]),
  {noreply, State}.

