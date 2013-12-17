-module(wsworker_handler).
-include("wsworker.hrl").
-export([init/1, handle/2]).

-record(state,{}).

init(_) ->
  #state{}.

handle({text, Message}, State) ->
  io:format("Received: ~s ~n", [Message]),
  {noreply, State}.

