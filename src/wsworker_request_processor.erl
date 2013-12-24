-module(wsworker_request_processor).

-export([handle_upgrade_request/1, handle_ws_request/2]).

handle_upgrade_request(Data) ->
  case wsrequest_http:process(Data) of
    {incomplete, D} -> {incomplete, D};
    {ok, Reply} -> {reply, Reply, wsrequest_ws:init()}
  end.

handle_ws_request(Data, State) ->
  wsrequest_ws:process(Data, State).
