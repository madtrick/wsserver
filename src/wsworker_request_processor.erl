-module(wsworker_request_processor).

-export([handle_upgrade_request/1, handle_ws_request/2]).

handle_upgrade_request(Data) ->
  {wsrequest_http:process(Data), wsrequest_ws:init()}.

handle_ws_request(Data, State) ->
  wsrequest_ws:process(Data, State).
