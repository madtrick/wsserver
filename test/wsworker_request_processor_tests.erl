-module(wsworker_request_processor_tests).
-include_lib("eunit/include/eunit.hrl").

handle_upgrade_request_test_() ->
  {setup,
    fun() ->
        meck:new(wsrequest_http),
        meck:expect(wsrequest_http, process, [data], response)
    end,
    fun(_) ->
        meck:unload(wsrequest_http)
    end,
    fun(_) ->
        {Response, ProcessorState} = wsworker_request_processor:handle_upgrade_request(data),
        [
          ?_assertEqual(meck:validate([wsrequest_http]), true),
          ?_assertEqual(Response, response),
          ?_assertEqual(ProcessorState, wsrequest_ws:init())
        ]
    end}.

handle_ws_request_test_() ->
  {setup,
    fun() ->
        meck:new(wsrequest_ws),
        meck:expect(wsrequest_ws, process, [data, state], {[message], new_state})
    end,
    fun(_) ->
        meck:unload(wsrequest_ws)
    end,
    fun(_) ->
        {Messages, NewState} = wsworker_request_processor:handle_ws_request(data, state),
        [
          ?_assertEqual(meck:validate([wsrequest_ws]), true),
          ?_assertEqual(Messages, [message]),
          ?_assertEqual(NewState, new_state)
        ]
    end}.
