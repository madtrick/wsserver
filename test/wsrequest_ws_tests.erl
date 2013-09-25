-module(wsrequest_ws_tests).
-include("wsrequest_ws.hrl").
-include_lib("wsock/include/wsock.hrl").
-include_lib("eunit/include/eunit.hrl").

when_initialized_returns_a_blank_state_test_() ->
  State = wsrequest_ws:init(),
  ?_assertEqual(State, #wsrequest_ws_state{}).

processes_unfragmented_message_test_() ->
  {setup,
    fun() ->
        meck:new(wsmessage),
        meck:expect(wsmessage, decode, 1, [{text, stub_text}])
    end,
    fun(_) ->
        meck:unload(wsmessage)
    end,
    fun(_) ->
        State = #wsrequest_ws_state{},
        {[{Kind, Payload}], NewState} = wsrequest_ws:process(data, State),
        [
          ?_assertEqual(Kind, text),
          ?_assertEqual(Payload, stub_text),
          ?_assertEqual(NewState, State)
        ]
    end}.

processes_fragmented_message_test_() ->
  {setup,
    fun() ->
        meck:new(wsmessage),
        meck:expect(wsmessage, decode, 1, [{fragmented, fragmented_message}])
    end,
    fun(_) ->
        meck:unload(wsmessage)
    end
    ,
    fun(_) ->
        {Messages, NewState} = wsrequest_ws:process(data, #wsrequest_ws_state{}),
        NewBuffer = NewState#wsrequest_ws_state.buffered,
        [
          ?_assertEqual(Messages, []),
          ?_assertEqual(NewState#wsrequest_ws_state.buffered, fragmented_message)
        ]
    end}.

attemps_to_complete_a_fragmented_message_test_() ->
  {setup,
    fun() ->
        meck:new(wsmessage),
        meck:expect(wsmessage, decode, [data, fragmented_message], [{text, message}])
    end,
    fun(_) ->
        meck:unload(wsmessage)
    end,
    fun(_) ->
        {[Message], NewState} = wsrequest_ws:process(data, #wsrequest_ws_state{ buffered = fragmented_message }),
        NewBuffer = NewState#wsrequest_ws_state.buffered,
        [
          ?_assertEqual(Message, {text, message}),
          ?_assertEqual(NewBuffer, undefined)
        ]
    end}.
