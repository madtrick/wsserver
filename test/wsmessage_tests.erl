-module(wsmessage_tests).
-include_lib("eunit/include/eunit.hrl").

encodes_data_as_text_test_() ->
  {setup,
    fun() ->
        meck:new(wsock_message),
        meck:expect(wsock_message, encode, 2, encoded_message)
    end,
    fun(_) ->
        meck:unload(wsock_message)
    end,
    fun(_) ->
        Message = wsmessage:encode(data),
        [
          ?_assertEqual(meck:called(wsock_message, encode, [data, [text]]), true),
          ?_assertEqual(Message, encoded_message)
        ]
    end}.
