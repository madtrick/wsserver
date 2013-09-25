-module(wsworker_handler_tests).

%-include("wsworker.hrl").
%-include_lib("eunit/include/eunit.hrl").

%handles_complete_messages_test_() ->
%  BeforeEach = fun() -> %setup
%        meck:new(wsock_message),
%        meck:expect(wsock_message, decode, 2, [#message{ type = text, payload = "Lorem ipsum" }]),

%        meck:new(fake_module),
%        meck:expect(fake_module, handle, 2, {no_reply, new_state, response})
%    end,

%  AfterEach = fun(_) -> %cleanup
%        meck:unload(wsock_message),
%        meck:unload(fake_module)
%    end,

%  {setup, BeforeEach, AfterEach,
%    fun (_) ->
%        {_, NewHandlerModuleState, Response} = wsworker_handler:handle(#state{ status = ?OPEN }, mock_data),
%        [
%          ?_assertEqual(NewHandlerModuleState, new_state_2),
%          ?_assertEqual(Response, response)
%        ]
%    end
%      }.
