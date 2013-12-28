-module(wsserver_websocket_callback).

-export([init/1]).
-export([handle_connection_out/2, handle_connection_in/2]).

init(Options)->
  init_handler_module(wsserver_websocket_callback_state_data:new(Options)).

handle_connection_out(Data, CallbackState) ->
  {ok, wsmessage:encode(Data)}.

handle_connection_in(Data, CallbackState) ->
  {Messages, NewCallbackState} = process_request(Data, CallbackState),
  process_messages(Messages, NewCallbackState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_request(Data, CallbackState) ->
  case wsserver_websocket_callback_state_data:buffered(CallbackState) of
    false ->
      process_decoded_messages(wsmessage:decode(Data), [], CallbackState);
    true ->
      process_decoded_messages(wsmessage:decode(Data, wsserver_websocket_callback_state_data:buffer(CallbackState)), [], CallbackState)
  end.

process_decoded_messages([], Acc, CallbackState) ->
  {lists:reverse(Acc), CallbackState};
process_decoded_messages([H | T], Acc, CallbackState) ->
  process_decoded_message(H, T, Acc, CallbackState).

process_decoded_message(Message = {Type, _}, Tail, Acc, CallbackState) ->
  case Type of
    fragmented ->
      process_decoded_messages(Tail, [Message | Acc], buffer_message(Message, CallbackState));
    _ ->
      process_decoded_messages(Tail, [Message | Acc], clear_buffer(CallbackState))
end.

process_messages(Messages, CallbackState) ->
  process_messages(Messages, CallbackState, []).

process_messages([], CallbackState, Replies) ->
  case Replies of
    [] ->
      {do_nothing, CallbackState};
    [H | _]  ->
      case H of
        {close, _} ->
          {close, lists:reverse(Replies), CallbackState};
        _ ->
        {send, lists:reverse(Replies), CallbackState}
    end
  end;

process_messages([H | T], CallbackState, Acc) ->
  case process_message(H, CallbackState) of
    {noreply, NewCallbackState} ->
      process_messages(T, NewCallbackState, Acc);
    {reply, Reply, NewCallbackState} ->
      process_messages(T, NewCallbackState, [{reply, Reply} | Acc]);
    {close, Reply, NewCallbackState} ->
      process_messages([], NewCallbackState, [{close, Reply} | Acc])
  end.

process_message(Message = {Type, _Payload}, CallbackState) when Type =:= text ; Type =:= binary->
  {ReplyType, NewHandlerState} = (handler_module(CallbackState)):handle(Message, handler_state(CallbackState)),
  {ReplyType, wsserver_websocket_callback_state_data:update(CallbackState, [{handler_state, NewHandlerState}])};

process_message({close, _Payload}, CallbackState) ->
  {close, wsmessage:close("OK"), CallbackState}.

handler_module(CallbackState) ->
  wsserver_websocket_callback_state_data:handler_module(CallbackState).

handler_state(CallbackState) ->
  wsserver_websocket_callback_state_data:handler_state(CallbackState).

buffer_message(Message, CallbackState) ->
  wsserver_websocket_callback_state_data:update(CallbackState, [{buffered, true}, {buffer, Message}]).

clear_buffer(CallbackState) ->
  wsserver_websocket_callback_state_data:update(CallbackState, [{buffered, false}, {buffer, undefined}]).

init_handler_module(CallbackState) ->
  HandlerState = (handler_module(CallbackState)):init([{worker, self()}]),
  wsserver_websocket_callback_state_data:update(CallbackState, [{handler_state, HandlerState}]).
