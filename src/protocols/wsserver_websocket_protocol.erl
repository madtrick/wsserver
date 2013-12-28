-module(wsserver_websocket_protocol).

-export([init/1]).
-export([handle_connection_out/2, handle_connection_in/2]).

init(Options)->
  init_handler_module(wsserver_websocket_protocol_state_data:new(Options)).

handle_connection_out(Data, _ProtocolState) ->
  {ok, wsmessage:encode(Data)}.

handle_connection_in(Data, ProtocolState) ->
  {Messages, NewProtocolState} = process_request(Data, ProtocolState),
  process_messages(Messages, NewProtocolState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_request(Data, ProtocolState) ->
  case wsserver_websocket_protocol_state_data:buffered(ProtocolState) of
    false ->
      process_decoded_messages(wsmessage:decode(Data), [], ProtocolState);
    true ->
      process_decoded_messages(wsmessage:decode(Data, wsserver_websocket_protocol_state_data:buffer(ProtocolState)), [], ProtocolState)
  end.

process_decoded_messages([], Acc, ProtocolState) ->
  {lists:reverse(Acc), ProtocolState};
process_decoded_messages([H | T], Acc, ProtocolState) ->
  process_decoded_message(H, T, Acc, ProtocolState).

process_decoded_message(Message = {Type, _}, Tail, Acc, ProtocolState) ->
  case Type of
    fragmented ->
      process_decoded_messages(Tail, [Message | Acc], buffer_message(Message, ProtocolState));
    _ ->
      process_decoded_messages(Tail, [Message | Acc], clear_buffer(ProtocolState))
end.

process_messages(Messages, ProtocolState) ->
  process_messages(Messages, ProtocolState, []).

process_messages([], ProtocolState, Replies) ->
  case Replies of
    [] ->
      {do_nothing, ProtocolState};
    [H | _]  ->
      case H of
        {close, _} ->
          {close, lists:reverse(Replies), ProtocolState};
        _ ->
        {send, lists:reverse(Replies), ProtocolState}
    end
  end;

process_messages([H | T], ProtocolState, Acc) ->
  case process_message(H, ProtocolState) of
    {noreply, NewProtocolState} ->
      process_messages(T, NewProtocolState, Acc);
    {reply, Reply, NewProtocolState} ->
      process_messages(T, NewProtocolState, [{reply, Reply} | Acc]);
    {close, Reply, NewProtocolState} ->
      process_messages([], NewProtocolState, [{close, Reply} | Acc])
  end.

process_message(Message = {Type, _Payload}, ProtocolState) when Type =:= text ; Type =:= binary->
  {ReplyType, NewHandlerState} = (handler_module(ProtocolState)):handle(Message, handler_state(ProtocolState)),
  {ReplyType, wsserver_websocket_protocol_state_data:update(ProtocolState, [{handler_state, NewHandlerState}])};

process_message({close, _Payload}, ProtocolState) ->
  {close, wsmessage:close("OK"), ProtocolState}.

handler_module(ProtocolState) ->
  wsserver_websocket_protocol_state_data:handler_module(ProtocolState).

handler_state(ProtocolState) ->
  wsserver_websocket_protocol_state_data:handler_state(ProtocolState).

buffer_message(Message, ProtocolState) ->
  wsserver_websocket_protocol_state_data:update(ProtocolState, [{buffered, true}, {buffer, Message}]).

clear_buffer(ProtocolState) ->
  wsserver_websocket_protocol_state_data:update(ProtocolState, [{buffered, false}, {buffer, undefined}]).

init_handler_module(ProtocolState) ->
  HandlerState = (handler_module(ProtocolState)):init([{worker, self()}]),
  wsserver_websocket_protocol_state_data:update(ProtocolState, [{handler_state, HandlerState}]).
