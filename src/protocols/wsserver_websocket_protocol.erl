%Copyright [2012] [Farruco Sanjurjo Arcay]

%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at

%       http://www.apache.org/licenses/LICENSE-2.0

%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.
-module(wsserver_websocket_protocol).

-export([init/1]).
-export([
  handle_action/2,
  handle_connection_in/2,
  handle_connection_close/2
]).

init(Options)->
  init_handler_module(wsserver_websocket_protocol_state_data:new([{status_module, wsserver_websocket_open_status} | Options])).

handle_action([send | [Data]], ProtocolState) ->
  handle_action_in_status_module(send, Data, ProtocolState);
handle_action([close | [Data]], ProtocolState) ->
  handle_action_in_status_module(close, Data, ProtocolState);
handle_action([ping | [Data]], ProtocolState) ->
  handle_action_in_status_module(ping, Data, ProtocolState).

handle_connection_in(Data, ProtocolState) ->
  {Messages, NewProtocolState} = process_request(Data, ProtocolState),
  process_messages(Messages, NewProtocolState).

handle_connection_close(_, ProtocolState) ->
  HandlerModule = wsserver_websocket_protocol_state_data:handler_module(ProtocolState),
  HandlerState  = wsserver_websocket_protocol_state_data:handler_state(ProtocolState),
  HandlerModule:handle(connection_close, HandlerState),
  {[{stop, normal}], ProtocolState}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_action_in_status_module(Action, Options, ProtocolState) ->
  (status_module(ProtocolState)):handle_action(Action, Options, ProtocolState).

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
      process_decoded_messages(Tail, Acc, buffer_message(Message, ProtocolState));
    _ ->
      process_decoded_messages(Tail, [Message | Acc], clear_buffer(ProtocolState))
end.

process_messages(Messages, ProtocolState) ->
  process_messages_in_status_module(Messages, ProtocolState).

process_messages_in_status_module(Messages, ProtocolState) ->
  (status_module(ProtocolState)):process_messages(Messages, ProtocolState).

buffer_message(Message, ProtocolState) ->
  wsserver_websocket_protocol_state_data:update(ProtocolState, [{buffered, true}, {buffer, Message}]).

clear_buffer(ProtocolState) ->
  wsserver_websocket_protocol_state_data:update(ProtocolState, [{buffered, false}, {buffer, undefined}]).

status_module(ProtocolState) ->
  wsserver_websocket_protocol_state_data:status_module(ProtocolState).

init_handler_module(ProtocolState) ->
  HandlerState = (handler_module(ProtocolState)):init([{worker, self()}]),
  wsserver_websocket_protocol_state_data:update(ProtocolState, [{handler_state, HandlerState}]).

handler_module(ProtocolState) ->
  wsserver_websocket_protocol_state_data:handler_module(ProtocolState).
