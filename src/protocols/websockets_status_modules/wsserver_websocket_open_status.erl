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
-module(wsserver_websocket_open_status).
-include("websockets.hrl").

-export([handle_action/3, process_messages/2]).

handle_action(send, Data, ProtocolState) ->
  {[{reply, wsmessage:encode(Data)}], ProtocolState};
handle_action(close, {StatusCode, ClosePayload}, ProtocolState) ->
  {[{reply, wsmessage:close({StatusCode, ClosePayload})}], update_protocol_state(ProtocolState, [{status_module, wsserver_websocket_closing_status}])};
handle_action(ping, Data, ProtocolState) ->
  {[{reply, wsmessage:ping(Data)}], ProtocolState}.


process_messages(Messages, ProtocolState) ->
  process_messages(Messages, ProtocolState, []).

process_messages([], ProtocolState, Replies) ->
  {lists:reverse(Replies), ProtocolState};

process_messages([H | T], ProtocolState, Acc) ->
  case process_message(H, ProtocolState) of
    {noreply, NewProtocolState} ->
      process_messages(T, NewProtocolState, Acc);
    {reply, Message, NewProtocolState} ->
      process_messages(T, NewProtocolState, [{reply, Message} | Acc]);
    {close, Message, NewProtocolState} ->
      process_messages([], NewProtocolState, [{close, Message} | Acc])
  end.

process_message(Message = {Type, _}, ProtocolState) ->
  Response = call_handler_with_message(Message, ProtocolState),

  build_reply_for_message(
    Type,
    extract_handler_reply_from_handler_response(Response),
    update_handler_state_in_protocol_state(ProtocolState, extract_handler_state_from_handler_response(Response))
  ).


build_reply_for_message(MessageType, Reply, ProtocolState) ->
  case {MessageType, Reply} of
    {close, close} ->
      {close, wsmessage:close({?NORMAL_CLOSURE_CODE, ?NORMAL_CLOSURE_REASON}), ProtocolState};

    {close, {close, Status, ClosePayload}} ->
      {close, wsmessage:close({Status, ClosePayload}), ProtocolState};

    {_, close} ->
      {reply, wsmessage:close(), update_protocol_state(ProtocolState, [{status_module, wsserver_websocket_closing_status}])};

    {_, {close, Status, ClosePayload}} ->
      {reply, wsmessage:close(Status, ClosePayload), update_protocol_state(ProtocolState, [{status_module, wsserver_websocket_closing_status}])};

    {_, ping} ->
      {reply, wsmessage:ping([]), ProtocolState};

    {_, {ping, ReplyPayload}} ->
      {reply, wsmessage:ping(ReplyPayload), ProtocolState};

    {_, pong} ->
      {reply, wsmessage:pong([]), ProtocolState};

    {_, {pong, ReplyPayload}} ->
      {reply, wsmessage:pong(ReplyPayload), ProtocolState};

    {_, {reply, ReplyPayload}} ->
      {reply, wsmessage:encode(ReplyPayload), ProtocolState};

    {_, noreply} ->
      {noreply, ProtocolState}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

call_handler_with_message(Message, ProtocolState) ->
  (handler_module(ProtocolState)):handle(Message, handler_state(ProtocolState)).

extract_handler_state_from_handler_response(Response) ->
  case Response of
    {_, State} -> State;
    {_, _, State} -> State;
    {_, _, _, State} -> State
  end.

extract_handler_reply_from_handler_response(Response) ->
  case Response of
    {A, _} -> A;
    {A, B, _} -> {A, B};
    {A, B, C, _} -> {A, B, C}
  end.

handler_module(ProtocolState) ->
  wsserver_websocket_protocol_state_data:handler_module(ProtocolState).

handler_state(ProtocolState) ->
  wsserver_websocket_protocol_state_data:handler_state(ProtocolState).

update_protocol_state(ProtocolState, Options) ->
  wsserver_websocket_protocol_state_data:update(ProtocolState, Options).

update_handler_state_in_protocol_state(ProtocolState, NewHandlerState) ->
  update_protocol_state(ProtocolState, [{handler_state, NewHandlerState}]).
