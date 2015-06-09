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
-module(wsserver_websocket_closing_status).

-export([handle_action/3, process_messages/2]).

handle_action(send, _, ProtocolState) ->
  {[noreply], ProtocolState};
handle_action(close, _, ProtocolState) ->
  {[noreply], ProtocolState};
handle_action(ping, _, ProtocolState) ->
  {[noreply], ProtocolState}.

process_messages(Messages, ProtocolState) ->
  process_messages(Messages, ProtocolState, []).

process_messages([], ProtocolState, Replies) ->
  {lists:reverse(Replies), ProtocolState};
process_messages([Message | Tail], ProtocolState, Replies) ->
  process_messages(Tail, ProtocolState, [process_message(Message, ProtocolState) | Replies]).

process_message(_Message = {Type, _}, _ProtocolState) when Type =/= close ->
  [];
process_message(_Message, _ProtocolState) ->
  {stop, normal}.

