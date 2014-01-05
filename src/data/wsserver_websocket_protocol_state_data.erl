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
-module(wsserver_websocket_protocol_state_data).

-export([new/1]).
-export([update/2]).
-export([buffered/1, buffer/1, handler_module/1, handler_state/1, status_module/1]).

-record(wsserver_websocket_protocol_state_data, {
    buffered,
    buffer,
    handler_module,
    handler_state,
    status_module
  }).

new(Options) ->
  update(#wsserver_websocket_protocol_state_data{ buffered = false }, Options).

buffered(#wsserver_websocket_protocol_state_data{ buffered = Buffered }) -> Buffered.
buffer(#wsserver_websocket_protocol_state_data{ buffer = Buffer }) -> Buffer.
handler_module(#wsserver_websocket_protocol_state_data{ handler_module = HandlerModule }) -> HandlerModule.
handler_state(#wsserver_websocket_protocol_state_data{ handler_state = HandlerState }) -> HandlerState.
status_module(#wsserver_websocket_protocol_state_data{ status_module = StatusModule }) -> StatusModule.

update(ProtocolState, Options) ->
  ProtocolState#wsserver_websocket_protocol_state_data{
    buffered       = proplists:get_value(buffered, Options, buffered(ProtocolState)),
    buffer         = proplists:get_value(buffer, Options, buffer(ProtocolState)),
    handler_module = proplists:get_value(handler_module, Options, handler_module(ProtocolState)),
    handler_state  = proplists:get_value(handler_state, Options, handler_state(ProtocolState)),
    status_module         = proplists:get_value(status_module, Options, status_module(ProtocolState))
  }.
