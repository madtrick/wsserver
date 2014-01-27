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
-module(wsserver_http_protocol).
-include_lib("wsock/include/wsock.hrl").

-export([init/1]).
-export([handle_connection_in/2]).

init(_Options) ->
  wsserver_http_protocol_state_data:new().

handle_connection_in(Data, ProtocolState) ->
  Buffer  = wsserver_http_protocol_state_data:buffer(ProtocolState),
  Request = <<Buffer/binary, Data/binary>>,

  case process_request(Request) of
    incomplete ->
      {[noreply], wsserver_http_protocol_state_data:update(ProtocolState, [{buffer, Request}])};
    error ->
      {stop, error_opening_connection};
    {reply, Reply} ->
      {[{reply, Reply}], new_protocol_module, wsserver_websocket_protocol}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_request(Data) ->
  case wsock_http:decode(Data, request) of
    fragmented_http_message ->
      incomplete;
    {error, _} ->
      error;
    {ok, OpenHTTPMessage} ->
      {ok, OpenHandshake}     = wsock_handshake:handle_open(OpenHTTPMessage),
      ClientWSKey             = wsock_http:get_header_value("sec-websocket-key", OpenHandshake#handshake.message),
      {ok, HandshakeResponse} = wsock_handshake:response(ClientWSKey),
      {reply, wsock_http:encode(HandshakeResponse#handshake.message)}
  end.
