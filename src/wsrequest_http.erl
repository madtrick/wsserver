-module(wsrequest_http).
-include_lib("wsock/include/wsock.hrl").

-export([process/1]).

process(Data) ->
  {ok, OpenHttpMessage}   = wsock_http:decode(Data, request),
  {ok, OpenHandshake}     = wsock_handshake:handle_open(OpenHttpMessage),
  ClientWSKey             = wsock_http:get_header_value("sec-websocket-key", OpenHandshake#handshake.message),
  {ok, HandshakeResponse} = wsock_handshake:response(ClientWSKey),
  wsock_http:encode(HandshakeResponse#handshake.message).
