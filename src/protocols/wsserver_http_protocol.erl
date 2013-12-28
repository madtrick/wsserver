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
      {do_nothing, wsserver_http_protocol_state_data:update(ProtocolState, [{buffer, Request}])};
    {reply, Reply} ->
      {send, Reply, new_protocol_module, wsserver_websocket_protocol}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_request(Data) ->
  case get_request(wsock_http_message_data:new(), Data) of
    incomplete ->
      incomplete;
    {ok, OpenHttpMessage} ->
      {ok, OpenHandshake}     = wsock_handshake:handle_open(OpenHttpMessage),
      ClientWSKey             = wsock_http:get_header_value("sec-websocket-key", OpenHandshake#handshake.message),
      {ok, HandshakeResponse} = wsock_handshake:response(ClientWSKey),
      {reply, wsock_http:encode(HandshakeResponse#handshake.message)}
  end.

get_request(HttpMessage, Data) ->
  case erlang:decode_packet(http_bin, Data, []) of
    {more, _} ->
      incomplete;
    {ok, {http_request, Method, Resource, {VersionMajor, VersionMinor}}, Rest} ->
      % wsock_handshake expects the method to be a string
      % wsock_handshake expects the version to be a string
      UpdateHttpMessage = wsock_http_message_data:update(HttpMessage, [{start_line, [
              {method, request_method_as_string(Method)},
              {resource, Resource},
              {version, request_version_as_string(VersionMajor, VersionMinor)}
            ]}]),
      get_headers(UpdateHttpMessage, Rest)
  end.

get_headers(HttpMessage, Data) ->
  case erlang:decode_packet(httph_bin, Data, []) of
    {more, _} ->
      incomplete;
    {ok, {http_header, _, Key, _, Value}, Rest} ->
      % wsock_handshake expects the header name to be a string
      UpdateHttpMessage = wsock_http_message_data:update(HttpMessage, [{headers, [
              {header_key_as_string(Key), header_value_as_string(Value)} | wsock_http_message_data:headers(HttpMessage)
            ]}]),
      get_headers(UpdateHttpMessage, Rest);
    {ok, http_eoh, _} ->
      {ok, HttpMessage}
  end.

header_key_as_string(Key) when is_binary(Key) ->
  erlang:binary_to_list(Key);
header_key_as_string(Key) when is_atom(Key) ->
  erlang:atom_to_list(Key);
header_key_as_string(Key) ->
  Key.

header_value_as_string(Value) when is_binary(Value) ->
  erlang:binary_to_list(Value);
header_value_as_string(Value) ->
  Value.

request_method_as_string(Method) when is_binary(Method) ->
  erlang:binary_to_list(Method);
request_method_as_string(Method) when is_atom(Method) ->
  erlang:atom_to_list(Method);
request_method_as_string(Method) ->
  Method.

request_version_as_string(VersionMajor, VersionMinor) ->
  [erlang:integer_to_list(VersionMajor) | [ "." | erlang:integer_to_list(VersionMinor) ] ].
