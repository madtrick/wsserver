-module(wsserver_http_protocol_state_data).

-export([new/0]).
-export([update/2]).
-export([buffer/1]).

-record(wsserver_http_protocol_state_data, {
    buffer
  }).

new() ->
  #wsserver_http_protocol_state_data{  buffer = <<>> }.

buffer(#wsserver_http_protocol_state_data{ buffer = Buffer }) -> Buffer.

update(ProtocolState, Options) ->
  ProtocolState#wsserver_http_protocol_state_data{
    buffer = proplists:get_value(buffer, Options, buffer(ProtocolState))
  }.
