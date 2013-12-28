-module(wsserver_websocket_protocol_state_data).

-export([new/1]).
-export([update/2]).
-export([buffered/1, buffer/1, handler_module/1, handler_state/1]).

-record(wsserver_websocket_protocol_state_data, {
    buffered,
    buffer,
    handler_module,
    handler_state
  }).

new(Options) ->
  update(#wsserver_websocket_protocol_state_data{ buffered = false }, Options).

buffered(#wsserver_websocket_protocol_state_data{ buffered = Buffered }) -> Buffered.
buffer(#wsserver_websocket_protocol_state_data{ buffer = Buffer }) -> Buffer.
handler_module(#wsserver_websocket_protocol_state_data{ handler_module = HandlerModule }) -> HandlerModule.
handler_state(#wsserver_websocket_protocol_state_data{ handler_state = HandlerState }) -> HandlerState.

update(ProtocolState, Options) ->
  ProtocolState#wsserver_websocket_protocol_state_data{
    buffered       = proplists:get_value(buffered, Options, buffered(ProtocolState)),
    buffer         = proplists:get_value(buffer, Options, buffer(ProtocolState)),
    handler_module = proplists:get_value(handler_module, Options, handler_module(ProtocolState)),
    handler_state  = proplists:get_value(handler_state, Options, handler_state(ProtocolState))
  }.
