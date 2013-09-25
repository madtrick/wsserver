-module(wsmessage).
-include_lib("wsock/include/wsock.hrl").

-export([decode/1, decode/2, close/1]).

decode(Data, FragmentedMessage) ->
  Messages = wsock_message:decode(Data, FragmentedMessage, [masked]),
  transform_to_expected_return_value(Messages).

decode(Data) ->
  Messages = wsock_message:decode(Data, [masked]),
  transform_to_expected_return_value(Messages).

close(Payload) ->
  wsock_message:encode(Payload, [close]).

transform_to_expected_return_value(Messages) ->
  [{type(Message), data(Message)} || Message <- Messages].

type(#message{ type = Type }) ->
  Type.

data(Message = #message{ type = fragmented }) ->
  Message;
data(#message{ payload = Payload }) ->
  Payload.
