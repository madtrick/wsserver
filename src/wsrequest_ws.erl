-module(wsrequest_ws).
-include("wsrequest_ws.hrl").

-export([init/0, process/2]).

init() ->
  #wsrequest_ws_state{}.

process(Data, State = #wsrequest_ws_state{ buffered = undefined }) ->
  process_decoded_messages(wsmessage:decode(Data), [], State);
process(Data, State = #wsrequest_ws_state{ buffered = FragmentedMessage }) ->
  process_decoded_messages(wsmessage:decode(Data, FragmentedMessage), [], State#wsrequest_ws_state{ buffered = undefined }).

process_decoded_messages([], Acc, State) ->
  {lists:reverse(Acc), State};
process_decoded_messages([H | T], Acc, State) ->
  process_decoded_message(H, T, Acc, State).

process_decoded_message({fragmented, Message}, Tail, Acc, State) ->
  process_decoded_messages(Tail, Acc, State#wsrequest_ws_state{ buffered = Message });
process_decoded_message(Message, Tail, Acc, State) ->
  process_decoded_messages(Tail, [Message | Acc], State).


