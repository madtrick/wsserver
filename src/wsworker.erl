-module(wsworker).

-export([start_link/1]).
-export([init/1, loop/2, handle_data/3]).

-include_lib("wsock/include/wsock.hrl").

-define(HANDSHAKE, handshake).
-define(OPEN, open).
-define(CLOSE, close).

-record(state, {
  status = ?HANDSHAKE,
  fragmented_message
  }).

start_link(Socket) ->
  spawn_link(?MODULE, init, [Socket]).

init(Socket) ->
  loop(Socket, #state{}).

loop(Socket, State) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      NewState = handle_data(Socket, Data, State),
      loop(Socket, NewState);
    {error, Reason} ->
      exit(Reason)
  end.

handle_data(Socket, Data, State) ->
  case handle(State, Data) of
    {reply, NewState, Messages} ->
      gen_tcp:send(Socket, Messages),
      NewState;
    {fragmented_message, NewState} ->
      NewState;
    _ ->
      io:format("Unknown response \n" , []),
      State
  end.

handle(State = #state{ status = ?HANDSHAKE }, Data) ->
  {ok, OpenHttpMessage}   = wsock_http:decode(Data, request),
  {ok, OpenHandshake}     = wsock_handshake:handle_open(OpenHttpMessage),
  ClientWSKey             = wsock_http:get_header_value("sec-websocket-key", OpenHandshake#handshake.message),
  {ok, HandshakeResponse} = wsock_handshake:response(ClientWSKey),
  ResponseHttpMessage     = wsock_http:encode(HandshakeResponse#handshake.message),
  {reply, State#state{ status = ?OPEN }, ResponseHttpMessage};

handle(State = #state{ fragmented_message = undefined }, Data) ->
  [Message] = wsock_message:decode(Data, [masked]),
  handle_message(State, Message);
handle(State = #state{ fragmented_message = FragmentedMessage }, Data) ->
  [Message] = wsock_message:decode(Data, FragmentedMessage, [masked]),
  handle_message(State, Message).

handle_message(State = #state{ status = ?OPEN }, Message = #message{ type = fragmented }) ->
  {fragmented_message, State#state{ fragmented_message = Message }};
handle_message(State = #state{ status = ?OPEN }, Message = #message{ type = text }) ->
  io:format("Receive message: ~s \n", [Message#message.payload]),
  ResponseMessages = wsock_message:encode("Received", [text]),
  {reply, State#state{ fragmented_message = undefined }, ResponseMessages};
handle_message(State = #state{ status = ?OPEN }, #message{ type = close }) ->
  io:format("Close connection \n", []),
  CloseMessage = wsock_message:encode("OK", [close]),
  {reply, State#state{status = ?CLOSE}, CloseMessage}.

