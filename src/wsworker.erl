-module(wsworker).

-export([start_link/1]).
-export([init/1, loop/2, handle_data/3]).

-include_lib("wsock/include/wsock.hrl").

-define(HANDSHAKE, handshake).
-define(OPEN, open).
-define(CLOSE, close).

-record(state, {
  status = ?HANDSHAKE
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
  case State#state.status of
    ?HANDSHAKE ->
      {reply, NextStatus, Response} = handle(?HANDSHAKE, Data),
      gen_tcp:send(Socket, Response),
      State#state{status = NextStatus};
    ?OPEN ->
      {reply, NextStatus, Messages} = handle(?OPEN, Data),
      gen_tcp:send(Socket, Messages),
      State#state{status = NextStatus};
    _ -> io:format("invalid state \n")
  end.

handle(?HANDSHAKE, Data) ->
  {ok, OpenHttpMessage}   = wsock_http:decode(Data, request),
  {ok, OpenHandshake}     = wsock_handshake:handle_open(OpenHttpMessage),
  ClientWSKey             = wsock_http:get_header_value("sec-websocket-key", OpenHandshake#handshake.message),
  {ok, HandshakeResponse} = wsock_handshake:response(ClientWSKey),
  ResponseHttpMessage     = wsock_http:encode(HandshakeResponse#handshake.message),
  {reply, ?OPEN, ResponseHttpMessage};

handle(Status, Data) ->
  [Message] = wsock_message:decode(Data, [masked]),
  handle_message(Status, Message).

handle_message(?OPEN, Message) when Message#message.type == text ->
  io:format("Receive message: ~s \n", [Message#message.payload]),
  ResponseMessages = wsock_message:encode("Received", [text]),
  {reply, ?OPEN, ResponseMessages};

handle_message(?OPEN, Message) when Message#message.type == close  ->
  io:format("Close connection \n", []),
  CloseMessage = wsock_message:encode("OK", [close]),
  {reply, ?CLOSE, CloseMessage}.

