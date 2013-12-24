-module(wsworker).
-behaviour(gen_server).

-export([start_link/2, process/2, send/2, stop/1]).
-export([init/1, handle_cast/2, terminate/2]).

-include("wsworker.hrl").
-include_lib("wsock/include/wsock.hrl").

-define(HANDSHAKE, handshake).
-define(OPEN, open).
-define(CLOSE, close).

-record(handler, {
    module,
    state
  }).

-record(state, {
  worker_socket,
  handler,
  status = ?HANDSHAKE,
  fragmented_message,
  processor_state,
  buffer
  }).

start_link(Socket, Options) ->
  gen_server:start_link(?MODULE, [Socket, Options], []).

process(Worker, Data) ->
  gen_server:cast(Worker, {process, Data}).

send(Worker, Data) ->
  gen_server:cast(Worker, {send, Data}).

stop(Worker) ->
  gen_server:cast(Worker, stop).

init([Socket, Options]) ->
  HandlerModule = proplists:get_value(handler, Options),
  HandlerState  = HandlerModule:init([{worker, self()}]),
  {ok, #state{ worker_socket = Socket, handler = #handler{ module = HandlerModule, state = HandlerState }, buffer = <<>>}}.

handle_cast({send, Data}, State) ->
  wsworker_socket:send(State#state.worker_socket, wsmessage:encode(Data)),
  {noreply, State};

handle_cast(stop, State) ->
  {stop, stop, State};

handle_cast({process, Data}, State) ->
  NewState = handle(State, Data),
  {noreply, NewState}.

terminate(_Reason, _State) ->
  ok.

handle(State = #state{ status = ?HANDSHAKE }, Data) ->
  Buffer  = State#state.buffer,
  Request = <<Buffer/binary, Data/binary>>,

  case wsworker_request_processor:handle_upgrade_request(Request) of
    {incomplete, ToBuffer} ->
      NewBuffer = <<Buffer/binary, ToBuffer/binary>>,
      State#state{ buffer = NewBuffer };
    {reply, Reply, NewProcessorState} ->
      wsworker_socket:send(State#state.worker_socket, Reply),
      State#state{ buffer = <<>>, status = ?OPEN, processor_state = NewProcessorState }
  end;

%handle(State = #state{ status = ?HANDSHAKE }, Data) ->
%  io:format("Handling handshape request ~w ~n", [Data]),
%  {Reply, NewProcessorState} = wsworker_request_processor:handle_upgrade_request(Data),
%  wsworker_socket:send(State#state.worker_socket, Reply),
%  State#state{ status = ?OPEN, processor_state = NewProcessorState };

handle(State = #state{ status = ?OPEN }, Data) ->
  {Messages, NewProcessorState} = wsworker_request_processor:handle_ws_request(Data, State#state.processor_state),
  process_messages(Messages, State#state{ processor_state = NewProcessorState }).

process_messages([], State) ->
  State;
process_messages([H | T], State) ->
  case process_message(H, State) of
    {noreply, NewState} -> 
      process_messages(T, NewState);
    {reply, Reply, NewState} ->
      wsworker_socket:send(State#state.worker_socket, Reply),
      process_messages(T, NewState);
    {close, Reply, NewState} ->
      wsworker_socket:send(State#state.worker_socket, Reply),
      wsworker:stop(self()),
      NewState
  end.

process_message(Message = {Type, _Payload}, State) when Type =:= text; Type =:= binary->
  {ReplyType, NewHandlerState} = (State#state.handler#handler.module):handle(Message, State#state.handler#handler.state),

  {ReplyType, State#state{ handler = State#state.handler#handler{ state = NewHandlerState } }};
process_message({close, _Payload}, State) ->
  {close, wsmessage:close("OK"), State#state{ status = ?CLOSE }}.
