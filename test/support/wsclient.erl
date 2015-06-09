% WS client copied from:
% https://github.com/jeremyong/websocket_client/blob/master/ct/ws_client.erl
-module(wsclient).
-behaviour(websocket_client_handler).

-export([
  start_link/0,
  start_link/1,
  send_text/2,
  send_binary/2,
  send_ping/2,
  recv/2,
  recv/1,
  stop/1,
  flush/1
]).

-export([
  init/2,
  websocket_handle/3,
  websocket_info/3,
  websocket_terminate/3
]).

-record(state, {
  buffer = [] :: list(),
  waiting = undefined :: undefined | pid()
}).

start_link() ->
  start_link(8080).
start_link(Port) ->
  PortAsString = erlang:integer_to_list(Port),
  Url          = "ws://localhost:" ++ PortAsString,
  websocket_client:start_link(Url, ?MODULE, []).

stop(Pid) ->
  Pid ! stop.

send_text(Pid, Msg) ->
  websocket_client:cast(Pid, {text, Msg}).

send_binary(Pid, Msg) ->
  websocket_client:cast(Pid, {binary, Msg}).

send_ping(Pid, Msg) ->
  websocket_client:cast(Pid, {ping, Msg}).

recv(Pid) ->
  recv(Pid, 5000).

recv(Pid, Timeout) ->
  Pid ! {recv, self()},
  receive
    M -> M
  after
    Timeout -> {error, timeout}
  end.

%TODO: remove the flush function
flush(Pid) ->
  Pid ! {flush, self()}.

init(_, _WSReq) ->
  {ok, #state{}}.

websocket_handle(Frame, _, State = #state{waiting = undefined, buffer = Buffer}) ->
  io:format("Client received frame~n"),
  {ok, State#state{buffer = [Frame|Buffer]}};
websocket_handle(Frame, _, State = #state{waiting = From}) ->
  io:format("Client received frame~n"),
  From ! Frame,
  {ok, State#state{waiting = undefined}}.

websocket_info({send_text, Text}, WSReq, State) ->
  websocket_client:send({text, Text}, WSReq),
  {ok, State};
websocket_info({flush, _}, _, State) ->
  {ok, State#state{buffer = []}};
websocket_info({recv, From}, _, State = #state{buffer = []}) ->
  {ok, State#state{waiting = From}};
websocket_info({recv, From}, _, State = #state{buffer = [Top|Rest]}) ->
  From ! Top,
  {ok, State#state{buffer = Rest}};
websocket_info(stop, _, State) ->
  {close, <<>>, State}.

websocket_terminate(Close, _, State) ->
  io:format("Websocket closed with frame ~p and state ~p", [Close, State]),
  ok.

