%Copyright [2012] [Farruco Sanjurjo Arcay]

%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at

%       http://www.apache.org/licenses/LICENSE-2.0

%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.
-module(wsmessage).
-include_lib("wsock/include/wsock.hrl").

-export([close/1, decode/1, decode/2, encode/1, ping/1, pong/1]).

close(Data) ->
  wsock_message:encode(Data, [close]).

decode(Data, FragmentedMessage) ->
  Messages = wsock_message:decode(Data, FragmentedMessage, [masked]),
  transform_to_expected_return_value(Messages).

decode(Data) ->
  Messages = wsock_message:decode(Data, [masked]),
  transform_to_expected_return_value(Messages).

encode(Data) ->
  wsock_message:encode(Data, [text]).

ping(Data) ->
  wsock_message:encode(Data, [ping]).

pong(Data) ->
  wsock_message:encode(Data, [pong]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transform_to_expected_return_value(Messages) ->
  [{type(Message), data(Message)} || Message <- Messages].

type(#message{ type = Type }) ->
  Type.

data(Message = #message{ type = fragmented }) ->
  Message;
data(#message{ payload = Payload }) ->
  Payload.
