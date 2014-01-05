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
-module(wsserver_worker_websocket).
-include("websockets.hrl").

-export([send/2, close/1, close/2, ping/1, ping/2]).

send(Worker, Data) ->
  gen_server:cast(Worker, {protocol_action, send, [Data]}).

ping(Worker) ->
  ping(Worker, []).
ping(Worker, Data) ->
  gen_server:cast(Worker, {protocol_action, ping, [Data]}).

close(Worker) ->
  close(Worker, {?NORMAL_CLOSURE_CODE, ?NORMAL_CLOSURE_REASON}).
close(Worker, Data) ->
  gen_server:cast(Worker, {protocol_action, close, [Data]}).
