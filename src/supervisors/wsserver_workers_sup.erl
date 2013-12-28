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
-module(wsserver_workers_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([new_worker/3]).

-define(CHILD(I, Type, Options), {I, {I, start_link, Options}, temporary, 5000, Type, [I]}).

start_link() ->
  supervisor:start_link(?MODULE, []).

new_worker(Supervisor, ListenSock, WorkerOptions) ->
  supervisor:start_child(Supervisor, [ListenSock, WorkerOptions]).

init([]) ->
  {ok, { {simple_one_for_one, 5, 10}, [?CHILD(wsserver_worker, worker, [])]} }.
