[![Analytics](https://ga-beacon.appspot.com/UA-46795389-1/wsserver/README)](https://github.com/igrigorik/ga-beacon)

# WSSERVER

A simple WebSocket server.

###About
This is a simple WebSocket server written in Erlang. It leverages the library [wsock](https://github.com/madtrick/wsock) (which implements the [RFC6455](http://tools.ietf.org/html/rfc6455)) to do all the WebSocket encoding/decoding .

###Usage

####Starting
To start **wsserver** just pass the required options. This options are:

* Port number.
* Number of workers which will be handling the connections.
* Handler module for messages received through a WebSocket channel.

Below is an example:

```erlang
Options = [
	{port, 8080},
	{number_of_workers, 10},
	{worker_options,[
		{protocol_modules,[
			{wsserver_websocket_protocol, [
				{handler_module, YOUR_HANDLER_MODULE}
			}]
		}]
	}]
],
wsserver_server:start_link(Options).
```

####Handler module

The handler module has to export two functions: ```init/1``` and ```handle/2```.

The ```init/1``` function will be called when a WebSocket connection is established. It will be passed a proplist as parameter. This proplist will contain a reference to the wsserver_worker (under the key worker) which handles the connection. Keep this reference to the worker because you will need it later to send data through the WebSocket connection. The value returned by this function will be used as the state of the handler and passed lately to the ```handle/2``` function.

The ```handle/2``` function will be called on each message received on the WebSocket connection and will be passed the message and the handler state (returned from init/1) as parameters. This function can return one of three terms:

* ```{noreply, NewState}```. See [sending data asynchronously](#send_async).
* ```{reply, Reply, NewState}```. Synchronous reply.
* ```{close, Reply, NewState}```. Close the WebSocket connection and stop the worker.

Below is an example of a handler module:

```erlang
-module(handler).
-export([init/1, handle/2]).

-record(state, {}).

init(Options) ->
 % Do something fancy with the options
 #state{}.
 
handle(Message, State) ->
 % Do something with the message
 {noreply, State}.
```

####Sending data asynchronously <a name="send_async"></a>

Use the function ```wsserver_worker_websocket:send/2``` to send data through a WebSocket connection at any moment. Pass it the reference of a worker and the data you want to send as parameters.

Below is an example:

```erlang
wsserver_worker:send(Worker, Data)
```


###ToDo


* When accepting a connection, provide some info about it (ip, port, etc.) to the ```init/1``` function.
* Allow the handling module to reject a connection before it is established.
* Accept connections only for a given resource.
* Add support for SSL.

###Contribute
If you find or think that something isn't working properly, just open an issue.

Pull requests and patches are welcome.
###Author
Farruco Sanjurjo. You can contact me at:

* Twitter [@madtrick](https://twitter.com/madtrick)
* Mail madtrick@gmail.com

###License
Copyright [2013] [Farruco Sanjurjo Arcay]

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
