#Change Log

###0.3.2

* Change the branch from where wsock is fetch.
* Remove http messages decoding code that was included to the server to cover a deficiency in wsock (bad decoding of fragmented http messages). This ad-hoc decoding code failed when the incomming requests didn't include a 'Origin' header. Use now the wsock decoding capabilities which can work with fragmented http messages.

###0.3.1

* Fix a bug. Don't pass to the handler module fragmented messages.

###0.3.0

* Fix a bug. The response from the ```wsserver_http_protocol:handle_connection_in/2``` was in some cases the atom ```do_nothing```  which is no longer valid.

###0.2.0

* New API module to interact with websocket workers: ```wsserver_worker_websocket```. This change makes unusable the old API module ```wsserver_worker```.
* Added to this API functions to close a connection (```wsserver_worker_websocket:close```) and to generate a ping request (```wsserver_worker_websocket:ping```). The old ```wsserver_worker:send``` is now ```wsserver_worker_websocket:send```.
* Now connection closure is properly handled in both ways (either initiated from server or from client).
