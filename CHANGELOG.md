#Change Log

###0.2.0

* New API module to interact with websocket workers: ```wsserver_worker_websocket```. This change makes unusable the old API module ```wsserver_worker```.
* Added to this API functions to close a connection (```wsserver_worker_websocket:close```) and to generate a ping request (```wsserver_worker_websocket:ping```). The old ```wsserver_worker:send``` is now ```wsserver_worker_websocket:send```.
* Now connection closure is properly handled in both ways (either initiated from server or from client).