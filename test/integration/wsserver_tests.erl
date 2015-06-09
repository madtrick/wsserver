-module(wsserver_tests).
-include_lib("eunit/include/eunit.hrl").

notifies_handler_when_connection_closes_test_() ->
  {setup,
    fun() ->
        process_flag(trap_exit, true),

        meck:new(wsserver_mock_handler, [passthrough]),
        meck:expect(wsserver_mock_handler, handle, 2, {noreply, ok}),

        ServerConfig = [
          {port, 8080},
          {number_of_workers, 1},
          {worker_options, [
            {protocol_modules_options, [
                {wsserver_websocket_protocol,[
                  {handler_module, wsserver_mock_handler}
                ]}
            ]}
        ]}],

        {ok, Server} = wsserver_server:start_link(ServerConfig),
        {ok, Client} = wsclient:start_link(),

        {client, Client, server, Server}
    end,
    fun(Pids) ->
        meck:unload(wsserver_mock_handler),
        {client, _, server, Server} = Pids,

        wsserver_server:stop(Server)
    end,
    fun(Pids) ->
        {client, Client, server, _} = Pids,
        catch exit(Client, die_motherfucker),
        timer:sleep(500),
        ?_assert(meck:called(wsserver_mock_handler, handle, [connection_close, '_']))
    end}.
