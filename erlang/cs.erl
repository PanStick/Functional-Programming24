-module (cs).
-export ([server/0,start/0,client/1]).
recvMsg() -> receive
    _ ->
        io:format("Message received")
%% tutaj obsluga przychodzacych komunikatow,
%% czyli kod serwera.
    end,
recvMsg().
server() -> io:format("Server started.~n",[]),
recvMsg().

client(SPID) -> SPID ! msg.
%% przykladowe dzialanie klienta, w tym
%% wypadku wyslanie atomu msg do serwera
start() -> Server_PID = spawn(cs,server,[]),
spawn (cs,client,[Server_PID]).