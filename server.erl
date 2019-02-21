-module(server).
%% Exported Functions
-export([start/0]).

start() ->
    ServerPid = spawn(fun() -> process_requests([]) end),
    register(myserver, ServerPid).

process_requests(Clients) ->
    receive
        {client_join_req, Name, From} ->
            NewClients = [From|Clients],  %% TODO: done 
            broadcast(NewClients, {join, Name}),
            process_requests(NewClients);  %% TODO: done 
        {client_leave_req, Name, From} ->
            NewClients = lists:delete(From,Clients),  %% TODO: done
            broadcast(Clients, {leave, Name}),  %% TODO: done 
            From ! exit,
            process_requests(NewClients);  %% TODO: done
        {send, Name, Text} ->
            broadcast(Clients, {message, Name,Text}),  %% TODO: done
            process_requests(Clients);
        disconnect ->
            unregister(myserver)
    end.

broadcast(PeerList, Message) ->
    Fun = fun(Peer) -> Peer ! Message end,
    lists:map(Fun, PeerList).
