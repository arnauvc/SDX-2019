-module(groupy1).
-export([start/2, stop/0]).

% We use the name of the module (i.e. gms3) as the parameter Module to the start procedure. Sleep stands for up to how many milliseconds the workers should wait until the next message is sent.

start(Module, Sleep) ->
    Node1 = 'p1@127.0.0.1',
    Node2 = 'p2@127.0.0.1',
    Node3 = 'p3@127.0.0.1',
    Node4 = 'p4@127.0.0.1',
    Node5 = 'p5@127.0.0.1',
    P = worker:start("P1", Module, Sleep),
    spawn(Node1, fun() -> register(a, P) end), 
    spawn(Node2, fun() -> register(b, worker:start("P2", Module, P, Sleep)) end),
    spawn(Node3, fun() -> register(c, worker:start("P3", Module, P, Sleep)) end),
    spawn(Node4, fun() -> register(d, worker:start("P4", Module, P, Sleep)) end),
    spawn(Node5, fun() -> register(e, worker:start("P5", Module, P, Sleep)) end).

stop() ->
    Node1 = 'p1@127.0.0.1',
    Node2 = 'p2@127.0.0.1',
    Node3 = 'p3@127.0.0.1',
    Node4 = 'p4@127.0.0.1',
    Node5 = 'p5@127.0.0.1',
    stop({a, Node1}),
    stop({b, Node2}),
    stop({c, Node3}),
    stop({d, Node4}),
    stop({e, Node5}).

stop(Name) ->
    if 
	is_tuple(Name) -> Name ! stop;
    true -> 
	case whereis(Name) of
            undefined ->
                ok;
            Pid ->
                Pid ! stop
        end
    end.

%start(Module, Sleep, IP) ->
%    Node = IP,
     
