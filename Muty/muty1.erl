-module(muty1).
-export([start/3, stop/0]).

% We use the name of the module (i.e. lock3) as a parameter to the start procedure. We also provide the average time (in milliseconds) the worker is going to sleep before trying to get the lock (Sleep) and work with the lock taken (Work).

start(Lock, Sleep, Work) ->
    spawn('node1@127.0.0.1', fun() -> register(l1, apply(Lock, start, [1]))end),
    spawn('node2@127.0.0.1', fun() -> register(l2, apply(Lock, start, [2]))end),
    spawn('node3@127.0.0.1', fun() -> register(l3, apply(Lock, start, [3]))end),
    spawn('node4@127.0.0.1', fun() -> register(l4, apply(Lock, start, [4]))end),
    {l1,'node1@127.0.0.1'} ! {peers, [l2, l3, l4]},
    {l2,'node2@127.0.0.1'} ! {peers, [l1, l3, l4]},
    {l3,'node3@127.0.0.1'} ! {peers, [l1, l2, l4]},
    {l4,'node4@127.0.0.1'} ! {peers, [l1, l2, l3]},
    spawn('node1@127.0.0.1', fun() -> register(w1, worker:start("John", l1, Sleep, Work))end),
    spawn('node2@127.0.0.1', fun() -> register(w2, worker:start("Ringo", l2, Sleep, Work))end),    
    spawn('node3@127.0.0.1', fun() -> register(w3, worker:start("Paul", l3, Sleep, Work))end),
    spawn('node4@127.0.0.1', fun() -> register(w4, worker:start("George", l4, Sleep, Work))end),
    ok.

stop() ->
    {w1,'node1@127.0.0.1'} ! stop,
    {w2,'node2@127.0.0.1'} ! stop,
    {w3,'node3@127.0.0.1'} ! stop,
    {w4,'node4@127.0.0.1'} ! stop.
