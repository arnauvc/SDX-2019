-module(lock3).
-export([start/1]).

start(MyId) ->
    spawn(fun() -> init(MyId, 0) end).

init(MyId, Clock) ->
    receive
        {peers, Nodes} ->
            open(Nodes, MyId, Clock);
        stop ->
            ok
    end.

open(Nodes, MyId, Clock) ->
    receive
        {take, Master, Ref} ->
            NewClock = Clock + 1,
            Refs = requests(Nodes, MyId, NewClock),
            wait(Nodes, Master, Refs, [], Ref, MyId, NewClock);
        {request, From,  Ref, _, OtherClock} ->
            NewClock = max(OtherClock, Clock),
            From ! {ok, Ref},
            open(Nodes, MyId, NewClock);
        stop ->
            ok
    end.

requests(Nodes, MyId, Clock) ->
    lists:map(
      fun(P) -> 
        R = make_ref(), 
        P ! {request, self(), R, MyId, Clock},
        R 
      end, 
      Nodes).

wait(Nodes, Master, [], Waiting, TakeRef, MyId, Clock) ->
    Master ! {taken, TakeRef},
    held(Nodes, Waiting, MyId, Clock);
wait(Nodes, Master, Refs, Waiting, TakeRef, MyId, Clock) ->
    receive
        {request, From, Ref, OtherId, OtherClock} ->
            if
                OtherClock < Clock -> From ! {ok, Ref},R = make_ref(),From ! {request, self(), R, MyId, Clock}, wait(Nodes, Master, [R | Refs], Waiting, TakeRef, MyId, Clock);

                OtherClock > Clock -> wait(Nodes, Master, Refs, [{From, Ref}|Waiting], TakeRef, MyId, Clock);

                OtherClock == Clock -> if
                                           MyId > OtherId -> From ! {ok, Ref},R = make_ref(),From ! {request, self(), R, MyId, Clock}, wait(Nodes, Master, [R | Refs], Waiting, TakeRef, MyId, Clock);

                                           MyId =< OtherId -> wait(Nodes, Master, Refs, [{From, Ref}|Waiting], TakeRef, MyId, Clock)
                                       end
            end;
        {ok, Ref} ->
            NewRefs = lists:delete(Ref, Refs),
            wait(Nodes, Master, NewRefs, Waiting, TakeRef, MyId, Clock);
        release ->
            ok(Waiting),            
            open(Nodes, MyId, Clock)
    end.

ok(Waiting) ->
    lists:map(
      fun({F,R}) -> 
        F ! {ok, R} 
      end, 
      Waiting).

held(Nodes, Waiting, MyId, Clock) ->
    receive
        {request, From, Ref, OtherClock} ->
            NewClock = max(OtherClock, Clock),
            held(Nodes, [{From, Ref}|Waiting], MyId, NewClock);
        release ->
            ok(Waiting),
            open(Nodes, MyId, Clock)
    end.
