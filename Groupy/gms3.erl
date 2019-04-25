-module(gms3).
-export([start/1, start/2]).
-define(timeout, 1000).
-define(arghh, 100).

start(Name) ->
  Self = self(),
  spawn_link(fun()-> init(Name, Self) end).

init(Name, Master) ->
  leader(Name, Master, [], 0).

start(Name, Grp) ->
  Self = self(),
  spawn_link(fun()-> init(Name, Grp, Self) end).


init(Name, Grp, Master) ->
  Self = self(),
  Grp ! {join, Self},
  receive
    {view, Leader, Slaves, I} ->
      Master ! joined,
      Ref = erlang:monitor(process, Leader),
      NewI = I + 1,
      slave(Name, Master, Leader, Slaves, Ref, NewI, null)
    after
      ?timeout ->
        Master ! {error, "no reply from leader"}
  end.

leader(Name, Master, Slaves, N) ->
  receive
    {mcast, Msg} ->
      bcast(Name, {msg, Msg, N}, Slaves),  %% TODO: COMPLETED
      %% TODO: ADD SOME CODE
      Master ! {deliver, Msg},
      NewN = N + 1,
      leader(Name, Master, Slaves, NewN);
    {join, Peer} ->
      NewSlaves = lists:append(Slaves, [Peer]),
      bcast(Name, {view, self(), NewSlaves, N}, NewSlaves),  %% TODO: COMPLETED
      NewN = N + 1,
      leader(Name, Master, NewSlaves, NewN);  %% TODO: COMPLETED
    stop ->
      ok;
    Error ->
      io:format("leader ~s: strange message ~w~n", [Name, Error])
  end.

bcast(Name, Msg, Nodes) ->
  lists:foreach(fun(Node) ->
    Node ! Msg,
    crash(Name, Msg)
                end,
    Nodes).
crash(Name, Msg) ->
  case rand:uniform(?arghh) of
    ?arghh ->
      io:format("leader ~s CRASHED: msg ~w~n", [Name, Msg]),
      exit(no_luck);
    _ ->
      ok
  end.


slave(Name, Master, Leader, Slaves, Ref, N, Last) ->
  receive
    {mcast, Msg} ->
      %% TODO: ADD SOME CODE
      Leader ! {mcast, Msg},
      slave(Name, Master, Leader, Slaves, Ref, N, Last);
    {join, Peer} ->
      %% TODO: ADD SOME CODE
      Leader ! {join, Peer},
      slave(Name, Master, Leader, Slaves, Ref, N, Last);
    {msg, _, I} when I < N ->
      slave(Name, Master, Leader, Slaves, Ref, N, Last);
    {msg, Msg, I} ->
      %% TODO: ADD SOME CODE
      NewN = I + 1,
      NewLast = {msg, Msg, I},
      Master ! {deliver, Msg},
      slave(Name, Master, Leader, Slaves, Ref, NewN, NewLast);
    {view, _, _, I} when I < N ->
      slave(Name, Master, Leader, Slaves, Ref, N, Last);
    {view, Leader, NewSlaves, I} ->
      NewN = I + 1,
      NewLast = {view, Leader, NewSlaves, I},
      slave(Name, Master, Leader, NewSlaves, Ref, NewN, NewLast);  %% TODO: COMPLETE
    {view, NewLeader, NewSlaves, I} ->
      NewN = I + 1,
      NewLast = {view, NewLeader, NewSlaves, I},
      erlang:demonitor(Ref, [flush]),
      NewRef = erlang:monitor(process, NewLeader),
      slave(Name, Master, NewLeader, NewSlaves, NewRef, NewN, NewLast);
    {'DOWN', _Ref, process, Leader, _Reason} ->
      election(Name, Master, Slaves, N, Last);
    stop ->
      ok;
    Error ->
      io:format("slave ~s: strange message ~w~n", [Name, Error])
  end.

election(Name, Master, Slaves, N, Last) ->
  Self = self(),
  case Slaves of
    [Self|Rest] ->
      %% TODO: ADD SOME CODE: DONE
      bcast(Name, Last, Rest),
      MidN = N + 1,
      bcast(Name, {view, Self, Rest, MidN}, Rest),
      NewN = MidN + 1,
      leader(Name, Master, Rest, NewN); %% TODO: COMPLETED
    [NewLeader|Rest] ->
      %% TODO: ADD SOME CODE
      Ref = erlang:monitor(process, NewLeader),
      slave(Name, Master, NewLeader, Rest, Ref, N, Last) %% TODO: COMPLETE
end.

