-module(node3).
-export([start/2, start/3]).

-define(Stabilize, 1000).
-define(Timeout, 5000).

start(Name, MyKey) ->
    start(Name, MyKey, nil).

start(Name, MyKey, PeerPid) ->
    timer:start(),
    register(Name, spawn(fun() -> init(MyKey, PeerPid) end)).

init(MyKey, PeerPid) ->
    Predecessor = nil,
    {ok, Successor} = connect(MyKey, PeerPid),
    schedule_stabilize(),    
    node(MyKey, Predecessor, Successor, storage:create(),nil).

connect(MyKey, nil) ->
    Ref = monit(self()),
    {ok, {MyKey, Ref, self()}};    %% DONE: ADD SOME CODE
connect(_, PeerPid) ->
    Qref = make_ref(),
    PeerPid ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            Sref = monit(PeerPid),
            {ok, {Skey, Sref, PeerPid}}    %% DONE: ADD SOME CODE
    after ?Timeout ->
        io:format("Timeout: no response from ~w~n", [PeerPid])
    end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

node(MyKey, Predecessor, Successor,Store,Next) ->
    receive 
		    delete ->
			      io:format("Delete OK"),
			      node(MyKey, Predecessor, Successor,[],Next);
        {key, Qref, Peer} ->
            Peer ! {Qref, MyKey},
            node(MyKey, Predecessor, Successor, Store,Next);
        {notify, NewPeer} ->
            {NewPredecessor,NewStore} = notify(NewPeer, MyKey, Predecessor,Store),
            node(MyKey, NewPredecessor, Successor, NewStore,Next);
        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(MyKey, Predecessor, Successor, Store,Next);
        {status, Pred, Nx} ->
          {NewSuccessor,NewNext} = stabilize(Pred, Nx, MyKey, Successor),
            node(MyKey, Predecessor, NewSuccessor, Store,NewNext);
        stabilize ->
            stabilize(Successor),
            node(MyKey, Predecessor, Successor, Store,Next);
        stop ->
            ok;
        probe ->
            create_probe(MyKey, Successor, Store),
            node(MyKey, Predecessor, Successor, Store,Next);
        {probe, MyKey, Nodes, T} ->
            remove_probe(MyKey, Nodes, T),
            node(MyKey, Predecessor, Successor, Store,Next);
        {probe, RefKey, Nodes, T} ->
            forward_probe(RefKey, [MyKey|Nodes], T, Successor, Store),
            node(MyKey, Predecessor, Successor, Store,Next);
		    {add, Key, Value, Qref, Client} ->
	    	    Added = add(Key, Value, Qref, Client,
            MyKey, Predecessor, Successor, Store),
	    	    node(MyKey, Predecessor, Successor, Added,Next);
	      {lookup, Key, Qref, Client} ->
			      lookup(Key, Qref, Client, MyKey, Predecessor, Successor, Store),
			      node(MyKey, Predecessor, Successor, Store,Next);
		    {handover, Elements} ->
			      Merged = storage:merge(Store, Elements),
			      node(MyKey, Predecessor, Successor, Merged,Next);
        {'DOWN', Ref, process, _, _} ->
            {NewPred, NewSucc, NewNext} = down(Ref, Predecessor, Successor, Next),
            node(MyKey, NewPred, NewSucc, Store, NewNext)

end.

stabilize(Pred, Nx, MyKey, Successor) ->
  {Skey, Ref, Spid} = Successor,
  case Pred of
      nil ->
          %% DONE: ADD SOME CODE
	  	    Spid ! {notify, {MyKey,self()}},
          {Successor,Nx};
      {MyKey, _} ->
          {Successor,Nx};
      {Skey, _} ->
          %% DONE: ADD SOME CODE
	  	    Spid ! {notify, {MyKey,self()}},
          {Successor,Nx};
      {Xkey, Xpid} ->
            case key:between(Xkey, MyKey, Skey) of
                true ->
                    %% DONE: ADD SOME CODE 
    		    	      Xpid ! {request, self()},
                    %% DONE: ADD SOME CODE
                    demonit(Ref),
                    Sref = monit(Xpid),
                    {{Xkey, Sref, Xpid},Successor};
                false ->
                    %% DONE: ADD SOME CODE
	  	    		      Spid ! {notify, {MyKey,self()}},
                    {Successor,Nx}
            end
    end.

stabilize({_, _,Spid}) ->
    Spid ! {request, self()}.

request(Peer, Predecessor, {Skey, _,Spid}) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil,{Skey, Spid}};
        {Pkey, _, Ppid} ->
            Peer ! {status, {Pkey, Ppid},{Skey, Spid}}
    end.

notify({Nkey, Npid}, MyKey, Predecessor, Store) ->
	case Predecessor of
		nil ->
			Keep = handover(Store, MyKey, Nkey, Npid),
			%% DONE: ADD SOME CODE
      Pref = monit(Npid),
			{{Nkey, Pref, Npid}, Keep};
		{Pkey, Ref, _} ->
			case key:between(Nkey, Pkey, MyKey) of
				true ->
					%% DONE: ADD SOME CODE
					Keep = handover(Store, MyKey, Nkey, Npid),
					%% DONE: ADD SOME CODE
          demonit(Ref),
          Pref = monit(Npid),
					{{Nkey, Pref, Npid}, Keep};
				false ->
					{Predecessor, Store}
			end
	end.

create_probe(MyKey, {_, _,Spid}, Store) ->
    Spid ! {probe, MyKey, [MyKey], erlang:monotonic_time()},
    io:format("Create probe ~w!~n", [MyKey]),
	io:format("Store: ~w~n", [Store]).
	
remove_probe(MyKey, Nodes, T) ->
    T2 = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(T2-T, native, millisecond),
    io:format("Received probe ~w in ~w ms Ring: ~w~n", [MyKey, Time, Nodes]).
	
forward_probe(RefKey, Nodes, T, {_, _,Spid}, Store) ->
    Spid ! {probe, RefKey, Nodes, T},
    io:format("Forward probe ~w!~n", [RefKey]),
	io:format("Store: ~w~n", [Store]).

add(Key, Value, Qref, Client, MyKey, {Pkey, _, _}, {_, _, Spid}, Store) ->
	case key:between(Key , Pkey , MyKey)  of    %% DONE: ADD SOME CODE
		true ->
			Added = lists:keystore(Key,1,Store,{Key,Value}),    %% DONE: ADD SOME CODE
			Client ! {Qref, ok},
			Added;
		false ->
			%% DONE
			Spid ! {add, Key, Value, Qref, Client},
			Store
	end.

lookup(Key, Qref, Client, MyKey, {Pkey, _, _}, {_, _, Spid}, Store) ->
	case key:between(Key , Pkey , MyKey)  of    %% DONE: ADD SOME CODE
		true ->
			Result = lists:keyfind(Key,1,Store) ,    %% DONE: ADD SOME CODE
			Client ! {Qref, Result};
		false ->
		%% DONE: ADD SOME CODE
		Spid ! {lookup, Key, Qref, Client}
	end.

handover(Store, MyKey, Nkey, Npid) ->
	{Keep, Leave} = storage:split(MyKey, Nkey, Store),
	Npid ! {handover, Leave},
	Keep.

monit(Pid) ->
  erlang:monitor(process, Pid).

demonit(nil) ->
  ok;

demonit(MonitorRef) ->
  erlang:demonitor(MonitorRef, [flush]).

down(Ref, {_, Ref, _}, Successor, Next) ->
  {nil, Successor, Next};

down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
  %% TODO: ADD SOME CODE
  Nref = monit(Npid),
  %% TODO: ADD SOME CODE
  self() ! stabilize,
  {Predecessor, {Nkey, Nref, Npid}, nil}.


