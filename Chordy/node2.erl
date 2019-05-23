-module(node2).
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
    node(MyKey, Predecessor, Successor, storage:create()).

connect(MyKey, nil) ->
    {ok, {MyKey, self()}};    %% DONE: ADD SOME CODE
connect(_, PeerPid) ->
    Qref = make_ref(),
    PeerPid ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            {ok, {Skey, PeerPid}}    %% DONE: ADD SOME CODE
    after ?Timeout ->
        io:format("Timeout: no response from ~w~n", [PeerPid])
    end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

node(MyKey, Predecessor, Successor,Store) ->
    receive 
		delete -> 
			io:format("Delete OK"),
			node(MyKey, Predecessor, Successor,[]);
        {key, Qref, Peer} ->
            Peer ! {Qref, MyKey},
            node(MyKey, Predecessor, Successor, Store);
        {notify, NewPeer} ->
            {NewPredecessor,NewStore} = notify(NewPeer, MyKey, Predecessor,Store),
            node(MyKey, NewPredecessor, Successor, NewStore);
        {request, Peer} ->
            request(Peer, Predecessor),
            node(MyKey, Predecessor, Successor, Store);
        {status, Pred} ->
            NewSuccessor = stabilize(Pred, MyKey, Successor),
            node(MyKey, Predecessor, NewSuccessor, Store);
        stabilize ->
            stabilize(Successor),
            node(MyKey, Predecessor, Successor, Store);
        stop ->
            ok;
        probe ->
            create_probe(MyKey, Successor, Store),
            node(MyKey, Predecessor, Successor, Store);
        {probe, MyKey, Nodes, T} ->
            remove_probe(MyKey, Nodes, T),
            node(MyKey, Predecessor, Successor, Store);
        {probe, RefKey, Nodes, T} ->
            forward_probe(RefKey, [MyKey|Nodes], T, Successor, Store),
            node(MyKey, Predecessor, Successor, Store);
		{add, Key, Value, Qref, Client} ->
	    	Added = add(Key, Value, Qref, Client,
            MyKey, Predecessor, Successor, Store),
	    	node(MyKey, Predecessor, Successor, Added);
	    {lookup, Key, Qref, Client} ->
			lookup(Key, Qref, Client, MyKey, Predecessor, Successor, Store),
			node(MyKey, Predecessor, Successor, Store);
		{handover, Elements} ->
			Merged = storage:merge(Store, Elements),
			node(MyKey, Predecessor, Successor, Merged)
   end.

stabilize(Pred, MyKey, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
      nil ->
          %% DONE: ADD SOME CODE
	  	  Spid ! {notify, {MyKey,self()}},
          Successor;
      {MyKey, _} ->
          Successor;
      {Skey, _} ->
          %% DONE: ADD SOME CODE
	  	  Spid ! {notify, {MyKey,self()}},
          Successor;
      {Xkey, Xpid} ->
            case key:between(Xkey, MyKey, Skey) of
                true ->
                    %% DONE: ADD SOME CODE 
    		    	Xpid ! {request, self()},
                    %% DONE: ADD SOME CODE 
		    		{Xkey, Xpid};
                false ->
                    %% DONE: ADD SOME CODE
	  	    		Spid ! {notify, {MyKey,self()}},
                    Successor
            end
    end.

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

notify({Nkey, Npid}, MyKey, Predecessor, Store) ->
	case Predecessor of
		nil ->
			Keep = handover(Store, MyKey, Nkey, Npid),
			%% DONE: ADD SOME CODE
			{{Nkey, Npid}, Keep};
		{Pkey,  _} ->
			case key:between(Nkey, Pkey, MyKey) of
				true ->
					%% DONE: ADD SOME CODE
					Keep = handover(Store, MyKey, Nkey, Npid),
					%% DONE: ADD SOME CODE
					{{Nkey, Npid}, Keep};
				false ->
					{Predecessor, Store}
			end
	end.

create_probe(MyKey, {_, Spid}, Store) ->
    Spid ! {probe, MyKey, [MyKey], erlang:monotonic_time()},
    io:format("Create probe ~w!~n", [MyKey]),
	io:format("Store: ~w~n", [Store]).
	
remove_probe(MyKey, Nodes, T) ->
    T2 = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(T2-T, native, millisecond),
    io:format("Received probe ~w in ~w ms Ring: ~w~n", [MyKey, Time, Nodes]).
	
forward_probe(RefKey, Nodes, T, {_, Spid}, Store) ->
    Spid ! {probe, RefKey, Nodes, T},
    io:format("Forward probe ~w!~n", [RefKey]),
	io:format("Store: ~w~n", [Store]).

add(Key, Value, Qref, Client, MyKey, {Pkey, _}, {_, Spid}, Store) ->
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

lookup(Key, Qref, Client, MyKey, {Pkey, _}, {_, Spid}, Store) ->
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
