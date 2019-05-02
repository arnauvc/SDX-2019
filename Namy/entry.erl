-module(entry).
-export([lookup/2, add/3, remove/2]).

lookup(Req, Entries) ->
	case lists:keyfind(Req,1,Entries) of
		false -> unknown;
		{Req, Entry} -> Entry
	end.	

remove(Name, Entries) ->
	lists:keydelete(Name,1,Entries).


add(Name, Entry, Entries) ->
	lists:keystore(Name, 1, Entries, {Name, Entry}).

