-module(cache).
-export([lookup/2, add/4, remove/2]).

lookup(Name, Cache) -> 
	Now = erlang:monotonic_time(),
	Time = erlang:convert_time_unit(Now, native, second),
	case lists:keyfind(Name,1,Cache) of
		false -> unknown;
                {Name, _, Expire} when Expire < Time -> invalid; 
                {Name, Reply, Expire} when Expire >= Time -> Reply
	end.

add(FullDomain, Expire, Reply, Cache) ->
	lists:keystore(FullDomain,1,Cache,{FullDomain,Reply,Expire}).

remove(Name, Cache) ->
        lists:keydelete(Name,1,Cache).
