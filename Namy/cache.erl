-module(cache).
-export([lookup/2, add/4, remove/2, purge/1]).

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

purge(Cache) ->
	Now = erlang:monotonic_time(),
	Time = erlang:convert_time_unit(Now, native, second),
	%lists:foreach(fun({Name, Reply, Expire}) ->
	%	if Expire >= Time -> 
	%		NewerCache = lists:keystore(Name,1,NewCache,{Name,Reply,Expire})
%		end
 %       end,
  %      Cache),
%	NewerCache.
	purge_rec(Cache, Time, []).

purge_rec([],_,NewResCache) ->
	NewResCache;

purge_rec([{Name,Reply,Expire} | Cache], Time, ResCache) ->
	if Expire >= Time ->
		NewResCache = lists:keystore(Name,1,ResCache,{Name,Reply,Expire})
	end,
	NewCache = lists:keydelete(Name,1,Cache),
	purge_rec(NewCache, Time, NewResCache).


