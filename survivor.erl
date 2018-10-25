-module(survivor).
-export([start/0, entry/1, init/0]). 

start() ->
	register(survivor, spawn(?MODULE, init, [])).

entry(Data)-> 
	ets:insert(logboek, {{now(), self()}, Data}). 

init() -> 
	ets:new(logboek, [named_table, ordered_set, public]),		
	loop().

loop() -> 
	receive
		stop -> ok
	end. 

