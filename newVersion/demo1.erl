-module(demo1).
-export([init/0, do/0, mkC/0, get_list/1, get_type/1]).
-export([pipeCircuit/0]).

init() ->
	observer:start(),
	survivor:start().

do() -> ok.

mkC() -> spawn(?MODULE, pipeCircuit, []).

get_list(C) ->
	C ! {list_instances, self()},
	receive 
		L -> {ok, L}
		after 5000 -> {error, timed_out}
	end. 

get_type({ok, [ RI | _ ]}) -> resource_instance:get_type(RI). 

pipeCircuit() -> 
	survivor:entry(test1_started),
	T = ets:new(config, []),

	{ok, PT1} = pipeTyp:create(),
	ets:insert(T, {pipe_type_no1, PT1}),

	{ok, PI1} = pipeInst:create(self(), PT1), 
	ets:insert(T, {pipe_instance_no1, PI1}),
	{ok, PI2} = pipeInst:create(self(), PT1), 
	ets:insert(T, {pipe_instance_no2, PI2}),
	{ok, PI3} = pipeInst:create(self(), PT1), 
	ets:insert(T, {pipe_instance_no3, PI3}),
	loop([PI1, PI2, PI3]).

loop(L) -> 
	receive 
		{list_instances, Pid} -> 
			Pid ! L, loop(L);
		stop -> ok
	end. 	

