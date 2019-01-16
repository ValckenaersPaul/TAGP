-module(demo2).
-export([init/0, do/0, mkA/0, get_root/1, get_aggr/1]).
-export([aggregate/0]).

init() ->
	observer:start(),
	survivor:start().

do() -> ok.

mkA() -> spawn(?MODULE, aggregate, []).

get_root(C) ->
	C ! {get_root, self()},
	receive 
		L -> {ok, L}
		after 5000 -> {error, timed_out}
	end. 

get_aggr(C) -> 
	C ! {get_aggr, self()}, 
	receive 
		A -> {ok, A}
		after 5000 -> {error, timed_out}
	end. 


get_type({ok, [ RI | _ ]}) -> resource_instance:get_type(RI). 

aggregate() -> 
	survivor:entry(test2_started),
	T = ets:new(configA, []),

	{ok, PT1} = pipeTyp:create(),
	ets:insert(T, {pipe_type_no1, PT1}),

	{ok, PI1} = pipeInst:create(self(), PT1), 
	ets:insert(T, {pipe_instance_no1, PI1}),
	{ok, PI2} = pipeInst:create(self(), PT1), 
	ets:insert(T, {pipe_instance_no2, PI2}),
	{ok, PI3} = pipeInst:create(self(), PT1), 
	ets:insert(T, {pipe_instance_no3, PI3}),
	{ok, AggrTyp_Pid} = simpleThermalCircuit_type:create(),
	{ok, AggrInst_Pid} = simpleThermalCircuit_instance:create(self(), AggrTyp_Pid, [PI1, PI2, PI3]),
	loop(AggrInst_Pid).

loop(AggrInst_Pid) -> 
	receive 
		{get_root, Pid} -> 
			Any = aggregate_instance:get_root(AggrInst_Pid),
			Pid ! Any, 
			loop(AggrInst_Pid);
	{get_aggr, Pid} -> 
		Pid ! AggrInst_Pid, 
		loop(AggrInst_Pid);
		stop -> ok
	end. 	

