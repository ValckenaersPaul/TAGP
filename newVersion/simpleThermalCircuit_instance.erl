-module(simpleThermalCircuit_instance).
-export([create/3]).
-export([init/3]). 

create(Host, AggrTyp_Pid, CircuitList) ->
	{ok, spawn(?MODULE, init, [Host, AggrTyp_Pid, CircuitList])}.

init(Host, AggrTyp_Pid, CircuitList) ->
	{ok, State} = aggregate_type:get_initial_state(AggrTyp_Pid, self(), CircuitList),
% 	i.e.  >>> msg:get(AggrTyp_Pid, initial_state, {self(), CircuitList})
	survivor:entry({aggregate_created, self(), AggrTyp_Pid}),
	loop(Host, State, AggrTyp_Pid).

loop(Host, State, AggrTyp_Pid) ->
	survivor:entry({stc_loop, State}),
	receive
		{get_root, ReplyFn} -> 
			survivor:entry({get_root, State}),
			{ok, Root} = aggregate_type:get_root(AggrTyp_Pid, State),
			ReplyFn(Root), loop(Host, State, AggrTyp_Pid);
		{add, InsertSpec} -> 
			{ok, NewState} = simpleThermalCircuit_type:add(AggrTyp_Pid, State, InsertSpec),
			loop(Host, NewState, AggrTyp_Pid);
		{remove, ToBeRemoved} -> 
			{ok, NewState} = simpleThermalCircuit_type:remove(AggrTyp_Pid, State, ToBeRemoved),
			loop(Host, NewState, AggrTyp_Pid)
	end.