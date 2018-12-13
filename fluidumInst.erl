-module(fluidumInst).

-export([create/2, init/2, get_resource_circuit/1]).

create(Root_ConnectorPid, ResTyp_Pid) -> 
	{ok, spawn(?MODULE, init, [Root_ConnectorPid, ResTyp_Pid])}.

init(Root_ConnectorPid, ResTyp_Pid) -> 
	{ok, State} = apply(resource_type, get_initial_state, [ResTyp_Pid, self(), [Root_ConnectorPid, plain_water]]),
	survivor:entry({ fluidInst_created, State }),
	loop(Root_ConnectorPid, State, ResTyp_Pid).

get_resource_circuit(ResInstPid) ->
	msg:get(ResInstPid, get_resource_circuit). 

loop(Root_ConnectorPid, State, ResTyp_Pid) -> 
	receive
		{get_locations, ReplyFn} ->
			{ok, L_List} = resource_type:get_locations_list(ResTyp_Pid, State), 
			ReplyFn(L_List),
			loop(Root_ConnectorPid, State, ResTyp_Pid);
		{get_type, ReplyFn} ->
			ReplyFn(ResTyp_Pid),
			loop(Root_ConnectorPid, State, ResTyp_Pid);
		{get_resource_circuit, ReplyFn} -> 
			{ok, C} = fluidumTyp:get_resource_circuit(ResTyp_Pid, State),
			ReplyFn(C), 
			loop(Root_ConnectorPid, State, ResTyp_Pid)
	end.
