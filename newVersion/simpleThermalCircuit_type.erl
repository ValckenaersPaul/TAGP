-module(simpleThermalCircuit_type).
-export([create/0, add/3,remove/3]).
-export([init/0]). 

create() ->
	{ok, spawn(?MODULE, init, [])}.

init() ->
	survivor:entry(simpleThermalCircuit_type_created),
	loop(). 

add(_AggrTyp_Pid, State, _InsertSpec) -> 
	{ok, State}. % Not yet implemented. 
%	msg:set_ack(AggrTyp_Pid, add, {State, InsertSpec}). 

remove(_AggrTyp_Pid, State, _ToBeRemoved) -> 
	{ok, State}. % Not yet implemented. 
%	msg:set_ack(AggrTyp_Pid, remove, ToBeRemoved). 

loop() ->
	receive 
		{get_initial_state, {Instance_Pid, CircuitList}, ReplyFn} -> 
			{ok, Root} = constructInitialCircuit(Instance_Pid, CircuitList),
			State = #{root => Root, instance => Instance_Pid, circuitList => CircuitList}, 
			ReplyFn(State), loop();
		{get_root, State, ReplyFn} -> 
			#{root := Root} = State, ReplyFn(Root), loop();
		Any -> 
			survivor:entry({notImplemented, Any}), loop()
	end. 

constructInitialCircuit(Instance_Pid, [FirstElement | OtherElements]) -> 
	{ok, LastElement} = constructCircuit(Instance_Pid, [FirstElement | OtherElements], FirstElement),
	mk_connection(LastElement, FirstElement), 
	resource_instance:set_host(LastElement, Instance_Pid),
	% todo: set the aggregate to be the Host of FirstElement. 
	{ok, FirstElement}.

constructCircuit(Instance_Pid, [FirstElement, NextElement | RemainingElements], FirstElement) -> 
	mk_connection(FirstElement, NextElement), 
	resource_instance:set_host(FirstElement, Instance_Pid),
	% todo: set the aggregate to be the Host of NextElement. 
	constructCircuit(Instance_Pid, [NextElement | RemainingElements], NextElement);

constructCircuit(_Instance_Pid, [LastElement], LastElement) -> 
	{ok, LastElement}. 

mk_connection(PI_out, PI_in) ->
	{ok, [_, C_out]} = resource_instance:list_connectors(PI_out),
	{ok, [C_in, _ ]} = resource_instance:list_connectors(PI_in),
	connector:connect(C_out, C_in),
	connector:connect(C_in, C_out). 