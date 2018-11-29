-module(fluidumTyp).

-export([create/0, init/0, discover_circuit/1]).

create() -> spawn(?MODULE, init, []).

init() -> 
	survivor:entry(fluidTyp_created), 
	loop().

loop() -> 
	receive
		{initial_state, [ResInst_Pid, [Root_ConnectorPid, TypeOptions]], ReplyFn} -> 
			{ok, C} = discover_circuit(Root_ConnectorPid), 
			ReplyFn(#{resInst => ResInst_Pid, circuit => C, typeOptions => TypeOptions}), 
			loop();
		{connections_list, _State , ReplyFn} -> 
			ReplyFn([]), 
			loop();
		{locations_list, _State, ReplyFn} -> 
			ReplyFn([]),
			loop()
	end. 


discover_circuit(Root_Pid) -> 
	{ok,  Circuit} = discover_circuit([Root_Pid], #{  }),
	{ok, {Root_Pid, Circuit}}.

discover_circuit([ disconnected | Todo_List], Circuit) -> 
	discover_circuit(Todo_List, Circuit);

discover_circuit([C | Todo_List], Circuit) -> 
	{ok, Updated_Todo_list, Updated_Circuit} = 
		process_connection(C, maps:find(C, Circuit ), Todo_List, Circuit),
	discover_circuit(Updated_Todo_list, Updated_Circuit);

discover_circuit([], Circuit) ->
	{ ok, Circuit }.

process_connection(C, error, Todo_List, Circuit) -> 
	Updated_Circuit = Circuit#{ C => processed },
    {ok, CC} = connector:get_connected(C),
	Updated_Todo_list = [ CC | Todo_List],
	{ok, ResPid} = connector:get_ResInst(C),
	{ok, C_list} = resource_instance:list_connectors(ResPid),
	{ok, C_list ++  Updated_Todo_list, Updated_Circuit};

process_connection( _, _ , Todo_List, Circuit) -> 
	{ok, Todo_List, Circuit}.





