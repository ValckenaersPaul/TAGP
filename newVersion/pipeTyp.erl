-module(pipeTyp).
-export([create/0, init/0, get_flow_influence/2]). % More to be added later. 

create() -> {ok, spawn(?MODULE, init, [])}.

init() -> 
	survivor:entry({pipeTyp_created, self()}),
	loop().

get_flow_influence(TypePid, State) -> 
	msg:get(TypePid, get_flow_influence, State).

loop() -> 
	receive
		{initial_state, {ResInst_Pid, TypeOptions}, ReplyFn} ->
			Location = location:create(ResInst_Pid, emptySpace),
			In = connector:create(ResInst_Pid, simplePipe),
			Out = connector:create(ResInst_Pid, simplePipe),
			ReplyFn(#{resInst => ResInst_Pid, chambers => [Location], 
					cList => [In, Out], typeOptions => TypeOptions}), 
			loop();
		{connections_list, State , ReplyFn} -> 
			#{cList := C_List} = State, ReplyFn(C_List), 
			loop();
		{locations_list, State, ReplyFn} -> 
			#{chambers := L_List} = State, ReplyFn(L_List),
			loop();
		{get_flow_influence, _State, ReplyFn} -> 
			FlowInfluenceFn = fun(Flow) -> flow(Flow) end, % placeholder only. 
			ReplyFn(FlowInfluenceFn), 
			loop()
	end. 


flow(N) -> - 0.01 * N. 