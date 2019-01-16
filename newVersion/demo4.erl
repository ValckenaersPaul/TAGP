-module(demo4).
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

aggregate() -> 
	survivor:entry(test1_started),
	T = ets:new(config, [public]),
%------------------------------------------
	{ok, PT1} = pipeTyp:create(),
	ets:insert(T, {pipe_type_no1, PT1}),
	
	{ok, PumpT1} = pumpTyp:create(),
	ets:insert(T, {pump_type_no1, PumpT1}),

	{ok, FlowMeterT1} = flowMeterTyp:create(),
	ets:insert(T, {flowMeterTyp_no1, FlowMeterT1}), 
%-------------------------------------------
	{ok, PI4Pump} = pipeInst:create(self(), PT1), 
	ets:insert(T, {pipe_instance_no1, PI4Pump}),
	timer:sleep(1000), 
    RWCmdFn = fun(Cmd) -> ets:insert(T, {pumpCmd, Cmd, self()}) end,
	{ok, PI1} = pumpInst:create(self(), PumpT1, PI4Pump, RWCmdFn), 
	ets:insert(T, {pump_instance_no1, PI1}),
%--------------------------------------------
	{ok, PI4FlowMeter} = pipeInst:create(self(), PT1),
	ets:insert(T, {pipe_instance_no2, PI4FlowMeter}),
	timer:sleep(1000), 
	{ok, PI2} =  flowMeterInst:create(self(), FlowMeterT1, PI4FlowMeter, fun() -> no_data end), 
	ets:insert(T, {flowMeter_instance_no1, PI2}),
%---------------------------------------------
	{ok, PI3} = pipeInst:create(self(), PT1), 
	ets:insert(T, {pipe_instance_no3, PI3}),
%---------------------------------------------
	{ok, AggrTyp_Pid} = simpleThermalCircuit_type:create(),
	{ok, AggrInst_Pid} = simpleThermalCircuit_instance:create(self(), AggrTyp_Pid, [PI1, PI2, PI3]),
%---------------------------------------------	
	{ok, FluidumTyp_Pid} = fluidumTyp:create(),
	{ok, [Root_ConnectorPid | _ ]} = resource_instance:list_connectors(PI1),
	survivor:entry({fluidumInstAbout2Bcreated, FluidumTyp_Pid, Root_ConnectorPid}),
	{ok, FluidumInst_Pid} = fluidumInst:create(Root_ConnectorPid, FluidumTyp_Pid),
	timer:sleep(1000),
	fluidumInst:load_circuit(FluidumInst_Pid), 
	pumpInst:switch_on(PI1), 
	loop(AggrInst_Pid, FluidumInst_Pid).

loop(AggrInst_Pid, FluidumInst_Pid) -> 
	receive 
		{get_root, Pid} -> 
			Any = aggregate_instance:get_root(AggrInst_Pid),
			Pid ! Any, 
			loop(AggrInst_Pid, FluidumInst_Pid);
		{get_aggr, Pid} -> 
			Pid ! AggrInst_Pid, 
			loop(AggrInst_Pid, FluidumInst_Pid);
		stop -> ok
	end. 	

