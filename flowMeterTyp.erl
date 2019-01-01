-module(flowMeterTyp).
-export([create/0, init/0]).
% -export([dispose/2, enable/2, new_version/2]).
% -export([get_initial_state/3, get_connections_list/2]). % use resource_type
% -export([update/3, execute/7, refresh/4, cancel/4, update/7, available_ops/2]). 

create() -> {ok, spawn(?MODULE, init, [])}.

init() -> 
	survivor:entry(flowMeterTyp_created),
	loop().

loop() -> 
	receive
		{initial_state, [MeterInst_Pid, [ResInst_Pid, RealWorldCmdFn]], ReplyFn} ->
			{ok, [L | _ ] } = resource_instance:list_locations(ResInst_Pid),
			{ok, Fluidum} = location:get_Visitor(L),
			ReplyFn(#{meterInst => MeterInst_Pid, resInst => ResInst_Pid, 
					  fluidum => Fluidum, rw_cmd => RealWorldCmdFn}), 
			loop();
		{measure_flow, State, ReplyFn} -> 
			#{rw_cmd := ExecFn} = State,
			ReplyFn(ExecFn()),
			loop(); 
		{estimate_flow, State, ReplyFn} -> 
			#{fluidum := F} = State, 
			{ok, C} = fluidumInst:get_resource_circuit(F),
			ReplyFn(computeFlow(C)),
			loop(); 
		{isOn, State, ReplyFn} -> 
			#{on_or_off := OnOrOff} = State, 
			ReplyFn(OnOrOff),
			loop()
	end. 

computeFlow(ResCircuit) -> 
 	Interval = {0, 10}, % ToDo >> discover upper bound for flow.
	{ok, InfluenceFnCircuit} = influence(maps:next(maps:iterator(ResCircuit)), []),
	compute(Interval, InfluenceFnCircuit).

influence({C, _ , Iter }, Acc) ->
		{ok, InflFn} = apply(resource_instance, get_flow_influence, [C]),
		influence(maps:next(Iter), [ InflFn | Acc ] );

influence(none, Acc) -> {ok, Acc}. 

compute({Low, High}, _InflFnCircuit) when (High - Low) < 1 -> 
	%Todo convergentiewaarde instelbaar maken. 
	(Low + High) / 2 ;
	
compute({Low, High}, InflFnCircuit) ->
	L = eval(Low, InflFnCircuit, 0),
	H = eval(High, InflFnCircuit, 0),
	L = eval(Low, InflFnCircuit, 0),
	H = eval(High, InflFnCircuit, 0),
	Mid = (H + L) / 2, M = eval(Mid, InflFnCircuit, 0),
	if 	M > 0 -> 
			compute({Low, Mid}, InflFnCircuit);
        true -> % works as an 'else' branch
            compute({Mid, High}, InflFnCircuit)
    end.

	
eval(Flow, [Fn | RemFn] , Acc) ->
	eval(Flow, RemFn, Acc + Fn(Flow));

eval(_Flow, [], Acc) -> Acc. 