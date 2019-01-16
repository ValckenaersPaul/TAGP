-module(test1).
-export([start/0, init/0, mkC/0, mkPC/0, mk_connection/2, mk_loop/1]).
-export([pipeCircuit/0, pumpCircuit/0]). 

init() ->
	observer:start(),
	survivor:start().

mkPC() -> spawn(?MODULE, pumpCircuit, []).

mkC() -> spawn(?MODULE, pipeCircuit, []).

pumpCircuit() -> 
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
    RWCmdFn = fun(Cmd) -> ets:insert(T, {pumpCmd, Cmd, self()}) end,
%   RWCmdFn = fun(_Cmd) -> survivor:entry({pumpCmd, self()}) end,
	{ok, PI1} = pumpInst:create(self(), PumpT1, PI4Pump, RWCmdFn), 
%--------------------------------------------
	{ok, PI4FlowMeter} = pipeInst:create(self(), PT1),
	ets:insert(T, {pipe_instance_no2, PI4FlowMeter}),
	{ok, PI2} =  flowMeterInst:create(self(), FlowMeterT1, PI4FlowMeter, fun() -> no_data end), 
	ets:insert(T, {flowMeter_instance_no2, PI2}),
%---------------------------------------------
	{ok, PI3} = pipeInst:create(self(), PT1), 
	ets:insert(T, {pipe_instance_no3, PI3}),
	mk_loop([PI1, PI2, PI3]),
	pumpInst:switch_on(PI1),
	survivor:entry(flowMeterInst:estimate_flow(PI2)),
	receive 
		stop -> ok
	end. 
	
pipeCircuit() -> 
	survivor:entry(test1_started),
	T = ets:new(config, []),
	{ok, PT1} = pipeTyp:create(),
	ets:insert(T, {pipe_type_no1, PT1}),
%	survivor:entry({pipeTyp, PT1}),
	{ok, PI1} = pipeInst:create(self(), PT1), 
	ets:insert(T, {pipe_instance_no1, PI1}),
%	survivor:entry({pipeInst, PI1}),
	{ok, PI2} = pipeInst:create(self(), PT1), 
	ets:insert(T, {pipe_instance_no2, PI2}),
	{ok, PI3} = pipeInst:create(self(), PT1), 
	ets:insert(T, {pipe_instance_no3, PI3}),
	mk_loop([PI1, PI2, PI3]),
	receive 
		stop -> ok
	end. 	

start() -> 
	survivor:entry(test1_started),
	T = ets:new(config, []),
	{ok, PT1} = pipeTyp:create(),
	ets:insert(T, {pipe_type_no1, PT1}),
%	survivor:entry({pipeTyp, PT1}),
	{ok, PI1} = pipeInst:create(self(), PT1), 
	ets:insert(T, {pipe_instance_no1, PI1}),
%	survivor:entry({pipeInst, PI1}),
	{ok, PI2} = pipeInst:create(self(), PT1), 
	ets:insert(T, {pipe_instance_no2, PI2}),
	{ok, PI3} = pipeInst:create(self(), PT1), 
	ets:insert(T, {pipe_instance_no3, PI3}),
	mk_loop([PI1, PI2, PI3]),
	timer:sleep(10000),
	{PI1, PT1}. 

mk_connection(PI_out, PI_in) ->
	{ok, [_, C_out]} = resource_instance:list_connectors(PI_out),
	{ok, [C_in, _ ]} = resource_instance:list_connectors(PI_in),
	connector:connect(C_out, C_in),
	connector:connect(C_in, C_out). 

mk_loop([ First | Remainder ]) -> 
	mk_loop([ First | Remainder ], First).

mk_loop([First | [Second | Remainder ]], Start) -> 
	mk_connection(First, Second),
	mk_loop([ Second | Remainder ], Start);

mk_loop([Last], Start) -> 
	mk_connection(Last, Start).
	


	