-module(pumpInst).
-export([create/4, init/4, switch_on/1, switch_off/1, is_on/1, flow_influence/1]).
% -export([commission/1, activate/1]).
% -export([deactivate/1, decommission/1]).

% Pump is a pipe and more; this pipe instance is passed to the create function.
% RealWorldCmdFn is a function to transfer commands to the real-world pump. 

create(Host, PumpTyp_Pid, PipeInst_Pid, RealWorldCmdFn) -> {ok, spawn(?MODULE, init, [Host, PumpTyp_Pid, PipeInst_Pid, RealWorldCmdFn])}.

init(Host, PumpTyp_Pid, PipeInst_Pid, RealWorldCmdFn) -> 
	{ok, State} = apply(resource_type, get_initial_state, [PumpTyp_Pid, self(),     [PipeInst_Pid, RealWorldCmdFn]]),
									%  get_initial_state  (ResTyp_Pid,  ResInst_Pid, TypeOptions) 
	survivor:entry({ pumpInst_created, State }),
	loop(Host, State, PumpTyp_Pid, PipeInst_Pid).

switch_off(PumpInst_Pid) ->
	PumpInst_Pid ! switchOff. 

switch_on(PumpInst_Pid) ->
	PumpInst_Pid ! switchOn. 

is_on(PumpInst_Pid) -> 
	msg:get(PumpInst_Pid, isOn).

flow_influence(PumpInst_Pid) -> 
	msg:get(PumpInst_Pid, get_flow_influence).


loop(Host, State, PumpTyp_Pid, PipeInst_Pid) -> 
	receive
		switchOn -> 
			{ok, NewState} = msg:set_ack(PumpTyp_Pid, switchOn, State),
			loop(Host, NewState, PumpTyp_Pid, PipeInst_Pid);
		switchOff -> 
			{ok, NewState} = msg:set_ack(PumpTyp_Pid, switchOff, State), 
			loop(Host, NewState, PumpTyp_Pid, PipeInst_Pid);
		{isOn, ReplyFn} -> 
			{ok, Answer} = msg:get(PumpTyp_Pid, isOn, State), 
			ReplyFn(Answer), 
			loop(Host, State, PumpTyp_Pid, PipeInst_Pid);
		{get_type, ReplyFn} -> 
			ReplyFn(PumpTyp_Pid),
			loop(Host, State, PumpTyp_Pid, PipeInst_Pid);
		{get_flow_influence, ReplyFn} ->
			{ok, InfluenceFn} = msg:get(PumpTyp_Pid, flow_influence, State),
			ReplyFn(InfluenceFn),
			loop(Host, State, PumpTyp_Pid, PipeInst_Pid);
		OtherMessage -> 
			PipeInst_Pid ! OtherMessage,
			loop(Host, State, PumpTyp_Pid, PipeInst_Pid)
	end.
