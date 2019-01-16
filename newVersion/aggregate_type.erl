-module(aggregate_type).
-export([create/2, get_initial_state/3, get_root/2, add/3, remove/3]).

create(Selector, Param) -> apply(Selector, create, Param).
% returns {ok, AggregateType_Pid}.

get_initial_state(AggrTyp_Pid, AggrInst_Pid, TypeOptions) -> 
	msg:get(AggrTyp_Pid, get_initial_state, {AggrInst_Pid, TypeOptions}). 

get_root(AggrTyp_Pid, State) -> 
	msg:get(AggrTyp_Pid, get_root, State). 


add(_AggrTyp_Pid, State, _InsertSpec) -> 
	{ok, State}. % Not yet implemented. 
%	msg:set_ack(AggrTyp_Pid, add, {State, InsertSpec}). 

remove(_AggrTyp_Pid, State, _ToBeRemoved) -> 
	{ok, State}. % Not yet implemented. 
%	msg:set_ack(AggrTyp_Pid, remove, ToBeRemoved). 