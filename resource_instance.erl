-module(resource_instance).
-export([create/2]).
-export([list_connectors/1, list_locations/1]).
-export([get_type/1, get_ops/1, get_state/1]).
-export([get_flow_influence/1]).
%%% More to follow later. 

create(Selector, Environment) -> 
	apply(Selector, create, Environment).
	% returns {ok, ResInst_Pid}
	
list_connectors(ResInst_Pid)	-> 
	msg:get(ResInst_Pid, get_connectors).	
	
list_locations(ResInst_Pid)	-> % ResInst is hosting
	msg:get(ResInst_Pid, get_locations).

get_type(ResInst_Pid) -> % allows to retrieve state-agnostic information
	msg:get(ResInst_Pid, get_type).

get_ops(ResInst_Pid) -> % list of commands available in the current state
	% Does not lock the resource state; list may change at any time
	msg:get(ResInst_Pid, get_ops).

get_state(ResInst_Pid) -> % current state understood by type (only)
	% Does not lock the resource state; may change at any time
	msg:get(ResInst_Pid, get_state).

get_flow_influence(ResInst_Pid) -> 
	msg:get(ResInst_Pid, get_flow_influence).
