-module(aggregate_instance).
-export([create/2, add/3, remove/2, get_root/1]).

create(Selector, Param) -> apply(Selector, create, Param).
% returns {ok, AggregateInst_Pid}.

add(_AggrInst_Pid, _AggrElem_ConnectorSpec, _NewElem_Connector_Spec) -> 
	{nok, not_implemented}. % returns {ok, NewState}. 
%	msg:set_ack(AggrInst_Pid, add, #{insertLocation => AggrElem_ConnectorSpec, insertElement => NewElem_Connector_Spec}). 

remove(_AggrInst_Pid, _ToBeRemoved_Pid) -> 
	{nok, not_implemented}. % returns {ok, NewState}.
%	msg:set_ack(AggrInst_Pid, remove, ToBeRemoved_Pid).

get_root(AggrInst_Pid) -> 
	msg:get(AggrInst_Pid, get_root). 
