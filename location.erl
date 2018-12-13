-module(location).
-export([create/2, get_ResInst/1, get_Visitor/1, get_Type/1, arrival/2, departure/1, dispose/1]).
-export([init/2]). 

create(ResInst_Pid, LocationTyp_Pid) ->
	spawn(?MODULE, init, [ResInst_Pid, LocationTyp_Pid]).
	
init(ResInst_Pid, LocationTyp_Pid) ->
	loop(ResInst_Pid, LocationTyp_Pid, vacant).
	
get_ResInst(Location_Pid) -> 
	msg:get(Location_Pid, get_ResInst).

get_Visitor(Location_Pid) ->
	msg:get(Location_Pid, get_Visitor).
	
get_Type(Location_Pid) ->
	msg:get(Location_Pid, get_Type).
	
arrival(Location_Pid, Visitor_Pid) ->
	Location_Pid ! {arrived, Visitor_Pid}. 

departure(Location_Pid) ->
	Location_Pid ! departed. 

dispose(Location_Pid) ->
	Location_Pid ! remove. 

loop(ResInst_Pid, LocationTyp_Pid, Visitor_Pid) -> 
	receive
		{get_ResInst, ReplyFn} -> 
			ReplyFn(ResInst_Pid),
			loop(ResInst_Pid, LocationTyp_Pid, Visitor_Pid);
		{get_Visitor, ReplyFn} -> 
			ReplyFn(Visitor_Pid),
			loop(ResInst_Pid, LocationTyp_Pid, Visitor_Pid);
		{get_Type, ReplyFn} -> 
			ReplyFn(LocationTyp_Pid),
			loop(ResInst_Pid, LocationTyp_Pid, Visitor_Pid);
		{arrived, V_Pid} ->
			loop(ResInst_Pid, LocationTyp_Pid, V_Pid);
		departed -> 
			loop(ResInst_Pid, LocationTyp_Pid, vacant);
		remove -> 
			stopped
	end. 