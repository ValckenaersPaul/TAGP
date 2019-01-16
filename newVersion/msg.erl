-module(msg).
-define(TimeOut, 5000).
-export([get/2, get/3, set_ack/2, set_ack/3]).
-export([test/0]). 

get(Pid, Key) ->
	Pid ! {Key, replier(R = make_ref())},
	receive
		{R, Info} -> {ok, Info}
		after ?TimeOut -> {error, timed_out, Pid, Key, R}
	end.

get(Pid, Key, P_list) -> 
	Pid ! {Key, P_list, replier(R = make_ref())},
	receive
		{R, Info} -> {ok, Info}
		after ?TimeOut -> {error, timed_out, Pid, Key, R}
	end.
	
set_ack(Pid, Key) -> % identical to get; for readability only. 
	Pid ! {Key, replier(R = make_ref())},
	receive
		{R, Info} -> {ok, Info}
		after ?TimeOut -> {error, timed_out, Pid, Key, R}
	end.

set_ack(Pid, Key, P_list) -> % identical to get; for readability only. 
	Pid ! {Key, P_list, replier(R = make_ref())},
	receive
		{R, Info} -> {ok, Info}
		after ?TimeOut -> {error, timed_out, Pid, Key, R}
	end.
		
replier(Ref) ->  
    Sender = self(),
	fun(Msg) -> Sender ! {Ref, Msg} end.

test() -> 
	Pid = spawn(fun() -> receive {_Dummy, [P | _ ], F } -> F(2 * P) end, ok end),
	msg:set_ack(Pid, dummy, [10]). 

