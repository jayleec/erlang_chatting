%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---

-module(chat_group).
-import(lib_chan_mm, [send/2, controller/2]).
-import(lists, [foreach/2, reverse/2, map/2]).

-export([start/2]).

start(C, Nick) ->
    process_flag(trap_exit, true),
    controller(C, self()),
    send(C, ack),
    self() ! {chan, C, {relay, Nick, "I'm starting the group"}},
    group_controller([{C,Nick}]).



delete(Pid, [{Pid,Nick}|T], L) -> {Nick, reverse(T, L)};
delete(Pid, [H|T], L)          -> delete(Pid, T, [H|L]);
delete(_, [], L)               -> {"????", L}.

list_to_str(L)  -> 
	lists:flatten(io_lib:format("~p ", [L])).

group_name_list(L)  ->
	% P = fun({_, B}, Acc)  -> io:format("~p~n",[B]), [B|Acc] end
	% lists:foldr(fun({_, B}, Acc) -> [B | Acc] end, [], L).
	lists:foldr(fun({_, B}, Acc)  -> io_lib:format("~p  ",[B]), [B|Acc] end, [], L).


group_controller([]) ->
    exit(allGone);
group_controller(L) ->
    receive
	{chan, C, {relay, Nick, Str}} ->
	    foreach(fun({Pid,_}) -> send(Pid, {msg,Nick,C,Str})end, L),
		% io:format("Group Name List: ~s~n", [group_name_list(L)]),
		self() ! {chan,C, {names, list_to_str(group_name_list(L))}},
	    group_controller(L);
	{login, C, Nick} ->
	    controller(C, self()),
	    send(C, ack),
	    self() ! {chan, C, {relay, Nick, "I'm joining the group"}},
	    group_controller([{C,Nick}|L]);
	{chan_closed, C} ->
	    {Nick, L1} = delete(C, L, []),
	    self() ! {chan, C, {relay, Nick, "I'm leaving the group"}},
	    group_controller(L1);
	{chan, C, {names, Str}}  ->
	    foreach(fun({Pid,_}) -> send(Pid, {names,Str})end, L),
		% io:format("{chan, C {names, Str}}~p~n", [Str]),
		group_controller(L);
	{groups, Str}  ->
		foreach(fun({Pid,_}) -> send(Pid, {groups,Str})end, L),
		io:format("{chan, C {groups, Str}}~p~n", [list_to_str(Str)]),
		group_controller(L);
	{chan, C, {oneToOne, From, To, Str}}  ->
		% TODO: for if Pid == To send message
		Grouplist = group_name_list(L),
		% io:format("group-tuple list: ~p~n", [L]),
		% io:format("TEST: ~p~n",[lists:member(To,Grouplist)]),
		TargetTuple = lists:keyfind(To, 2, L),
		io:format("targetpid: ~p~n", [TargetTuple]),
		% Str1 = lists:sublist(string:tokens(Str," ") ,3, 100),
		case TargetTuple of
			{Pid,_}  -> send(Pid, {oneToOne, From, C, Str})
		end,
	    % foreach(fun({Pid,_}) -> send(Pid, {msg,From,C,Str}),
		% io:format("Pid test print: ~p~n", [Pid])
		% end,	
		%  L),
		group_controller(L);
	Any ->
	    io:format("group controller received Msg=~p~n", [Any]),
	    group_controller(L)
    end.

