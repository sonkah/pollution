%% @author Ania
%% @doc @todo Add description to pollution_server.


-module(pollution_server).

%% ====================================================================
%% API functions
%% ====================================================================
-import(pollution, [createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getMinimumDistanceStation/2]).
%-export([]).
-compile[export_all].
start() ->
	register(server, spawn (polution_server, init, [])).
	
stop() ->
	server ! quit.

init(A) -> 
	Monitor = pollution:createMonitor(),
	loop(Monitor).

loop(Monitor) ->
	receive
		{addStation, Name, {X, Y}} -> 
			 case addStation(Name, {X, Y}, Monitor) of
				 NewState -> io:format("dziala ~n") , loop(NewState)
			 end;			 
		
		{addValue, {X, Y}, Time, Type, Value} ->
			case addValue({X, Y}, Time, Type, Value, Monitor) of
				NewState -> io:format("dziala~n"), loop(NewState)
				%_ -> io:format("co~n")
			end
	end.

addStat(Name, {X, Y}, Monitor) -> 
	server ! {addStation, Name, {X, Y}, Monitor},
	receive
		M->M
	end.


	%	case removeValue
	%	case getOneValue

	%	case getDailyMean

	%	case getStationMean
%		case getMinimumDistanceStation

%% ====================================================================
%% Internal functions
%% ====================================================================


