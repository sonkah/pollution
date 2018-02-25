%% @author Ania
%% @doc @todo Add description to pollution.


-module(pollution).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).
%-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3,
% getDailyMean/3, ]).

-record(coords, {x,y}).
-record(rec, {type, time, value}).
-record(station, {name, coords, rec = []}).

%	createMonitor/0 - tworzy i zwraca nowy monitor zanieczyszczen;
createMonitor() -> [].


%	addStation/3 - dodaje do monitora wpis o nowej stacji pomiarowej (nazwa i wspolrzedne geograficzne), zwraca zaktualizowany monitor;
addStation(Name, {X, Y}, Monitor) -> 
	case existingStation(Monitor, Name, {X, Y}) of
		true -> "This station already exists.";
		false -> [#station{name = Name, coords = #coords{x = X, y = Y}} | Monitor]
	end.

% ======
%	addValue/5 - dodaje odczyt ze stacji (wspolrzedne geograficzne lub nazwa stacji, data, typ pomiaru, wartosc), zwraca zaktualizowany monitor;
% ======

addValue({X, Y}, Time, Type, Value, Monitor) -> 
	case couldBeAdded({X, Y}, Time, Type, Value, Monitor) of 
		true -> addVal({X, Y}, Time, Type, Value, Monitor);
		_ -> "Wrong coords or this record already exists."
	end;
addValue(Name, Time, Type, Value, Monitor) -> 
	case couldBeAdded(Name, Time, Type, Value, Monitor) of 
		true -> addVal(Name, Time, Type, Value, Monitor);
		_ -> "Wrong station name or this record already exists."
	end.



% ======
%	removeValue/4 - usuwa odczyt ze stacji (wspolrzedne geograficzne lub nazwa stacji, data, typ pomiaru), zwraca zaktualizowany monitor;
% ======


removeValue({X, Y}, Time, Type, Monitor) ->	
	case couldBeRemoved({X, Y}, Time, Type, Monitor) of
		true -> "can";%remVal({X, Y}, Time, Type, Monitor);
		_ -> "This record cannot be removed"
	end;
removeValue(Name, Time, Type, Monitor) ->
	case couldBeRemoved(Name, Time, Type, Monitor) of
		true -> "can";%remVal(Name, Time, Type, Monitor);
		_ -> "This record can't be removed"
	end.

couldBeRemoved({X, Y}, Time, Type, Monitor) -> 
	lists:any(fun(#station{coords = #coords{x = X1, y = Y1}, rec = Rec}) ->
    (X == X1) and
	(Y == Y1) and 
	(lists:any(fun(#rec{type = T, time = T1}) -> 
		(T == Type) and 
		(T1 == Time) 
		end, Rec)) end, Monitor);
couldBeRemoved(Name, Time, Type, Monitor) -> 
	lists:any(fun(#station{name = Name1, rec = Rec}) -> 
		(Name == Name1) and
		(lists:any(fun(#rec{type = T, time = Time1}) -> 
			(T == Type) and 
			(Time1 == Time) 
			end, Rec))
			end, Monitor).
   
remVal(_, _, _, []) -> [];
remVal({_, _}, _, _, []) -> [];
remVal({X, Y}, Time, Type, [H = #station{coords = #coords{x = X, y = Y}} |T]) ->
  [#station{name = H#station.name, coords = #coords{x = X, y = Y},
    rec = lists:filter((fun(#rec{type = Type1, time = Time1}) -> (Type1 /= Type) and (Time1 /= Time) end), H#station.rec)} | T];
remVal({X, Y}, Time, Type, [H|T]) -> [H | removeValue({X,Y}, Time, Type, T)];
remVal(Name, Time, Type, [H = #station{name = Name} | T]) ->
  [#station{name = Name, coords = #coords{x = H#station.coords#coords.x, y = H#station.coords#coords.y},
    rec = lists:filter((fun(#rec{type = Type1, time = Time1}) -> (Type1 /= Type) and (Time1 /= Time) end), H#station.rec)} | T];
remVal(Name, Time, Type, [H|T]) -> [H | remVal(Name, Time, Type, T)].


% ======
%	getOneValue/4 - zwraca wartosc pomiaru o zadanym typie, z zadanej daty i stacji;
% ======

getOneValue(_, _, _, []) -> "Nie ma takiego pomiaru.";
getOneValue(Name, Time, Type, [H = #station{name = Name} | _]) -> getValue(lists:filter((fun (#rec {time = Time1, type = Type1}) -> 
	(Type == Type1) and (Time == Time1) end), H#station.rec));
getOneValue(Name, Time, Type, [_ | T]) -> getOneValue(Name, Time, Type, T).



% ======
%	getStationMean/3 - zwraca srednia wartosc parametru danego typu z zadanej stacji;
% ======

getStationMean(_, _, []) -> 0;
getStationMean(Name, Type, [H = #station{name = Name} | _]) -> average(lists:filter(fun (#rec {type = Type1}) -> (Type == Type1) end, H#station.rec));
getStationMean(Name, Type, [_ | T]) -> getStationMean(Name, Type, T).


% ======
%	getDailyMean/3 - zwraca srednia wartosc parametru danego typu, danego dnia na wszystkich stacjach;
% ======

getDailyMean(_, _, []) -> 0;
getDailyMean(Type, {Y, M, D}, M) -> getDaily(Type, {Y, M, D}, M, []).


% ======
%	getMinimumDistanceStation - wyszukuje pare najblizszych stacji
% ======

getMinimumDistanceStation({_, _}, []) -> "Brak stacji.";
getMinimumDistanceStation({X,Y},L) -> 
	{getName(lists:filter(fun(#station{coords = #coords{x = X1, y=Y1}}) -> (getDistance({X,Y}, {X1,Y1}) == minimum(getDistances({X,Y}, L), {-1,-1})) end, L)), 
	 getName(lists:filter(fun(#station{coords = #coords{x = X1, y=Y1}}) -> (getDistance({X,Y}, {X1,Y1}) == secondMinimum(getDistances({X,Y}, L), {-1,-1})) end, L))}.

%% ====================================================================
%% Internal functions
%% ====================================================================

existingStation([], _, _) -> false;
existingStation([#station{name = Name} | _], Name, _) -> true;
existingStation([#station{name = _, coords = #coords{x = X, y = Y}} | _], _, {X, Y}) -> true;
existingStation([_|T], Name, {X,Y}) -> existingStation(T, Name, {X,Y}).

%
couldBeAdded({X, Y}, Time, Type, Value, Monitor) ->
	lists:any(fun(#station{coords = #coords{x = X1, y = Y1}, rec = Rec}) ->
		(X == X1) and 
		(Y == Y1) and not 
		(lists:any(fun(#rec{type = T, time = Time1, value = V}) ->
				(T == Type) and 
				(V == Value) and
				(Time1 == Time) end, Rec)) 
		end, Monitor);

couldBeAdded(Name, Time, Type, Value, Monitor) -> 
	lists:any(fun(#station{name = Name1, rec = Rec}) ->
		(Name == Name1) and not 
		(lists:any(fun(#rec{type = T, time = Time1, value = V}) ->
				(T == Type) and 
				(V == Value) and
				(Time1 == Time) end, Rec)) 
		end, Monitor).

%
addVal({X, Y}, Time, Type, Value,[H = #station{coords = #coords{x = X, y = Y}} | T]) ->
	[#station{name = H#station.name, coords = #coords{x = X, y = Y},
    rec = H#station.rec ++ [#rec{type = Type, time = Time, value = Value}]} | T];
addVal({X, Y}, Time, Type, Value,[H|T]) -> 
	[H | addVal({X, Y}, Time, Type, Value, T)];

addVal(Name, Time, Type, Value, [H = #station{name = Name} | T]) ->
  [#station{name = Name, 
			coords = #coords{x = H#station.coords#coords.x, y = H#station.coords#coords.y},
			rec = H#station.rec ++ [#rec{type = Type,time = Time, value = Value}]} | T];
addVal(Name, Time, Type, Value, [H | T]) -> [H | addVal(Name, Time, Type, Value, T)].

%
getValue([#rec{value = Value}]) -> Value.

%

getDaily(_, _, [], D) -> average(D);
getDaily(Type, {Year, Month, Day}, [H | T], D) ->
getDaily(Type, {Year, Month, Day}, T, lists:filter(fun (#rec {type = Type1, time = {{Year1, Month1, Day1}, _ }}) -> (Type == Type1) and (Year == Year1) and (Month == Month1) and (Day == Day1) end, H#station.rec) ++ D).

%
average([]) -> "Nie dodano pomiarow.";
average(L) -> valueList(L, []).

valueList([], L) -> average2(L, 0, 0);
valueList([H | T], L) -> valueList(T, [H#rec.value | L]).

average2([], Sum, NumberOfEl) -> Sum / NumberOfEl;
average2([H | T], Sum, NumberOfEl) -> average2(T, Sum + H, NumberOfEl + 1).


%
gmds({_, _}, [])-> "d";
gmds({X,Y},L) -> getName(lists:filter(fun(#station{coords = #coords{x = X1, y=Y1}}) -> (getDistance({X,Y}, {X1,Y1}) == minimum(getDistances({X,Y}, L), {-1,-1})) end, L)).

getDistance({X,Y},{X1,Y1}) -> math:sqrt((X-X1)*(X-X1)+(Y-Y1)*(Y-Y1)).

getDistances({_, _}, []) -> [];
getDistances({MyX, MyY}, [H = #station{coords = #coords{x=X,y=Y}}|T]) ->
	 [getDistance({X,Y},{MyX,MyY}) | getDistances({MyX, MyY}, T)].

minimum([], {A, _}) -> A;
minimum([H|T], {A, B}) -> 
	if (A<0) -> minimum(T, {H, B});
		((H=<A)) -> minimum(T, {H, A});
		((H=<B) and (A>0) and (H>A)) -> minimum(T, {A, H});
		true -> minimum(T, {A, B})
	end.
	
secondMinimum([], {_, B}) -> B;
secondMinimum([H|T], {A, B}) -> 
	if (A<0) -> secondMinimum(T, {H, B});
		((H=<A)) -> secondMinimum(T, {H, A});
		((H=<B) and (A>0) and (H>A)) -> secondMinimum(T, {A, H});
		true -> secondMinimum(T, {A, B})
	end.

getName([#station{name = Name}]) -> Name.
%
main() ->
	M = pollution:createMonitor(),
	P1 = pollution:addStation("Aleja Slowackiego", {50.2345, 18.3445}, M),
	P2 = pollution:addStation("Inna", {40.3, 15.0}, P1),
	P3 = pollution:addStation("Jeszcze Inna", {12.0, 12.0}, P2),
	P4 = pollution:addValue({50.2345, 18.3445}, calendar:local_time(),"PM10", 70, P3),
	P5 = pollution:addValue("Inna", calendar:local_time(), "PM10", 50, P4),
	P6 = pollution:addValue("Aleja Slowackiego", calendar:local_time(), "PM10", 40, P5),
	%P7 = pollution:removeValue({50.2345, 18.3445}, calendar:local_time(), "PM10", P6).
	%P8 = pollution:removeValue("Aleja Sowackiego", calendar:local_time(), "PM2,5", P3).
	%A = getStationMean("Aleja Slowackiego", "PM10", P6).
	%B = getDailyMean("PM10", {2017, 4, 24}, P6).
	%A = getMinimumDistanceStation({40.0, 17.0}, P6).
	F = getOneValue("Inna", calendar:local_time(), "PM10", P6).
	%D = gmds({40.0, 17.0}, P6).
	%H =getDistances({40.0, 17.0}, P6).
	%K1 = secondMinimum(getDistances({40.0, 17.0}, P6), {-1,-2}).
	%K = minimum(getDistances({40.0, 17.0}, P6), {-1,-2}).
%% ====================================================================