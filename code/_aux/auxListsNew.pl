/**/
:- module(auxListsNew, [ getValue/3, assignValue/3, groundList/1, getInValues/3, getInValusDiffLists/3 ]).


getValue(_Key, [], _Val) :-
	writeln('can not find ID in list').
getValue(Key, [kvp(Key,Value)|_KVPs], Value):-!.
getValue(Key, [kvp(Key1,_Value1)|KVPs], Value) :-
	dif(Key,Key1),
	getValue(Key, KVPs, Value).
	
assignValue(_ID, [], _) :-
	writeln('can not find ID in list').
assignValue(ID, [kvp(ID1,_)|Vars], Val) :-
	dif(ID,ID1),
	assignValue(ID, Vars, Val).
assignValue(ID, [kvp(ID,Val)|_Vars], Val).
	
groundList([X|Xs]) :-
	ground(X),
	groundList(Xs).
groundList([]).

getInValues([In|Ins],VarsMap,[Val|Vals]) :-
	getValue(In, VarsMap, Val),
	getInValues(Ins, VarsMap, Vals).
getInValues([],_,[]).

getInValusDiffLists([], _, E-E).
getInValusDiffLists([In|Ins], VarsMap, Vals1-Tail) :-
	getInValusDiffLists(Ins, VarsMap, Vals1-Vals2),
	getValue(In, VarsMap, Val),
	Vals2 = [Val|Tail].