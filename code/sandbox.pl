:- module('sandbox', [ test_containsFreeVars/0, test_appendToEndOfList/0 ]).

test_appendToEndOfList :-
	Gates = [ (gate5408,z923,[z761],inverter,dominated,[s0]), (gate5407,z228,[z119],inverter,dominated,[f]), (gate5404,z761,[z501],buffer,dominated,[f]), (gate5401,z119,[z12],buffer,dominated,[s0]), (gate1555,z1251,[z119,z923],nand,dominated,[f]), (gate1554,z922,[z761,z228],nand,dominated,[f]), (gate1556,z1358,[z922,z1251],nand,dominator,[s0])],
	VarsMap = [kvp(z12,1),kvp(z501,1),kvp(z923,_G3675915),kvp(z228,_G3675936),kvp(z761,_G3675957),kvp(z119,_G3675978),kvp(z1251,_G3675999),kvp(z922,_G3676020),kvp(z1358,_G3676041)],
	FBitsMap = [kvp(gate5408,[-1,1,-1]),kvp(gate5407,[-1,-1,-1]),kvp(gate5404,[-1,-1,-1]),kvp(gate5401,[-1,-1,-1]),kvp(gate1555,[-1,-1,-1]),kvp(gate1554,[-1,-1,-1]),kvp(gate1556,[-1,-1,-1])],
	writeln('----'),
	writeln(gates:Gates),
	writeln('----'),
	writeln(varsmap:VarsMap),
	writeln('----'),
	writeln(fbitsmap:FBitsMap),
	writeln('----'),
	
	propagateGates(Gates,VarsMap,FBitsMap),
	
	writeln(test).

	
	
propagateGates([],_,_):-!.
propagateGates([(ID,Out,InList,LType,DomType,Modes)|Gates],VarsMap,FBitsMap):-writeln(propagate:ID),writeln('***********'),%writeln(varsmap:VarsMap),
	getValue(Out, VarsMap, VOut),writeln(gotval:VOut),
	getInValues(InList, VarsMap, InVals),writeln(inlist:InList),writeln(gotinval:InVals),
	getValue(ID, FBitsMap, FBits),writeln(propagateGate(LType,InVals,VOut,FBits)),
	((containsFreeVars(InVals), \+containsFreeVars(FBits), (fBitsAreEqual(FBits,[-1,-1,-1]); fBitsAreEqual(FBits,[1,-1,-1]))) -> 
		writeln(skippropagation),
		append(Gates,[(ID,Out,InList,LType,DomType,Modes)],FixedGates),
		propagateGates(FixedGates,VarsMap,FBitsMap)
		; 
		writeln(startpropagation),
		propagateGate(LType,InVals,VOut,FBits),writeln(propagated),
		propagateGates(Gates,VarsMap,FBitsMap)
	).
	
propagateGate(_Type,_InVals,-1,[-1,1,-1]) :- !.
propagateGate(_Type,_InVals,1,[-1,-1,1]) :- !.
propagateGate(Type,InVals,Out,[1,-1,-1]) :- !,
	gateOutput(Type,InVals,Out1), Out is -Out1.
propagateGate(Type,InVals,Out,[-1,-1,-1]):-
	gateOutput(Type,InVals,Out).

gateOutput(inverter,[In],O) :- O is -In.
gateOutput(buffer,[In],In).
gateOutput(Type,InVals,O) :-
	atom_concat('n',RType,Type),!,
	gateOutput(RType,InVals,NegOut),
	O is -NegOut.
gateOutput(and,InVals,O) :-
	(member(-1,InVals) -> O is (-1); O is 1).
gateOutput(or,InVals,O) :-
	(member(1,InVals) -> O is 1; O is (-1)).
gateOutput(xor,InVals,O) :-
	count1s(InVals,Counter),
	Mod is Counter mod 2,
	(Mod is 0 -> O is (-1) ; O is 1).
	
count1s([],0).
count1s([1|Xs],Cnt) :- !, count1s(Xs,Cnt1), Cnt is Cnt1+1.
count1s([_|Xs],Cnt) :- count1s(Xs,Cnt).

containsFreeVars([]) :- false.
containsFreeVars([X|_Xs]) :- var(X),!.
containsFreeVars([X|Xs]) :- \+var(X), containsFreeVars(Xs).

fBitsAreEqual([],[]).
fBitsAreEqual([X|Xs],[Y|Ys]) :- \+var(X), \+var(Y), X is Y, fBitsAreEqual(Xs,Ys).


getValue(_Key, [], _Val) :-
	writeln('can not find ID in list').
getValue(Key, [kvp(Key,Value)|_KVPs], Value):-!.
getValue(Key, [kvp(Key1,_Value1)|KVPs], Value) :-
	dif(Key,Key1),
	getValue(Key, KVPs, Value).
	
getInValues([In|Ins],VarsMap,[Val|Vals]) :-
	getValue(In, VarsMap, Val),
	getInValues(Ins, VarsMap, Vals).
getInValues([],_,[]).
	
	
	

test_containsFreeVars :-
	
	List1 = [X,1,2,Y],
	List2 = [X],
	List3 = [1,2,Y],
	List4 = [X,Y],
	List5 = [1,2],
	List6 = [2],
	List7 = [X,1],
	
	writeln('*****'),
	((write(List1),write('\t'),containsFreeVars(List1)) -> writeln(contains) ; writeln(notcontains)),
	((write(List2),write('\t\t'),containsFreeVars(List2)) -> writeln(contains) ; writeln(notcontains)),
	((write(List3),write('\t\t'),containsFreeVars(List3)) -> writeln(contains) ; writeln(notcontains)),
	((write(List4),write('\t\t'),containsFreeVars(List4)) -> writeln(contains) ; writeln(notcontains)),
	((write(List5),write('\t\t\t'),containsFreeVars(List5)) -> writeln(contains) ; writeln(notcontains)),
	((write(List6),write('\t\t\t'),containsFreeVars(List6)) -> writeln(contains) ; writeln(notcontains)),
	((write(List7),write('\t\t'),containsFreeVars(List7)) -> writeln(contains) ; writeln(notcontains)).