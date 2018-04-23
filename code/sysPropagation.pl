:- module('sysPropagation', [ 	createMapsFromGates/4, createFBitsMapsFromGates/4,
				simulateSys/5, propagateGates/3, setRestHealthy/1 ]).

:- use_module('aux/auxListsNew').

/**************************/
/******* Simulation *******/
/**************************/
simulateSys(Gates, ObsIO, Diag, VarsMap, FBitsMap) :-
	createFBitsMapsFromGates(Gates, ObsIO, VarsMap, FBitsMap),
	setFaulty(Diag, FBitsMap),
	setRestHealthy(FBitsMap),
	propagateGates(Gates,VarsMap,FBitsMap).
	
/*************************/
/******* Set HBits *******/
/*************************/
setFaulty([], _HMap).
setFaulty([p(ID,FaultType)|FaultyGates], FBitsMap) :- 
	getValue(ID, FBitsMap, FBits),
	(FaultType = f -> FBits = [1,-1,-1]; true), 
	(FaultType = s0 -> FBits = [-1,1,-1]; true),
	(FaultType = s1 -> FBits = [-1,-1,1]; true),
	setFaulty(FaultyGates, FBitsMap).

setRestHealthy([]).
setRestHealthy([kvp(_GateID,[F,S0,S1])|KVPs]):-
	(var(F) -> F is -1 ; true),
	(var(S0) -> S0 is -1 ; true),
	(var(S1) -> S1 is -1 ; true),
	setRestHealthy(KVPs).
	
/***************************/
/******* Propagation *******/
/***************************/
propagateGates([],_,_):-!.
propagateGates([(ID,Out,InList,LType,_,_Modes)|Gates],VarsMap,FBitsMap):-writeln(propagate:ID),writeln('***********'),%writeln(varsmap:VarsMap),
	getValue(Out, VarsMap, VOut),writeln(gotval:VOut),
	getInValues(InList, VarsMap, InVals),writeln(inlist:InList),writeln(gotinval:InVals),
	getValue(ID, FBitsMap, FBits),writeln(propagateGate(LType,InVals,VOut,FBits)),
	propagateGate(LType,InVals,VOut,FBits),writeln(propagated),
	propagateGates(Gates,VarsMap,FBitsMap).
	
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

/***************************/
/******* Create Maps *******/
/***************************/
createMapsFromGates(Gates,Obs,VarsMap,HealthMap) :-
	createHealthMap(Gates, VarsMap1, HealthMap),
	reverse(Obs,ObsRev),
	createVarsMap(ObsRev, VarsMap1, VarsMap).

createHealthMap([], [], []).
createHealthMap([(ID,Out,_Ins,_Type,_GType,_Modes)|Gates], [kvp(Out, _)|Vars], [kvp(ID, _)|HBits]) :-
	createHealthMap(Gates, Vars, HBits).

createVarsMap([], VarsMap, VarsMap).
createVarsMap([IO|IOs], VarsMap, FinalVarsMap) :-
	pureIO(IO,ID,Val),
	member(kvp(ID,_),VarsMap),
	assignValue(ID, VarsMap, Val),
	createVarsMap(IOs, VarsMap, FinalVarsMap),!.
createVarsMap([IO|IOs], VarsMap, FinalVarsMap) :-
	pureIO(IO,ID,Val),
	\+member(kvp(ID,_),VarsMap),
	append([kvp(ID,Val)],VarsMap,VarsMap1),
	createVarsMap(IOs, VarsMap1, FinalVarsMap),!.
pureIO(-ID,ID,-1).
pureIO(ID,ID,1).

createFBitsMapsFromGates(Gates,Obs,VarsMap,FBitsMap) :-
	createFBitsMap(Gates, VarsMap1, FBitsMap),
	reverse(Obs,ObsRev),
	createVarsMap(ObsRev, VarsMap1, VarsMap).
	
createFBitsMap([], [], []).
createFBitsMap([(ID,Out,_Ins,_Type,_GType, Modes)|Gates], [kvp(Out, _)|Vars], [kvp(ID, [F,S0,S1])|FBits]) :-
	(\+member(f,Modes) -> F is -1 ; true),
	(\+member(s0,Modes) -> S0 is -1 ; true),
	(\+member(s1,Modes) -> S1 is -1 ; true),
	createFBitsMap(Gates, Vars, FBits).

