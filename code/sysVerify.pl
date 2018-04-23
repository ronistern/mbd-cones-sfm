:- module('sysVerify', [ verify/2 ]).

:- use_module('aux/auxListsNew').
:- use_module(sysPropagation).

/**************************/
/******* BEE Verify *******/
/**************************/
%verify(_Instance,_Solution).

verify(_,_) :- !.

verify(Instance,Solution) :-
	writef("Verifying solution: %w...\n", [Solution]),
	(verifyDiagnosis(Instance, Solution) -> 
		true 
	;
		writef("\nFailed verifying %w\n", [Solution])
	).
	
verifyDiagnosis(instance(Gates,_Cones,ObsIO,UpBound), Solution) :-
	verifyDiagnosis(instance(Gates,ObsIO,UpBound), Solution).
	
verifyDiagnosis(instance(Gates,ObsIO,UpBound), Solution) :-
	verifyCardinality(Solution, UpBound),
	verifyConsistency(Gates, ObsIO, Solution).
	
verifyCardinality(Solution, UpBound) :- 
	length(Solution, Cardinality),
	Cardinality =< UpBound.
	
verifyConsistency(Gates, ObsIO, Solution) :-
	simulateSys(Gates, ObsIO, Solution, VarsMap, _FBitsMap),
	getOutputs(ObsIO, PosOutputs, NegOutputs),
	verifyPosOuts(PosOutputs, VarsMap),
	verifyNegOuts(NegOutputs, VarsMap).
	
verifyPosOuts([], _).
verifyPosOuts([O|Os], VarsMap) :- getValue(O, VarsMap, 1), verifyPosOuts(Os, VarsMap).
verifyNegOuts([], _).
verifyNegOuts([O|Os], VarsMap) :- getValue(O, VarsMap, -1), verifyNegOuts(Os, VarsMap).
	
getOutputs(ObsIO, PosOutputs, NegOutputs) :-
	filterVars(ObsIO, PosVars, NegVars),
	filterOutputs(PosVars, PosOutputs),
	filterOutputs(NegVars, NegOutputs).
	
filterVars([],[],[]).
filterVars([-V|Vs], PosVars, [V|NegVars]) :- filterVars(Vs, PosVars, NegVars).
filterVars([V|Vs], [V|PosVars], NegVars) :- filterVars(Vs, PosVars, NegVars).

filterOutputs([],[]).
filterOutputs([V|Vs],[V|Outs]) :- atom_concat(o,_,V), filterOutputs(Vs, Outs).
filterOutputs([_|Vs],Outs) :- filterOutputs(Vs, Outs).