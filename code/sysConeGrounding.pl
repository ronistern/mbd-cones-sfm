:- module('sysConeGrounding', [ 	findValidCandidates/6, 
					getCandidates/3, getConeGates/3, 
					getConeInputs/2, getInputsValues/3,
					modeIsOk/6
				]).

:- use_module('aux/auxListsNew').
:- use_module(sysPropagation).

/********************************************************/
/******* Find candidates to replace the dominator *******/
/********************************************************/	
%Given a TLD, this predicate will get all valid candidates to replace each one of the dominator gates.
findValidCandidates(_G, _C, _O, [], _TLD, []).
findValidCandidates(Gates, Cones, ObsIO, [p(Dominator,_)|Dominators], VarsMap, [ValidCandidates|MoreValidCandidates]) :-
	getCandidates(Cones, Dominator, Candidates),%writeln(candidates:Candidates),
	getConeGates(Gates, Candidates, ConeGates),%writeln(coneGates: ConeGates),
	getConeInputs(ConeGates, ConeInputs),%writeln(coneInputs:ConeInputs),
	select((Dominator,ConeOutput,_,_,_,_),Gates,_),
	getInputsValues(ConeInputs, VarsMap, ConeInputsVals), %writeln(ConeInputs),writeln(ConeInputsVals),nl,
	getValue(ConeOutput,VarsMap,ConeOutputVal),
	getValidCandidates(Candidates, ConeGates, ConeOutput, ConeOutputVal, ConeInputsVals, ValidCandidates1),!,
	append(ValidCandidates1,ValidCandidates),
	findValidCandidates(Gates, Cones, ObsIO, Dominators, VarsMap, MoreValidCandidates).
/*First try fails when current gate is not a dominator (may be a free gate, so we can't expand it)*/
findValidCandidates(Gates, Cones, ObsIO, [p(Dominator,Fault)|Dominators], TLD, [[p(Dominator,Fault)]|MoreValidCandidates]) :-
	findValidCandidates(Gates, Cones, ObsIO, Dominators, TLD, MoreValidCandidates).

/*************************/
/******* Cone Info *******/
/*************************/
%Given a dominator gates, returns all dominated gates in the corresponding cone.
getCandidates(Cones, Dominator, Candidates) :- %writeln(Dominator), 
	select((Dominator,Candidates1), Cones, _), %write("\t"),writeln(Candidates1),
	append(Candidates1,[Dominator],Candidates).

%Returns a detailed list of cone's gates.
getConeGates(_G, [], []).
getConeGates(Gates, [Candidate|Candidates], [(Candidate,Out,In,T,LT,Modes)|ConeGates]) :-
	select((Candidate,Out,In,T,LT,Modes), Gates, _),
	getConeGates(Gates, Candidates, ConeGates).
	
%Returns cone's relevant input wires.
getConeInputs(ConeGates, ConeInputs) :-
	getAllInputs(ConeGates, AllConeInputs1-EmptyList),
	EmptyList = [],
	sort(AllConeInputs1,AllConeInputs),
	getAllOutputs(ConeGates, AllOutputs),
	subtract(AllConeInputs,AllOutputs,ConeInputs).
	
getAllInputs([],E-E).
getAllInputs([(_,_,Inputs,_,_,_)|Gates],In1-Tail):-
	getAllInputs(Gates,In1-In2),
	getAllInputs1(Inputs,In2-Tail).

getAllInputs1([],E-E).
getAllInputs1([X|Xs],[X|Xs1]-Xs2) :- getAllInputs1(Xs, Xs1-Xs2).

getAllOutputs([],[]).
getAllOutputs([(_,Out,_,_,_,_)|Gates],[Out|Outs]) :- getAllOutputs(Gates, Outs).

%Returns cones input values
getInputsValues([], _VM, []).
getInputsValues([In|Ins], VarsMap, [InVar|InVars]) :-
	getValue(In, VarsMap, InVar1),
	getVar(In, InVar1, InVar),
	getInputsValues(Ins, VarsMap, InVars).

getVar(ID,1,ID).
getVar(ID,-1,-ID).

/*******************************/
/******* Cone Validation *******/
/*******************************/
%Given a dominator gate and a list of candidates to replace it,
%finds the list of valid candidates such that C belongs to ValidCandidates iff
%the output of the dominator when it's faulty, equals the output of it when it's healthy and C is faulty.
getValidCandidates([], _ConeGates, _Dominator, _DomOut, _InGatesInputs, []).

getValidCandidates([Candidate|Candidates], ConeGates, DomOut, DomOutVal, InGatesInputs, [Candidate1|ValidCandidates]) :-
	%writef("\tCandidate: %w\n", [Candidate]),
	ModesBooleans = [F,S0,S1],
	(modeIsOk(Candidate, ConeGates, DomOut, DomOutVal, InGatesInputs,[1,-1,-1]) -> F = true ; F = false),
	(modeIsOk(Candidate, ConeGates, DomOut, DomOutVal, InGatesInputs,[-1,1,-1]) -> S0 = true ; S0 = false),
	(modeIsOk(Candidate, ConeGates, DomOut, DomOutVal, InGatesInputs,[-1,-1,1]) -> S1 = true ; S1 = false),
	(ModesBooleans = [true,true,true] -> Candidate1 = [p(Candidate,f),p(Candidate,s0),p(Candidate,s1)] ; true),
	(ModesBooleans = [true,true,false] -> Candidate1 = [p(Candidate,f),p(Candidate,s0)] ; true),
	(ModesBooleans = [true,false,true] -> Candidate1 = [p(Candidate,f),p(Candidate,s1)] ; true),
	(ModesBooleans = [false,true,true] -> Candidate1 = [p(Candidate,s0),p(Candidate,s1)] ; true),
	(ModesBooleans = [false,false,true] -> Candidate1 = [p(Candidate,s1)] ; true),
	(ModesBooleans = [false,true,false] -> Candidate1 = [p(Candidate,s0)] ; true),
	(ModesBooleans = [true,false,false] -> Candidate1 = [p(Candidate,f)] ; true),
	(ModesBooleans = [false,false,false] -> Candidate1 = [] ; true),
	getValidCandidates(Candidates, ConeGates, DomOut, DomOutVal, InGatesInputs, ValidCandidates).

modeIsOk(Candidate, ConeGates, DomOut, DomOutVal, InGatesInputs,CandidateModes) :- writeln(modeisok),
	createFBitsMapsFromGates(ConeGates,InGatesInputs,VarsMap,FBitsMap),
	writef("\tFBitsMap: %w\n", [FBitsMap]),writeln(getValue(Candidate, FBitsMap, CandidateModes)),
	getValue(Candidate, FBitsMap, CandidateModes),
	writef("\tCandidateModes: %w\n", [CandidateModes]),
	setRestHealthy(FBitsMap),writeln(setRestHealthy),writeln('-------'),writeln(propagateGates(ConeGates,VarsMap,FBitsMap)),
	propagateGates(ConeGates,VarsMap,FBitsMap),writeln(propagateGates),
	getValue(DomOut,VarsMap,CurrentDomOutVal),writeln(getValue),
	CurrentDomOutVal=:=DomOutVal,!.