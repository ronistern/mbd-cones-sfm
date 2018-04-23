%Author: Orel Elimelech

:- module('expandTLD', [ groundTLDs/5 ]).

:- use_module(sysPropagation).
:- use_module(sysConeGrounding).

/*******************************/
/******* Ground all TLDs *******/
/*******************************/
groundTLDs(Gates, Cones, ObsIO, TLDs, AllDiags) :- 
	expandTLDs(Gates, Cones, ObsIO, TLDs, AllDiags1),
	append(AllDiags1, AllDiags2),
	sort(AllDiags2,AllDiags).
	
%Iterates over all TLDs and expand each one to all minimal cardinality diagnosis.
expandTLDs(_Gates,_Cones,_ObsIO,[],[]).
expandTLDs(Gates,Cones,ObsIO,[TLD|TLDs],[ExpandedTLD|ExpandedTLDs]):- %writeln(currTLD:TLD),
	expandTLD(Gates,Cones,ObsIO,TLD,ExpandedTLD),%writeln(expanded:ExpandedTLD),
	expandTLDs(Gates,Cones,ObsIO,TLDs,ExpandedTLDs).

/******************************/
/******* Ground one TLD *******/
/******************************/
%Expands one TLD to all minimal cardinality diagnosis.
expandTLD(Gates,Cones,ObsIO,TLD,ExpandedTLD) :- %writeln(expanding),
	%get_time(T0),
	simulateSys(Gates, ObsIO, TLD, VarsMap, _FBitsMap),
	%get_time(T1), TVars is T1-T0,
	findValidCandidates(Gates, Cones, ObsIO, TLD, VarsMap, ValidCandidates),%writeln(validCand:ValidCandidates),
	%get_time(T2), TCandidates is T2-T1,
	length(TLD,MC),%writeln(mc:MC),
	findall(Diagnoses, (length(Diagnoses1,MC),hittingSet(Diagnoses1,ValidCandidates), sort(Diagnoses1,Diagnoses)), ExpandedTLD1),
	%get_time(T3), THittingSet is T3-T2,
	%writef("TVars: %w, TCandidates: %w, THittingSet: %w\n", [TVars, TCandidates, THittingSet]),
	sort(ExpandedTLD1,ExpandedTLD).

/***************************/
/******* Hitting Set *******/
/***************************/
hittingSet(_,[]).
hittingSet(Diagnosis, [List|Lists]) :-
	hitsList(Diagnosis, List),
	hittingSet(Diagnosis, Lists).
	
hitsList([Gate|Gates],List) :- (member(Gate,List) ; hitsList(Gates,List)).


