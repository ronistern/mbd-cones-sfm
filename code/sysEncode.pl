:- module('sysEncode', [ 	
				gatesConstraint/7, boundConstraint/4, initDominatedGates/2 ]).

:- use_module(sysGateEncoding).

:- use_module('aux/auxListsv2').
:- use_module('aux/auxLiteralsv2').
:- use_module('aux/auxSrtListsv2').
:- use_module('aux/auxListsNew').

/********************************/
/******* BEE Encoding Aux *******/
/********************************/
initDominatedGates([],_).
initDominatedGates([(_Dominator,DominatedGates)|Cones],HealthMap) :-
	makeDominatedHealthy(DominatedGates,HealthMap),
	initDominatedGates(Cones, HealthMap).
	
makeDominatedHealthy([],_).
makeDominatedHealthy([GateID|IDs],HealthMap) :- %writeln(dom:GateID),
	getValue(GateID,HealthMap,1),
	makeDominatedHealthy(IDs, HealthMap).
	
gatesConstraint([],_,_,[],[],(E1-E1,E2-E2),E3-E3).
gatesConstraint([Gate|Gates],VMap,HMap,[FBit|FBits],[kvp(ID,ThreeFBits)|Ts],(Bools1-Bools3,Ints1-Ints3),Cs1-Cs3):- 
	 Gate = (ID,Out,Ins,GateType,_,Modes),
	 gateConstraint(GateType,Ins,Out,Modes,VMap,HMap,ID,ThreeFBits,FBit,(Bools1-Bools2,Ints1-Ints2),Cs1-Cs2),
	 gatesConstraint(Gates,VMap,HMap,FBits,Ts,(Bools2-Bools3,Ints2-Ints3),Cs2-Cs3).

boundConstraint(UpBound,NegHealthList,(E1-E1,E2-E2),[bool_array_sum_leq(NegHealthList, UpBound)|Tail]-Tail).

