%Author: Orel Elimelech
%Date: 01/05/2014

:- module(sysGateEncoding, [ gateConstraint/11 ]).

:- use_module('aux/auxListsNew').

/******************************/
/******* Gates Encoding *******/
/******************************/

%and gate encoding
gateConstraint(and,Ins,Out,Modes,VMap,HMap,ID,FaultyBits,NegHVar,(Bools-B,E-E),Cs1-Cs2) :-
	getInValusDiffLists(Ins, VMap, Bools-Tail),
	getInValues(Ins, VMap, InVars),
	getValue(Out, VMap, OutVar),
	getValue(ID, HMap, HVar),
	FaultyBits = [FlipBit,StAt0Bit, StAt1Bit],
	getFaultyBits(Modes,FaultyBits),
	Tail = [OutVar, HVar, NegHVar, Z1, Z2, Z3, FlipBit, StAt0Bit, StAt1Bit|B],
	NegHVar = -HVar,
	Cs1 = [	bool_array_and_reif(InVars,Z1), 
		bool_xor_reif(Z1,FlipBit,Z2),
		bool_or_reif(Z2,StAt1Bit,Z3),
		bool_and_reif(Z3,-StAt0Bit,OutVar),
		bool_array_sum_leq(FaultyBits,1),
		bool_array_or_reif(FaultyBits,NegHVar)|Cs2].

%or gate encoding
gateConstraint(or,Ins,Out,Modes,VMap,HMap,ID,FaultyBits,NegHVar,(Bools-B,E-E),Cs1-Cs2) :-
	getInValusDiffLists(Ins, VMap, Bools-Tail),
	getInValues(Ins, VMap, InVars),
	getValue(Out, VMap, OutVar),
	getValue(ID, HMap, HVar),
	FaultyBits = [FlipBit,StAt0Bit, StAt1Bit],
	getFaultyBits(Modes,FaultyBits),
	Tail = [OutVar, HVar, NegHVar, Z1, Z2, Z3, FlipBit, StAt0Bit, StAt1Bit|B],
	NegHVar = -HVar,
	Cs1 = [	bool_array_or_reif(InVars,Z1), 
		bool_xor_reif(Z1,FlipBit,Z2),
		bool_or_reif(Z2,StAt1Bit,Z3),
		bool_and_reif(Z3,-StAt0Bit,OutVar),
		bool_array_sum_leq(FaultyBits,1),
		bool_array_or_reif(FaultyBits,NegHVar)|Cs2].
	
%nand gate encoding
gateConstraint(nand,Ins,Out,Modes,VMap,HMap,ID,FaultyBits,NegHVar,(Bools-B,E-E),Cs1-Cs2) :-
	getInValusDiffLists(Ins, VMap, Bools-Tail),
	getInValues(Ins, VMap, InVars),
	getValue(Out, VMap, OutVar),
	getValue(ID, HMap, HVar),
	FaultyBits = [FlipBit,StAt0Bit, StAt1Bit],
	getFaultyBits(Modes,FaultyBits),
	Tail = [OutVar, HVar, NegHVar, Z1, Z2, Z3, FlipBit, StAt0Bit, StAt1Bit|B],
	NegHVar = -HVar,
	Cs1 = [	bool_array_and_reif(InVars,Z1), 
		bool_xor_reif(-Z1,FlipBit,Z2),
		bool_or_reif(Z2,StAt1Bit,Z3),
		bool_and_reif(Z3,-StAt0Bit,OutVar),
		bool_array_sum_leq(FaultyBits,1),
		bool_array_or_reif(FaultyBits,NegHVar)|Cs2].
	
%nor gate encoding
gateConstraint(nor,Ins,Out,Modes,VMap,HMap,ID,FaultyBits,NegHVar,(Bools-B,E-E),Cs1-Cs2) :-
	getInValusDiffLists(Ins, VMap, Bools-Tail),
	getInValues(Ins, VMap, InVars),
	getValue(Out, VMap, OutVar),
	getValue(ID, HMap, HVar),
	FaultyBits = [FlipBit,StAt0Bit, StAt1Bit],
	getFaultyBits(Modes,FaultyBits),
	Tail = [OutVar, HVar, NegHVar, Z1, Z2, Z3, FlipBit, StAt0Bit, StAt1Bit|B],
	NegHVar = -HVar,
	Cs1 = [	bool_array_or_reif(InVars,Z1), 
		bool_xor_reif(-Z1,FlipBit,Z2),
		bool_or_reif(Z2,StAt1Bit,Z3),
		bool_and_reif(Z3,-StAt0Bit,OutVar),
		bool_array_sum_leq(FaultyBits,1),
		bool_array_or_reif(FaultyBits,NegHVar)|Cs2].

%xor gate encoding
gateConstraint(xor,Ins,Out,Modes,VMap,HMap,ID,FaultyBits,NegHVar,(Bools-B,E-E),Cs1-Cs2) :-
	getInValusDiffLists(Ins, VMap, Bools-Tail),
	getInValues(Ins, VMap, InVars),
	getValue(Out, VMap, OutVar),
	getValue(ID, HMap, HVar),
	FaultyBits = [FlipBit,StAt0Bit, StAt1Bit],
	getFaultyBits(Modes,FaultyBits),
	Tail = [OutVar, HVar, NegHVar, Z1, Z2, Z3, FlipBit, StAt0Bit, StAt1Bit|B],
	NegHVar = -HVar,
	Cs1 = [	bool_array_xor_reif(InVars,Z1), 
		bool_xor_reif(Z1,FlipBit,Z2),
		bool_or_reif(Z2,StAt1Bit,Z3),
		bool_and_reif(Z3,-StAt0Bit,OutVar),
		bool_array_sum_leq(FaultyBits,1),
		bool_array_or_reif(FaultyBits,NegHVar)|Cs2].
	
%inverter encoding
gateConstraint(inverter,Ins,Out,Modes,VMap,HMap,ID,FaultyBits,NegHVar,(Bools-B,E-E),Cs1-Cs2) :-
	getInValusDiffLists(Ins, VMap, Bools-Tail),
	getInValues(Ins, VMap, InVars),
	getValue(Out, VMap, OutVar),
	getValue(ID, HMap, HVar),
	FaultyBits = [FlipBit,StAt0Bit, StAt1Bit],
	getFaultyBits(Modes,FaultyBits),
	Tail = [OutVar, HVar, NegHVar, Z2, Z3, FlipBit, StAt0Bit, StAt1Bit|B],
	NegHVar = -HVar,
	InVars = [In],
	Cs1 = [	bool_xor_reif(-In,FlipBit,Z2),
		bool_or_reif(Z2,StAt1Bit,Z3),
		bool_and_reif(Z3,-StAt0Bit,OutVar),
		bool_array_sum_leq(FaultyBits,1),
		bool_array_or_reif(FaultyBits,NegHVar)|Cs2].

%buffer encoding
gateConstraint(buffer,Ins,Out,Modes,VMap,HMap,ID,FaultyBits,NegHVar,(Bools-B,E-E),Cs1-Cs2) :-
	getInValusDiffLists(Ins, VMap, Bools-Tail),
	getInValues(Ins, VMap, InVars),
	getValue(Out, VMap, OutVar),
	getValue(ID, HMap, HVar),
	FaultyBits = [FlipBit,StAt0Bit, StAt1Bit],
	getFaultyBits(Modes,FaultyBits),
	Tail = [OutVar, HVar, NegHVar, Z2, Z3, FlipBit, StAt0Bit, StAt1Bit|B],
	NegHVar = -HVar,
	InVars = [In],
	Cs1 = [	bool_xor_reif(In,FlipBit,Z2),
		bool_or_reif(Z2,StAt1Bit,Z3),
		bool_and_reif(Z3,-StAt0Bit,OutVar),
		bool_array_sum_leq(FaultyBits,1),
		bool_array_or_reif(FaultyBits,NegHVar)|Cs2].

getFaultyBits(Modes,[_,_,_]):- member(f,Modes), member(s0,Modes), member(s1,Modes),!.
getFaultyBits(Modes,[-1,_,_]):- \+member(f,Modes), member(s0,Modes), member(s1,Modes),!.
getFaultyBits(Modes,[_,-1,_]):- member(f,Modes), \+member(s0,Modes), member(s1,Modes),!.
getFaultyBits(Modes,[_,_,-1]):- member(f,Modes), member(s0,Modes), \+member(s1,Modes),!.
getFaultyBits(Modes,[-1,-1,_]):- \+member(f,Modes), \+member(s0,Modes), member(s1,Modes),!.
getFaultyBits(Modes,[-1,_,-1]):- \+member(f,Modes), member(s0,Modes), \+member(s1,Modes),!.
getFaultyBits(Modes,[_,-1,-1]):- member(f,Modes), \+member(s0,Modes), \+member(s1,Modes),!.
	
	
	
	
	
	
	
	
	
	
	