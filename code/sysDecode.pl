:- module('sysDecode', [ decode/2 ]).

:- use_module(bee('beeCompiler/bDecode.pl')).

/****************************/
/******* BEE Decoding *******/
/****************************/
decode(map(HealthMap,FaultyBitsMap),Diag) :- decode1(HealthMap,FaultyBitsMap,Diag).

decode1([], [], []).
decode1([kvp(ID,Val)|KVPs], [kvp(ID,FaultyBits1)|KVPs1], [p(ID,Diag)|IDs]) :-
	term_variables(Val,Val1),
	(Val1 = [] -> true ; Val1 = [1]),
	decodeInt(Val,ValDecoded),
	ValDecoded is -1,
	decodeIntArray(FaultyBits1,FaultyBits),
	faultType(FaultyBits,Diag),
	decode1(KVPs, KVPs1, IDs).

decode1([kvp(_ID,Val)|KVPs], [_|KVPs1], IDs) :-
	term_variables(Val,Val1),
	(Val1 = [] -> true ; Val1 = [1]),
	decodeInt(Val,ValDecoded),
	\+(ValDecoded is -1),
	decode1(KVPs, KVPs1, IDs).
	
faultType([1,-1,-1],f).
faultType([-1,1,-1],s0).
faultType([-1,-1,1],s1).