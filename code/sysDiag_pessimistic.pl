:- module('sysDiag_pessimistic', [ diagnostics_pessimistic/3 ]).

:- use_module(bee('satsolver/satsolver.pl')).
:- use_module(bee('beeCompiler/bCompiler.pl')).
:- use_module(bee('bApplications/auxs/auxRunExpr')).
:- use_module(bee('bApplications/auxs/auxRunExprAll.pl'), [runExprAll/5]).

:- use_module(mainAux).
:- use_module(dataWriter).
:- use_module(sysEncode).
:- use_module(sysDecode).
:- use_module(sysVerify).

:- use_module(sysPropagation).
:- use_module(expandTLD).

/***************************/
/******* Main Method *******/
/***************************/
%diagnostics_pessimistic(+,+,+).
diagnostics_pessimistic(SysFile, System, Obs) :-
	Obs = (SysID,ObsNum,ObsIO),
	System = system(SysID, _In, _Out, Gates, _Sec, AllCones),
	length(Gates, GatesCnt), length(AllCones,AllConesCnt),
	writef("------\n**System Info.**\nSystem file: %w, System ID: %w, Obs#: %w\n", [SysFile, SysID, ObsNum]),
	writef("|Gates|: %w, |All Cones|: %w\n", [GatesCnt, AllConesCnt]),
	
	%getFinalConesFile(SysFile, "p", ObsNum, FinalConesFile),
	
	%Step 0 - remove irrelevant cones
	get_time(T0),
	remove_irrelevant_cones(Gates, AllCones, ConesWithFlip),
	
	length(ConesWithFlip, ConesCnt),
	get_time(T1),
	PreTime is T1 - T0,
	sysInfoToFile("p", SysFile, ObsNum, ConesCnt),
	
	getFinalConesFile(SysFile, "p", ObsNum, FinalConesFile),
	getBeeResultsFile(SysFile, "p", ObsNum, BeeResultsFile),
	
	tell(FinalConesFile),
	writeln(ConesWithFlip),
	told,
	
	writef("------\n**Preprocess (remove irrelevant cones)**\nPreprocess time: %w, Cones count: %w\n", [PreTime, ConesCnt]),
	
	%Step 1 - find MC
	length(Gates, UpBound),
	tell(BeeResultsFile),
	run_mc_pessimistic(Gates,ConesWithFlip,ObsIO,UpBound,_MCdiag,MC),
	told,
	get_time(T2),
	MCTime is T2 - T1,
	mcToFile("p",SysFile, ObsNum, PreTime, 0, MC, MCTime),
	writef("------\n**MC**\nMC: %w, MC time: %w\n", [MC, MCTime]).
	
	/*
	%Step 2 - find all TLDs
	(run_tld_pessimistic(Gates,ConesWithFlip,ObsIO,MC,TLDs) -> true; TLDs = [MCdiag]),
	length(TLDs,TLDCnt),
	get_time(T3),
	TLDTime is T3 - T2,
	writef("------\n**TLD**\nTLD count: %w, TLD time: %w\nTLDs: %w\n", [TLDCnt, TLDTime, TLDs]),

	%Step 3 - find all MC Diags - new
	groundTLDs(Gates,ConesWithFlip,ObsIO,TLDs,AllMCDiags),
	length(AllMCDiags,AllMCDiagsCnt),
	get_time(T4),
	AllMCDiagsTime is T4-T3,
	writef("------\n**All Diags: **\nAll mc diags count: %w, All mc diags time: %w\nAll mc diags: %w\n", [AllMCDiagsCnt, AllMCDiagsTime, AllMCDiags]),!.
	*/
	
/***************************/
/******* Step 1 - MC *******/
/***************************/
run_mc_pessimistic(Gates,Cones,ObsIO,UpBound,Solution,MC) :-
	runExprMin(instance(Gates,Cones,ObsIO,UpBound),Solution,
		sysDiag_pessimistic:encodeMC_pessimistic,
		sysDecode:decode,
		sysVerify:verify),
	(Solution=unsat -> MC is -1 ; length(Solution,MC)).

encodeMC_pessimistic(instance(Gates,Cones,ObsIO,UpBound),map(HealthMap,FaultyBitsMap),FBitsSum,Cs1) :-
	get_time(T0),
	createMapsFromGates(Gates,ObsIO,VarsMap,HealthMap),
	initDominatedGates(Cones,HealthMap),
	gatesConstraint(Gates,VarsMap,HealthMap,FaultyBits,FaultyBitsMap,(_,_),Cs1-Cs2),
	Cs2 = [new_int(FBitsSum,1,UpBound), bool_array_sum_eq(FaultyBits,FBitsSum)|Cs3],
	boundConstraint(UpBound,FaultyBits,(_,_),Cs3-Cs4),
	Cs4 = [],
	get_time(T1),
	EncodingTime is T1-T0,
	format('~w,',[EncodingTime]).
	
/*****************************/
/******* Step 2 - TLDs *******/
/*****************************/	
run_tld_pessimistic(Gates,Cones,ObsIO,MC,Solutions) :-
	runExprAll(instance(Gates,Cones,ObsIO,MC),Solutions,
            sysDiag_pessimistic:encode_tld_pessimistic,
            sysDecode:decode,
            sysVerify:verify).

encode_tld_pessimistic(instance(Gates,Cones,ObsIO,MC),map(HealthMap,FaultyBitsMap),(Bools1,Ints1),Cs1):-
	createMapsFromGates(Gates,ObsIO,VarsMap,HealthMap),
	initDominatedGates(Cones,HealthMap),
	gatesConstraint(Gates,VarsMap,HealthMap,FaultyBits,FaultyBitsMap,(Bools1-Bools2,Ints1-Ints2),Cs1-Cs2),
	boundConstraint(MC,FaultyBits,(Bools2-Bools3,Ints2-Ints3),Cs2-Cs3),
	Cs3 = [], Bools3 = [], Ints3 = [].
	
/************************************************/
/******* Step 0 - Remove irrelevant cones *******/
/************************************************/
%TODO, add also cones whose dominator has [s0,s1].
remove_irrelevant_cones(_G, [], []).
remove_irrelevant_cones(Gates, [(DomID,DominatedGates)|Cones], [(DomID,DominatedGates)|Cones1]) :-
	getGateModes(DomID, Gates, Modes),
	member(f,Modes),!,
	remove_irrelevant_cones(Gates, Cones, Cones1).
	
remove_irrelevant_cones(Gates, [(DomID,_DominatedGates)|Cones], Cones1) :-
	getGateModes(DomID, Gates, Modes),
	\+member(f,Modes),!,
	remove_irrelevant_cones(Gates, Cones, Cones1).
	
getGateModes(ID, [(ID,_,_,_,_,Modes)|_], Modes) :-!.
getGateModes(ID, [_Gate|Gates], Modes) :- getGateModes(ID, Gates, Modes).
getGateModes(ID, [], _) :- write('error: could not find gate '),writeln(ID).
