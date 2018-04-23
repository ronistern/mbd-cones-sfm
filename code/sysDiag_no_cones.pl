:- module('sysDiag_no_cones', [ diagnostics_no_cones/3 ]).

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

/***************************/
/******* Main Method *******/
/***************************/
%diagnostics_no_cones(+,+,+).
diagnostics_no_cones(SysFile, System, Obs) :-
	Obs = (SysID,ObsNum,ObsIO),
	System = system(SysID, _In, _Out, Gates, _Sec, _AllCones),
	%length(Gates, GatesCnt),
	writef("System file: %w, System ID: %w, Obs#: %w\n", [SysFile, SysID, ObsNum]),
	sysInfoToFile("nc", SysFile, ObsNum, 0),
	
	getFinalConesFile(SysFile, "nc", ObsNum, FinalConesFile),
	getBeeResultsFile(SysFile, "nc", ObsNum, BeeResultsFile),
	
	%getOutFile(SysFile, "nc", ObsNum, OutFile),
	%('c880_0',1,nc,0,0,0,1,3.2116036415100098).
	
	%Step 1 - find MC
	get_time(T0),
	length(Gates, UpBound),
	
	tell(FinalConesFile),
	writeln('[]'),
	told,
	
	tell(BeeResultsFile),
	run_mc_no_cones(Gates,ObsIO,UpBound,_MCdiag,MC),
	told,
	get_time(T1),
	MCTime is T1 - T0,
	mcToFile("nc", SysFile, ObsNum, 0, 0, MC, MCTime),
	writef("MC: %w, MC time: %w\n", [MC, MCTime]).
	
	/*
	%Step 2 - find all diagnoses
	(run_allmcdiags_no_cones(Gates,ObsIO,MC,AllMCDiags) -> true; AllMCDiags = [MCdiag]),
	length(AllMCDiags,AllMCDiagsCnt),
	get_time(T2),
	AllMCDiagsTime is T2-T1,
	writef("All mc diags count: %w, All mc diags time: %w\nAll mc diags: %w\n", [AllMCDiagsCnt, AllMCDiagsTime, AllMCDiags]),!.
	*/
/***************************/
/******* Step 1 - MC *******/
/***************************/
run_mc_no_cones(Gates,ObsIO,UpBound,Solution,MC) :-
	runExprMin(instance(Gates,ObsIO,UpBound),Solution,
		sysDiag_no_cones:encodeMC_no_cones,
		sysDecode:decode,
		sysVerify:verify),
	(Solution=unsat -> MC is -1 ; length(Solution,MC)).
	
encodeMC_no_cones(instance(Gates,ObsIO,UpBound),map(HealthMap,FaultyBitsMap),FBitsSum,Cs1) :-
	get_time(T0),
	createMapsFromGates(Gates,ObsIO,VarsMap,HealthMap),
	gatesConstraint(Gates,VarsMap,HealthMap,FaultyBits,FaultyBitsMap,(_,_),Cs1-Cs2),
	Cs2 = [new_int(FBitsSum,1,UpBound), bool_array_sum_eq(FaultyBits,FBitsSum)|Cs3],
	boundConstraint(UpBound,FaultyBits,(_,_),Cs3-Cs4),
	Cs4 = [],
	get_time(T1),
	EncodingTime is T1-T0,
	format('~w,',[EncodingTime]).
	
/**************************************/
/******* Step 2 - All diagnoses *******/
/**************************************/
run_allmcdiags_no_cones(Gates,ObsIO,MC,Solutions) :-
	runExprAll(instance(Gates,ObsIO,MC),Solutions,
            sysDiag_no_cones:encode_AllMCDiags_no_cones,
            sysDecode:decode,
            sysVerify:verify).
	
encode_AllMCDiags_no_cones(instance(Gates,ObsIO,MC),map(HealthMap,FaultyBitsMap),(Bools1,Ints1),Cs1):-
	createMapsFromGates(Gates,ObsIO,VarsMap,HealthMap),
	gatesConstraint(Gates,VarsMap,HealthMap,FaultyBits,FaultyBitsMap,(Bools1-Bools2,Ints1-Ints2),Cs1-Cs2),
	boundConstraint(MC,FaultyBits,(Bools2-Bools3,Ints2-Ints3),Cs2-Cs3),
	Cs3 = [], Bools3 = [], Ints3 = [].
