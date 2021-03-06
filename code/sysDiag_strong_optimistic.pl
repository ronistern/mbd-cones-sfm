:- module('sysDiag_strong_optimistic', [ diagnostics_strong_optimistic/3 ]).

:- use_module(bee('satsolver/satsolver.pl')).
:- use_module(bee('beeCompiler/bCompiler.pl')).
:- use_module(bee('bApplications/auxs/auxRunExpr')).
:- use_module(bee('bApplications/auxs/auxRunExprAll.pl'), [runExprAll/5]).

:- use_module(mainAux).
:- use_module(dataWriter).
:- use_module(sysEncode).
:- use_module(sysDecode).
:- use_module(sysVerify).

:- use_module(sysConeGrounding).
:- use_module(sysPropagation).
:- use_module('aux/auxListsNew').

/***************************/
/******* Main Method *******/
/***************************/
%diagnostics_strong_optimistic(+,+,+).
diagnostics_strong_optimistic(SysFile, System, Obs) :-
	Obs = (SysID,ObsNum,ObsIO),
	System = system(SysID, _In, _Out, Gates, _Sec, AllCones),
	length(Gates, GatesCnt), length(AllCones,AllConesCnt),
	writef("------\n**System Info.**\nSystem file: %w, System ID: %w, Obs#: %w\n", [SysFile, SysID, ObsNum]),
	writef("|Gates|: %w, |All Cones|: %w\n", [GatesCnt, AllConesCnt]),
	sysInfoToFile("so", SysFile, ObsNum, AllConesCnt),
	
	getFinalConesFile(SysFile, "so", ObsNum, FinalConesFile),
	getBeeResultsFile(SysFile, "so", ObsNum, BeeResultsFile),
	
	%Step 1 - find MC
	get_time(T0),
	length(Gates, UpBound),
	validMCdiag(Gates, AllCones, ObsIO, UpBound, _MCdiag, MC, FinalCones,FinalConesFile, BeeResultsFile),
	length(FinalCones, FinalConesCnt), %writef("start: %w, final: %w",[AllConesCnt,FinalConesCnt]),
	ConesRemoved is AllConesCnt - FinalConesCnt,
	get_time(T1),
	MCTime is T1 - T0,
	mcToFile("so", SysFile, ObsNum, 0, ConesRemoved, MC, MCTime),
	writef("------\n**MC**\nMC: %w, MC time: %w\n", [MC, MCTime]),
	writef("|Final Cones|: %w, |Cones Removed|: %w\n", [FinalConesCnt, ConesRemoved]),!.

validMCdiag(Gates, CurrCones, ObsIO, UpBound, MCdiag, MC, FinalCones,FinalConesFile, BeeResultsFile) :-
	%write current cones abstraction to file
	%length(CurrCones,L),
	%writeln(lenstrong:L),
	tell(FinalConesFile),
	writeln(CurrCones),
	told,
	
	%Find desired MC first.
	addFlipToDominators(Gates, ModifiedGates, CurrCones), %length(CurrCones,ConesCnt), writef("\ncurr:%w\n",[ConesCnt]),
	tell(BeeResultsFile),
	run_mc_strong_optimistic(ModifiedGates, CurrCones, ObsIO, UpBound, MCdiag1, MC1),
	told,
	%( MC1 is -1 ->
	%(MCdiag = MCdiag1, MC = MC1, FinalCones = CurrCones)
	%;
	%(
	%Tries to ground the first diagnosis
	%get_time(T0),
	groundDiagnosis([MCdiag1], Gates, ModifiedGates, CurrCones, ObsIO, GroundedDiag1, UngroundableCones1),
	%get_time(T1),
	%TGrounding is T1-T0,
	%writeln(grounding1:TGrounding),
	(UngroundableCones1 = [] ->
		%The first diagnosis was succesfully grounded
		(MC = MC1, CurrCones = FinalCones, MCdiag = GroundedDiag1)
		;
		%Couldn't ground the first diagnosis
		%Find all MC abstract diagnoses.
		(tell(BeeResultsFile),
		%writeln(runall),
		%get_time(T2),
		run_all_strong_optimistic(ModifiedGates, CurrCones, ObsIO, MC1, AllDiags),
		%get_time(T3),
		%TallSol is T3-T2,
		%writeln(runall:TallSol),
		%writeln(endrunall),
		told,
		%length(CurrCones,ConesCnt),writef("ConesCnt: %w\nAll Diags: %w\nCones: %w\n\n",[ConesCnt, AllDiags,CurrCones]),
		%get_time(T4),
		groundDiagnosis(AllDiags, Gates, ModifiedGates, CurrCones, ObsIO, GroundedDiag, UngroundableCones),
		%get_time(T5),
		%TGrounding2 is T5-T4,
		%writeln(grounding2:TGrounding2),
		(UngroundableCones = [] ->
			%One of the abstract diagnoses was succesfully grounded
			(MC = MC1, CurrCones = FinalCones, MCdiag = GroundedDiag)
			;
			%Couldn't find a groundable diagnosis, remove cone and continue.
			(removeCone(CurrCones, NewCones, UngroundableCones),
			validMCdiag(Gates, NewCones, ObsIO, UpBound, MCdiag, MC, FinalCones,FinalConesFile, BeeResultsFile))
		)
		)
	).
		
	
	

removeCone([],[],_).
removeCone(OldCones, NewCones,[ToRemove|_]) :- select(ToRemove, OldCones, NewCones).

%Iterates over all abstract diagnoses and try to ground one of them.
%If succeed, returns the grounded diagnosis.
%Otherwise (all diagnoses can't be grounded), fail.
groundDiagnosis([], _Gates, _ModifiedGates, _CurrCones, _ObsIO, _GroundedDiag, []).
groundDiagnosis([Diag|Diags], Gates, ModifiedGates, CurrCones, ObsIO, GroundedDiag, UngroundableCones) :- %writef("\tDiag: %w\n",[Diag]),
	simulateSys(ModifiedGates, ObsIO, Diag, VarsMap, _FBitsMap),
	removeUngroundableCone(VarsMap, Gates, CurrCones, Diag, FixedDiag, NewCones, BadCone),
	(CurrCones = NewCones ->
		(GroundedDiag = FixedDiag, UngroundableCones = [])
		;
		(UngroundableCones = [BadCone|RestBadCones], %writeln(cone:BadCone),
		groundDiagnosis(Diags, Gates, ModifiedGates, CurrCones, ObsIO, GroundedDiag, RestBadCones))
	).

%Remove cone if it's ungroundable and retry with a smaller cones list.
%If all cones are found groundable, FixedMCdiag is the grounded diagnosis.
removeUngroundableCone(_VarsMap, _Gates, CurrCones, [], [], NewCones, none) :-
	CurrCones = NewCones.
removeUngroundableCone(VarsMap, Gates, CurrCones, MCdiag, FixedMCdiag, NewCones, BadCone) :- 
	MCdiag = [p(Dominator,FakeFault)|RestFromDiag],
	member((Dominator,_),CurrCones), %writef("\t\tDominator: %w\n",[Dominator]),
	FixedMCdiag = [p(FinalGate, Fault)|RestFromFixedDiag], %writef("\n\tdominator:%w\n",[Dominator]),
	(isValidCone(VarsMap,Gates,CurrCones,Dominator,FakeFault,FinalGate,Fault) ->
		(%writeln(p(Dominator,FakeFault)),writeln(p(FinalGate,Fault)),nl,
		removeUngroundableCone(VarsMap, Gates,CurrCones,RestFromDiag,RestFromFixedDiag,NewCones, BadCone))
		;
		(select((Dominator,Cone), CurrCones, NewCones), BadCone = (Dominator,Cone))
		
	).

%Gate may be just a free one and not a dominator
removeUngroundableCone(VarsMap, Gates, CurrCones, MCdiag, FixedMCdiag, NewCones, BadCone) :- 
	MCdiag = [p(FreeGate,Fault)|RestFromDiag],
	\+member((FreeGate,_),CurrCones), %writef("\t\tFreegate: %w\n",[FreeGate]),
	FixedMCdiag = [p(FreeGate, Fault)|FinalDiagGates],
	removeUngroundableCone(VarsMap, Gates, CurrCones, RestFromDiag, FinalDiagGates, NewCones, BadCone).

/*******************************/
/******* Cone Validation *******/
/*******************************/
%A cone is valid if:
%1) Its dominator gate is valid.
%2) One of its dominated gates can produce the desired cone output.
%FakeFault - is the original dominator's fault.
%FinalGate and Fault are the correct pair of (FaultyGateID,FaultType).
isValidCone(VarsMap, Gates,CurrCones,Dominator,FakeFault,FinalGate,Fault) :- %writef("\tdominator:%w\n",[Dominator]),
	%Get cone output value
	select((Dominator,ConeOutput,_,_,_,DomModes),Gates,_),
	getValue(ConeOutput,VarsMap,ConeOutputVal),
	%Get cone input values
	getCandidates(CurrCones, Dominator, ConeGatesIDs),
	getConeGates(Gates, ConeGatesIDs, OrigConeGates),
	getConeInputs(OrigConeGates, ConeInputs),
	getInputsValues(ConeInputs, VarsMap, ConeInputsVals), %writef("\n\tdom val:%w\n\tins: %w\n",[ConeOutputVal,ConeInputsVals]),
	((dominatorIsValid(DomModes,ConeOutputVal,FakeFault,Fault),Dominator=FinalGate,!) ; 
	oneDominatedIsValid(ConeGatesIDs, OrigConeGates, ConeOutput, ConeOutputVal, ConeInputsVals,FinalGate,Fault)).

%A dominator gate is valid if:
%1) It has {Flip} mode.
%2) It has {Stuck_at_1} mode and the cone output value is 1.
%3) It has {Stuck_at_0} mode and the cone output value is 0.
dominatorIsValid(Modes,1,_,s1) :- member(s1,Modes).
dominatorIsValid(Modes,-1,_,s0) :- member(s0,Modes).
dominatorIsValid(Modes,_,f,f) :- member(f,Modes).

%Given a dominator gate and a list of candidates to replace it,
%finds a list of valid candidates such that C belongs to ValidCandidates iff
%the output of the dominator when it's faulty, equals the output of it when it's healthy and C is faulty.
oneDominatedIsValid([], _OrigConeGates, _ConeOutput, _ConeOutputVal, _ConeInputsVals,_FinalGate,_Fault) :- fail.
oneDominatedIsValid([Dominated|Doms], OrigConeGates, ConeOutput, ConeOutputVal, ConeInputsVals,FinalGate,Fault) :- 
	%writef("\t\tDominated:%w\n", [Dominated]),
	(
		modeIsOk(Dominated, OrigConeGates, ConeOutput, ConeOutputVal, ConeInputsVals,[1,-1,-1]),
		FinalGate = Dominated, Fault = f) 
	;
	(
		modeIsOk(Dominated, OrigConeGates, ConeOutput, ConeOutputVal, ConeInputsVals,[-1,1,-1]),
		FinalGate = Dominated, Fault = s0
	) ;
	(
		modeIsOk(Dominated, OrigConeGates, ConeOutput, ConeOutputVal, ConeInputsVals,[-1,-1,1]),
		FinalGate = Dominated, Fault = s1
	) ;
	oneDominatedIsValid(Doms, OrigConeGates, ConeOutput, ConeOutputVal, ConeInputsVals,FinalGate,Fault).

/*************************************/
/******* Step 1 - MC Diagnosis *******/
/*************************************/
run_mc_strong_optimistic(Gates,Cones,ObsIO,UpBound,Solution,MC) :-
	runExprMin(instance(Gates,Cones,ObsIO,UpBound),Solution,
		sysDiag_strong_optimistic:encodeMC_strong_optimistic,
		sysDecode:decode,
		sysVerify:verify),
	(Solution=unsat -> MC is -1 ; length(Solution,MC)).

encodeMC_strong_optimistic(instance(Gates,Cones,ObsIO,UpBound),map(HealthMap,FaultyBitsMap),FBitsSum,Cs1) :-
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
	
/***********************************************/
/******* Step 2 - All Abstract Diagnoses *******/
/***********************************************/	
run_all_strong_optimistic(Gates,Cones,ObsIO,MC,Solutions) :-
	runExprAll(instance(Gates,Cones,ObsIO,MC),Solutions,
            sysDiag_strong_optimistic:encode_all_strong_optimistic,
            sysDecode:decode,
            sysVerify:verify).

encode_all_strong_optimistic(instance(Gates,Cones,ObsIO,MC),map(HealthMap,FaultyBitsMap),(Bools1,Ints1),Cs1):-
	get_time(T0),
	createMapsFromGates(Gates,ObsIO,VarsMap,HealthMap),
	initDominatedGates(Cones,HealthMap),
	gatesConstraint(Gates,VarsMap,HealthMap,FaultyBits,FaultyBitsMap,(Bools1-Bools2,Ints1-Ints2),Cs1-Cs2),
	boundConstraint(MC,FaultyBits,(Bools2-Bools3,Ints2-Ints3),Cs2-Cs3),
	Cs3 = [], Bools3 = [], Ints3 = [],
	get_time(T1),
	EncodingTime is T1-T0,
	format('~w,',[EncodingTime]).
	
/******************************************/
/******* Add flip to all dominators *******/
/******************************************/
addFlipToDominators([], [], _Cones).
addFlipToDominators([(GateID,Out,In,T,LT,Modes)|Gates], [(GateID,Out,In,T,LT,Modes2)|Gates1], Cones):-
	member((GateID,_),Cones),!,
	append([f],Modes,Modes1),
	sort(Modes1,Modes2),
	addFlipToDominators(Gates,Gates1,Cones).
addFlipToDominators([(GateID,Out,In,T,LT,Modes)|Gates], [(GateID,Out,In,T,LT,Modes)|Gates1], Cones):-
	\+member((GateID,_),Cones),!,
	addFlipToDominators(Gates,Gates1,Cones).
