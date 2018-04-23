:- module('sysDiag_weak_optimistic', [ diagnostics_weak_optimistic/3 ]).

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
%diagnostics_weak_optimistic(+,+,+).
diagnostics_weak_optimistic(SysFile, System, Obs) :-
	Obs = (SysID,ObsNum,ObsIO),
	System = system(SysID, _In, _Out, Gates, _Sec, AllCones),
	length(Gates, GatesCnt), length(AllCones,AllConesCnt),%writeln(AllCones),
	writef("------\n**System Info.**\nSystem file: %w, System ID: %w, Obs#: %w\n", [SysFile, SysID, ObsNum]),
	writef("|Gates|: %w, |All Cones|: %w\n", [GatesCnt, AllConesCnt]),
	sysInfoToFile("wo", SysFile, ObsNum, AllConesCnt),
	
	getFinalConesFile(SysFile, "wo", ObsNum, FinalConesFile),
	getBeeResultsFile(SysFile, "wo", ObsNum, BeeResultsFile),
	
	format('Number of initial cones: ~w\n', [AllConesCnt]),
	
	%Step 1 - find MC
	get_time(T0),
	length(Gates, UpBound),
	validMCdiag(Gates, AllCones, ObsIO, UpBound, _MCdiag, MC, FinalCones,FinalConesFile, BeeResultsFile),
	%writeln('*********************'),
	length(FinalCones, FinalConesCnt),
	ConesRemoved is AllConesCnt - FinalConesCnt,
	get_time(T1),
	MCTime is T1 - T0,
	mcToFile("wo", SysFile, ObsNum, 0, ConesRemoved, MC, MCTime),
	writef("------\n**MC**\nMC: %w, MC time: %w\n", [MC, MCTime]),
	writef("|Final Cones|: %w, |Cones Removed|: %w\n", [FinalConesCnt, ConesRemoved]),!.

validMCdiag(Gates, CurrCones, ObsIO, UpBound, MCdiag, MC, FinalCones,FinalConesFile, BeeResultsFile) :-
	%write current cones abstraction to file
	%length(CurrCones,L),
	%writeln(lenweak:L),
	tell(FinalConesFile),
	writeln(CurrCones),
	told,
	
	addFlipToDominators(Gates, ModifiedGates, CurrCones),
	get_time(T0),
	%tell(BeeResultsFile),
	run_mc_weak_optimistic(ModifiedGates, CurrCones, ObsIO, UpBound, MCdiag1, MC1), %writeln(diag1:MCdiag1),
	%told,
	get_time(T1),
	Tweakopt is T1-T0,
	writeln(tweakopt:Tweakopt),
	%( MC1 is -1 ->
	%(MCdiag = MCdiag1, MC = MC1, FinalCones = CurrCones)
	%;
	%(
	get_time(T2),
	simulateSys(ModifiedGates, ObsIO, MCdiag1, VarsMap, _FBitsMap), %writeln(diag:MCdiag1),
	get_time(T3),
	TsimSys is T3-T2,
	writeln(tsimulatesys:TsimSys),
	writeln(beforeremoveungroundable),
	removeUngroundableCone(VarsMap, Gates, CurrCones, MCdiag1, FixedMCdiag, NewCones),
	writeln(afterremoveungroundable),
	get_time(T4),
	TRemoveungroundable is T4-T3,
	writeln(tRemoveungroundable:TRemoveungroundable),
	(CurrCones = NewCones ->
		(MCdiag = FixedMCdiag, MC = MC1, FinalCones = CurrCones)
		;
		(%writef('\n\nRESET\n\n',[]),
		%writeln(conesBefore:CurrCones), nl, writeln(conesAfter:NewCones),
		validMCdiag(Gates, NewCones, ObsIO, UpBound, MCdiag, MC, FinalCones,FinalConesFile, BeeResultsFile))
	).

%Remove cone if it's ungroundable and retry with a smaller cones list.
%If all cones are found groundable, FixedMCdiag is the grounded diagnosis.
removeUngroundableCone(_VarsMap, _Gates, CurrCones, [], [], NewCones) :-
	CurrCones = NewCones.
removeUngroundableCone(VarsMap, Gates, CurrCones, MCdiag, FixedMCdiag, NewCones) :-
	MCdiag = [p(Dominator,FakeFault)|RestFromDiag],
	member((Dominator,_),CurrCones),
	FixedMCdiag = [p(FinalGate, Fault)|RestFromFixedDiag], writef("\n\tdominator:%w\n",[Dominator]),
	(isValidCone(VarsMap,Gates,CurrCones,Dominator,FakeFault,FinalGate,Fault) ->
		(%writeln(p(Dominator,FakeFault)),writeln(p(FinalGate,Fault)),nl,
		removeUngroundableCone(VarsMap, Gates,CurrCones,RestFromDiag,RestFromFixedDiag,NewCones))
		;
		select((Dominator,_), CurrCones, NewCones)
	).

%Gate may be just a free one and not a dominator
removeUngroundableCone(VarsMap, Gates, CurrCones, MCdiag, FixedMCdiag, NewCones) :- 
	MCdiag = [p(FreeGate,Fault)|RestFromDiag],
	\+member((FreeGate,_),CurrCones), writef("\n\tfree gate:%w",[FreeGate]),
	FixedMCdiag = [p(FreeGate, Fault)|FinalDiagGates],
	removeUngroundableCone(VarsMap, Gates, CurrCones, RestFromDiag, FinalDiagGates, NewCones).

/*******************************/
/******* Cone Validation *******/
/*******************************/
%A cone is valid if:
%1) Its dominator gate is valid.
%2) One of its dominated gates can produce the desired cone output.
%FakeFault - is the original dominator's fault.
%FinalGate and Fault are the correct pair of (FaultyGateID,FaultType).
isValidCone(VarsMap, Gates,CurrCones,Dominator,FakeFault,FinalGate,Fault) :- writef("\tdominator:%w\n",[Dominator]),
	%Get cone output value
	select((Dominator,ConeOutput,_,_,_,DomModes),Gates,_),writeln(a),
	getValue(ConeOutput,VarsMap,ConeOutputVal),writeln(b),
	%Get cone input values
	getCandidates(CurrCones, Dominator, ConeGatesIDs),writeln(c),
	getConeGates(Gates, ConeGatesIDs, OrigConeGates),writeln(d),
	getConeInputs(OrigConeGates, ConeInputs),writeln(e),
	getInputsValues(ConeInputs, VarsMap, ConeInputsVals), writef("\n\tdom val:%w\n\tins: %w\n",[ConeOutputVal,ConeInputsVals]),
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
	writef("\t\tDominated:%w\n", [Dominated]),
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
	
/****************************/
/******* MC Diagnosis *******/
/****************************/
run_mc_weak_optimistic(Gates,Cones,ObsIO,UpBound,Solution,MC) :-
	runExprMin(instance(Gates,Cones,ObsIO,UpBound),Solution,
		sysDiag_weak_optimistic:encodeMC_weak_optimistic,
		sysDecode:decode,
		sysVerify:verify),
	(Solution=unsat -> MC is -1 ; length(Solution,MC)).

encodeMC_weak_optimistic(instance(Gates,Cones,ObsIO,UpBound),map(HealthMap,FaultyBitsMap),FBitsSum,Cs1) :-
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