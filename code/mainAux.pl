:- module('mainAux', [ getSysID/2, getSysPrecents/2, filter_cones_by_classification/4 ]).


%SysFile = <SysID>_<Precents>
getSysID(SysFile, SysID) :- atomic_list_concat([SysID|_], '_', SysFile).

%SysFile = <SysID>_<Precents>
getSysPrecents(SysFile, SysPrecents) :- atomic_list_concat([_,SysPrecents], '_', SysFile).

		
/*******************************************************/
/******* Step 0 - Filter cones by classification *******/
/*******************************************************/

filter_cones_by_classification(SysFile,ObsNum,AllCones,SmartCones) :- !,
	working_directory(CWD,CWD),
	getSysID(SysFile,SysID),
	concat_atom(['input/',SysID,'/',SysID,'_allowedcones/',SysFile,'_',ObsNum,'_allowedcones.txt'],FileDir),
	writeln(FileDir),
	open(FileDir, read, FStream),
	read_term(FStream,AllowedCones, []),
	filter_cones_by_classification_aux(AllowedCones,AllCones,SmartCones).
	%writeln(allowed:AllowedCones),
	%writeln(all:AllCones),
	%writeln(afterfilter:SmartCones).

filter_cones_by_classification_aux(_AllowedCones,[],[]).
filter_cones_by_classification_aux(AllowedCones, [(DomID,DominatedGates)|Cones], [(DomID,DominatedGates)|Cones1]) :-
	member(DomID,AllowedCones),!,%writeln(not:DomID),
	filter_cones_by_classification_aux(AllowedCones, Cones, Cones1).
	
filter_cones_by_classification_aux(AllowedCones, [(DomID,_DominatedGates)|Cones], Cones1) :-
	\+member(DomID,AllowedCones),!,%writeln(yes:DomID),
	filter_cones_by_classification_aux(AllowedCones, Cones, Cones1).
