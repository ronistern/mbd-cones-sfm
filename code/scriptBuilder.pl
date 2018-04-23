%Author: Orel Elimelech

:- module('scriptBuilder', [  generateScript/3, createOutputFolder/1 ]).

/*****************************/
/******* Output Folder *******/
/*****************************/
createOutputFolder(SysID) :-
	%initialize the output path
	format('Creating output folder for system ~w...\n', [SysID]),
	working_directory(CWD,CWD), 
	concat_atom([CWD,'output/',SysID,'/'],SysDir),
	
	%validate that user is sure he wants to delete all contents
	format('Are you sure you want to delete all content from:\n~w\ny\\n?', [SysDir]),
	get_char(INPUT_CHAR),
	writeln(INPUT_CHAR),
	((INPUT_CHAR = 'y' ; INPUT_CHAR = 'Y') ->
		format('Deleting all content from ~w...\n', [SysDir]) ;
		(writeln('Quitting.\n'),halt)),
	
	%delete directory if exists
	(exists_directory(SysDir) -> (format('Please delete\n~w\nand then try again\n', [SysDir]), halt) ; true),
	
	%create directory
	(exists_directory(SysDir) -> true ; make_directory(SysDir)),
	format('Created new directory ~w...\n', [SysDir]),
	
	SystemModesList = ['0','0.25','0.5','0.75','1'],
	%AlgorithmsList = ['nc','p','smartstrong','smartweak','so','wo'],
	%AlgorithmsList = ['nc','p','wo','so'],
	%AlgorithmsList = ['p'],
	AlgorithmsList = ['wo','smartweak'],
	
	init_modes_folders(SysID, SysDir, SystemModesList, AlgorithmsList),
	
	format('Done, output folder of ~w is ready', [SysID]).

init_modes_folders(_SysID, _SysDir, [], _AlgorithmsList).
init_modes_folders(SysID, SysDir, [Mode|Modes], AlgorithmsList) :- format('\tCreating mode folder ~w ...\n', [Mode]),
	concat_atom([SysDir,'/',SysID,'_',Mode],SysModeDir),
	make_directory(SysModeDir),
	init_algorithms_folders(SysModeDir, AlgorithmsList),
	init_modes_folders(SysID, SysDir, Modes, AlgorithmsList).

init_algorithms_folders(_SysModeDir, []).
init_algorithms_folders(SysModeDir, [Alg|Algs]) :- format('\t\tCreating algorithm folder ~w ...\n', [Alg]),
	concat_atom([SysModeDir,'/',Alg],SysAlgDir),
	make_directory(SysAlgDir),
	init_algorithms_folders(SysModeDir,Algs).
	
/**********************/
/******* Script *******/
/**********************/
generateScript(SysID, ObsID, Timeout) :-
	writeln('Creating script file ...'),
	concat_atom([SysID,'_script.sh'],ScriptFile),
	open(ScriptFile,write,FStream),
	getSysFiles(SysID, SysFiles),
	%Algs = [nc,p,wo,so],
	%Algs = [p],
	%Algs = [nc,p,wo,so,smartweak,smartstrong],
	Algs = [wo,smartweak],
	generateScriptAllSystems(SysFiles, FStream, SysID, ObsID, Timeout, Algs),
	close(FStream),
	writef('Done, you can find your file at %w',[ScriptFile]).
	
generateScriptAllSystems([], _FStream, _SysID, _ObsID, _Timeout, _Algs).
generateScriptAllSystems([SysFile|SysFiles], FStream, SysID, ObsID, Timeout, Algs) :-
	getOBStream(SysID, ObsID, OBStream),
	writeLinesToScriptFile_obs(FStream,OBStream,SysFile,SysID,ObsID,Timeout,Algs),
	close(OBStream),
	generateScriptAllSystems(SysFiles, FStream, SysID, ObsID, Timeout, Algs).


%./timeoutscript.sh -t20 swipl -G100g -T20g -L1g -g 
%"['code/main.pl'], nl, solveObs_strong_optimistic('c432_0.25', iscas85, 1), halt" -t 'halt(1).'
writeLinesToScriptFile_obs(FStream,OBStream,SysFile,SysID,ObsID,Timeout,Algs) :-
	read_term(OBStream, OBS, []),
	OBS=(SysID,ObsNum,_),!,
	writeLinesToScriptFile_algs(Algs,FStream,SysFile,ObsID,ObsNum,Timeout),
	writeLinesToScriptFile_obs(FStream,OBStream,SysFile,SysID,ObsID,Timeout,Algs).
	
writeLinesToScriptFile_obs(_,OBStream,_,_,_,_,_) :- read_term(OBStream, end_of_file, []).

writeLinesToScriptFile_algs([],_FStream,_SysFile,_ObsID,_ObsNum,_Timeout).
writeLinesToScriptFile_algs([Alg|Algs], FStream, SysFile, ObsID, ObsNum, Timeout) :-
	algIDToPredicate(Alg,AlgPredicate),
	write(FStream,"./timeoutscript.sh -t"),
	write(FStream,Timeout),
	write(FStream," swipl -G100g -T20g -L1g -g ""['code/main.pl'], nl, "),
	write(FStream, AlgPredicate),
	write(FStream, "('"),
	write(FStream, SysFile),
	write(FStream,"', "),
	write(FStream, ObsID),
	write(FStream, ", "),
	write(FStream, ObsNum),
	write(FStream,"), halt"""),
	write(FStream," -t 'halt(1).'\n"),
	writeLinesToScriptFile_algs(Algs, FStream, SysFile, ObsID, ObsNum, Timeout).

%[nc,p,wo,so],
algIDToPredicate(nc,"solveObs_no_cones").
algIDToPredicate(p,"solveObs_pessimistic").
algIDToPredicate(wo,"solveObs_weak_optimistic").
algIDToPredicate(so,"solveObs_strong_optimistic").
algIDToPredicate(smartweak,"solveObs_smart_weak_optimistic").
algIDToPredicate(smartstrong,"solveObs_smart_strong_optimistic").


getOBStream(SysID, ObsID, OBStream) :-
	working_directory(CWD,CWD), 
	concat_atom([CWD,'input/',SysID,'/',SysID,'_',ObsID,'.obs'],ObsFile),
	open(ObsFile, read, OBStream).
	
getSysFiles(SysID, SysFiles) :-
	working_directory(CWD,CWD), 
	concat_atom([CWD,'input/',SysID],SysDir),
	directory_files(SysDir,DirFiles),
	keepSysFilesOnly(DirFiles,SysFiles1),
	sort(SysFiles1,SysFiles).

%filters the list and keeps only .sys files
keepSysFilesOnly([],[]).
keepSysFilesOnly([File|Files],[SysFile|Files1]) :-
	atom_concat(SysFile,'.sys',File),!,
	keepSysFilesOnly(Files,Files1).
keepSysFilesOnly([_|Files],Files1) :-
	keepSysFilesOnly(Files,Files1).




