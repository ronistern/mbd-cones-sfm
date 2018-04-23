%Author: Orel Elimelech

:- module('raw2csv', [  collectRawData/1 ]).

:- use_module(mainAux).

/******************************/
/******* Raw to Results *******/
/******************************/

collectRawData(SysID) :-
	working_directory(CWD,CWD),
	concat_atom([CWD,'output/',SysID,'/'],Root),
	directory_files(Root,SystemsDirs1),
	keepDirsOnly(SystemsDirs1, SystemsDirs),
	concat_atom([CWD,'output/',SysID,'_raw.csv'],ResFile),
	open(ResFile,write,ResFID),
	format(ResFID, "~w,~w,~w,~w,", ["SystemID", "Algorithm", "SystemModes", "ObsIndex"]),
	format(ResFID, "~w,~w,~w,~w,~w\n", ["PreprocessTime", "Cones", "BrokenCones", "MC", "MCTime"]),
	collect_raw_systems(SystemsDirs,Root,ResFID),
	close(ResFID).
	
keepDirsOnly(InList, OutList) :- 
	(member('.',InList) -> select('.',InList,InList1) ; InList = InList1),
	(member('..',InList1) -> select('..',InList1,OutList) ; InList1 = OutList).

%removes any string that contains FilterString
filter_by_string([],[],_FilterString).
filter_by_string([In|Ins],Outs,FilterString) :-
	atom_string(In,InStr),%writeln(str:InStr),writeln(filter:FilterString),
	atomic_list_concat(ResList, FilterString, InStr),%writeln(res:ResList),
	length(ResList,1) -> (Outs=[In|Outs1],filter_by_string(Ins,Outs1,FilterString)) ; filter_by_string(Ins,Outs,FilterString).

collect_raw_systems([],_,_).
collect_raw_systems([SysName|Systems],Root,ResFID) :-
	concat_atom([Root,SysName],SysDir),
	directory_files(SysDir,SystemAlgorithmsDirs1),
	keepDirsOnly(SystemAlgorithmsDirs1,SystemAlgorithmsDirs),
	getSysPrecents(SysName, SysPrecents),
	getSysID(SysName, SysID),
	collect_raw_data(SystemAlgorithmsDirs, SysID, SysPrecents, SysDir, ResFID),
	collect_raw_systems(Systems,Root,ResFID).
	
collect_raw_data([],_,_,_,_).
collect_raw_data([AlgName|AlgNames], SysID, SysPrecents, SysDir, ResFID) :-
	concat_atom([SysDir,'/',AlgName,'/'],DataFolder),
	directory_files(DataFolder,RawDataFiles1),
	keepDirsOnly(RawDataFiles1, RawDataFiles2),
	filter_by_string(RawDataFiles2,RawDataFiles3,"finalcones"),%writeln(RawDataFiles2),writeln(RawDataFiles3),nl,
	sort(RawDataFiles3, RawDataFiles),
	scan_raw_data(RawDataFiles, DataFolder, SysID, SysPrecents, AlgName, ResFID),
	collect_raw_data(AlgNames, SysID, SysPrecents, SysDir, ResFID).
	
scan_raw_data([], _DataFolder, _SysID, _SysPrecents, _SysAlgorithmDir, _ResFID).

%FAILURE
scan_raw_data([DataFile|Files], DataFolder, SysID, SysPrecents, AlgName, ResFID) :- 
	concat_atom([DataFolder,'/',DataFile],FullFilePath),
	open(FullFilePath,read,FID),
	read_term(FID, RawData, []),
	close(FID),
	RawData = (_SysFile, ObsIdx,_Alg,_PPTime,_InitCones,_ConesRemoved,MC,_MCTime),MC = -1, !,
	format(ResFID, "~w,~w,~w,~w,", [SysID, AlgName, SysPrecents, ObsIdx]),
	format(ResFID, "~w,~w,~w,~w,~w\n", ["-", "-", "-", "unsat", "-"]),
	scan_raw_data(Files, DataFolder, SysID, SysPrecents, AlgName, ResFID).
	
%SUCCESS
scan_raw_data([DataFile|Files], DataFolder, SysID, SysPrecents, AlgName, ResFID) :- 
	concat_atom([DataFolder,'/',DataFile],FullFilePath),
	open(FullFilePath,read,FID),
	read_term(FID, RawData, []),
	close(FID),
	RawData = (_SysFile,ObsIdx,_Alg,PPTime,InitCones,ConesRemoved,MC,MCTime),!,
	format(ResFID, "~w,~w,~w,~w,", [SysID, AlgName, SysPrecents, ObsIdx]),
	format(ResFID, "~w,~w,~w,~w,~w\n", [PPTime, InitCones, ConesRemoved, MC, MCTime]),
	scan_raw_data(Files, DataFolder, SysID, SysPrecents, AlgName, ResFID).
	
%TIMEOUT
scan_raw_data([DataFile|Files], DataFolder, SysID, SysPrecents, AlgName, ResFID) :-
	concat_atom([DataFolder,'/',DataFile],FullFilePath),
	open(FullFilePath,read,FID),
	close(FID),
	atomic_list_concat([_,_,ObsIdx,_], '_', DataFile),
	format(ResFID, "~w,~w,~w,~w,", [SysID, AlgName, SysPrecents, ObsIdx]),
	format(ResFID, "~w,~w,~w,~w,~w\n", ["-", "-", "-", "timeout", "-"]),
	scan_raw_data(Files, DataFolder, SysID, SysPrecents, AlgName, ResFID).
