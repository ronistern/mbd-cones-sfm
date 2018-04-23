%Author: Orel Elimelech

:- module('raw2results', [  analyzeRawData/1 ]).

/******************************/
/******* Raw to Results *******/
/******************************/

analyzeRawData(SysID) :-
	working_directory(CWD,CWD),
	concat_atom([CWD,'output/',SysID,'/'],Root),
	directory_files(Root,SystemsDirs1),
	keepDirsOnly(SystemsDirs1, SystemsDirs),
	concat_atom([CWD,'output/',SysID,'_results.txt'],ResFile),
	open(ResFile,write,ResFID),
	analyze_raw_systems(SystemsDirs,Root,ResFID),
	close(ResFID).
	
keepDirsOnly(InList, OutList) :-
	(member('.',InList) -> select('.',InList,InList1) ; InList = InList1),
	(member('..',InList1) -> select('..',InList1,OutList) ; InList1 = OutList).

analyze_raw_systems([],_,_).
analyze_raw_systems([SysName|Systems],Root,ResFID) :-
	analyze_raw_algs(SysName,Root,ResFID),
	analyze_raw_systems(Systems,Root,ResFID).

analyze_raw_algs(SysName,Root,ResFID) :-
	concat_atom([Root,SysName],SysDir),
	format(ResFID, "***************************************\n~w:\n***************************************\n", [SysDir]),
	directory_files(SysDir,SystemAlgorithmsDirs1),
	keepDirsOnly(SystemAlgorithmsDirs1,SystemAlgorithmsDirs),
	analyze_raw_data(SystemAlgorithmsDirs, SysDir,ResFID).
	
analyze_raw_data([],_,_).
analyze_raw_data([SysAlgorithmDir|Dirs],SysDir,ResFID) :-
	concat_atom([SysDir,'/',SysAlgorithmDir,'/'],DataFolder),
	directory_files(DataFolder,RawDataFiles1),
	keepDirsOnly(RawDataFiles1, RawDataFiles),
	getCounters(RawDataFiles,DataFolder,Counters,Agg),
	writeCountersToFile(ResFID,DataFolder,Counters,Agg),
	analyze_raw_data(Dirs,SysDir,ResFID).
	
writeCountersToFile(ResFID, DataFolder, Counters, Agg) :-
	Counters = counters(Total,TotalSucc,TotalFailed,TotalTimeout),
	Agg = agg(PPTimeCnt, InitConesCnt, ConesRemovedCnt, MCCnt, MCTimeCnt),
	(TotalSucc > 0 ->
		(AvgPPTime is PPTimeCnt / TotalSucc,
		AvgInitConesCnt is InitConesCnt / TotalSucc,
		AvgConesRemovedCnt is ConesRemovedCnt / TotalSucc,
		AvgMC is MCCnt / TotalSucc,
		AvgMCTime is MCTimeCnt / TotalSucc)
	;
		(AvgPPTime is 0,
		AvgInitConesCnt is 0,
		AvgConesRemovedCnt is 0,
		AvgMC is 0,
		AvgMCTime is 0)
	),
	format(ResFID, "-------------\n~w:\n", [DataFolder]),
	format(ResFID, "Total: ~w\nTotal succeeded: ~w\nTotal failed: ~w\nTotal timeout: ~w\n", [Total,TotalSucc,TotalFailed,TotalTimeout]),
	format(ResFID, "Average preprocess time: ~w\nAverage initial cones count: ~w\nAverage number of cones removed: ~w\n", [AvgPPTime,AvgInitConesCnt,AvgConesRemovedCnt]),
	format(ResFID, "Average MC: ~w\nAverage MC Time:~w\n", [AvgMC, AvgMCTime]).

%Counters = counters(Total,TotalSucc,TotalFailed,TotalTimeout).
%Agg = agg(PPTimeCnt, InitConesCnt, ConesRemovedCnt, MCCnt, MCTimeCnt).
getCounters([],_,Counters,Agg) :-
	Counters = counters(0,0,0,0),
	Agg = agg(0,0,0,0,0).
%FAILURE
getCounters([DataFile|Files],DataFolderDir,Counters,Agg) :-
	concat_atom([DataFolderDir,'/',DataFile],FullFilePath),
	open(FullFilePath,read,FID),
	read_term(FID, RawData, []),
	close(FID),
	RawData = (_SysFile,_ObsIdx,_Alg,_PPTime,_InitCones,_ConesRemoved,MC,_MCTime),MC = -1, !,
	getCounters(Files,DataFolderDir,Counters1,Agg1),
	Counters = counters(Total,TotalSucc,TotalFailed,TotalTimeout),
	Agg = agg(PPTimeCnt, InitConesCnt, ConesRemovedCnt, MCCnt, MCTimeCnt),
	Counters1 = counters(Total1,TotalSucc1,TotalFailed1,TotalTimeout1),
	Agg1 = agg(PPTimeCnt1, InitConesCnt1, ConesRemovedCnt1, MCCnt1, MCTimeCnt1),
	Total is Total1 + 1,
	TotalSucc is TotalSucc1 + 0,
	TotalFailed is TotalFailed1 + 1,
	TotalTimeout is TotalTimeout1 + 0,
	PPTimeCnt is PPTimeCnt1 + 0,
	InitConesCnt is InitConesCnt1 + 0,
	ConesRemovedCnt is ConesRemovedCnt1 + 0,
	MCCnt is MCCnt1 + 0,
	MCTimeCnt is MCTimeCnt1 + 0.
%SUCCESS
getCounters([DataFile|Files],DataFolderDir,Counters,Agg) :-
	concat_atom([DataFolderDir,DataFile],FullFilePath),
	open(FullFilePath,read,FID),
	read_term(FID, RawData, []),
	close(FID),
	RawData = (_SysFile,_ObsIdx,_Alg,PPTime,InitCones,ConesRemoved,MC,MCTime),!,
	getCounters(Files,DataFolderDir,Counters1,Agg1),
	Counters = counters(Total,TotalSucc,TotalFailed,TotalTimeout),
	Agg = agg(PPTimeCnt, InitConesCnt, ConesRemovedCnt, MCCnt, MCTimeCnt),
	Counters1 = counters(Total1,TotalSucc1,TotalFailed1,TotalTimeout1),
	Agg1 = agg(PPTimeCnt1, InitConesCnt1, ConesRemovedCnt1, MCCnt1, MCTimeCnt1),
	Total is Total1 + 1,
	TotalSucc is TotalSucc1 + 1,
	TotalFailed is TotalFailed1 + 0,
	TotalTimeout is TotalTimeout1 + 0,
	PPTimeCnt is PPTimeCnt1 + PPTime,
	InitConesCnt is InitConesCnt1 + InitCones,
	ConesRemovedCnt is ConesRemovedCnt1 + ConesRemoved,
	MCCnt is MCCnt1 + MC,
	MCTimeCnt is MCTimeCnt1 + MCTime.
%TIMEOUT
getCounters([DataFile|Files],DataFolderDir,Counters,Agg) :-
	concat_atom([DataFolderDir,DataFile],FullFilePath),
	open(FullFilePath,read,FID),
	close(FID),
	getCounters(Files,DataFolderDir,Counters1,Agg1),
	Counters = counters(Total,TotalSucc,TotalFailed,TotalTimeout),
	Agg = agg(PPTimeCnt, InitConesCnt, ConesRemovedCnt, MCCnt, MCTimeCnt),
	Counters1 = counters(Total1,TotalSucc1,TotalFailed1,TotalTimeout1),
	Agg1 = agg(PPTimeCnt1, InitConesCnt1, ConesRemovedCnt1, MCCnt1, MCTimeCnt1),
	Total is Total1 + 1,
	TotalSucc is TotalSucc1 + 0,
	TotalFailed is TotalFailed1 + 0,
	TotalTimeout is TotalTimeout1 + 1,
	PPTimeCnt is PPTimeCnt1 + 0,
	InitConesCnt is InitConesCnt1 + 0,
	ConesRemovedCnt is ConesRemovedCnt1 + 0,
	MCCnt is MCCnt1 + 0,
	MCTimeCnt is MCTimeCnt1 + 0.
