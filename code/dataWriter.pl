:- module('dataWriter', [ sysInfoToFile/4, mcToFile/7, getOutFile/4, getFinalConesFile/4, getBeeResultsFile/4 ]).



sysInfoToFile(AlgName, SysFile, ObsNum, ConesCnt) :-
	getOutFile(SysFile, AlgName, ObsNum, OutFile),
	open(OutFile,write,FID),
	format(FID, "('~w',~w,~w,~w,", [SysFile, ObsNum, AlgName, ConesCnt]),
	close(FID).
	
mcToFile(AlgName, SysFile, ObsNum, PPTime, ConesRemoved, MC, MCTime) :-
	getOutFile(SysFile, AlgName, ObsNum, OutFile),
	open(OutFile,append,FID),
	format(FID, "~w,~w,~w,~w).\n", [ConesRemoved, PPTime, MC, MCTime]),
	close(FID).
	
getOutFile(SysFile, AlgName, ObsNum, OutFile) :-
	atom_string(SysFile,SysFileStr),
	split_string(SysFileStr,"_","",[SysID|_]),
	concat_atom(['output/',SysID,'/',SysFile,'/',AlgName,'/'],FileDir),
	concat_atom([FileDir,SysFile,'_',ObsNum,'_',AlgName,'_results.txt'],OutFile).

getBeeResultsFile(SysFile, AlgName, ObsNum, OutFile) :-
	atom_string(SysFile,SysFileStr),
	split_string(SysFileStr,"_","",[SysID|_]),
	concat_atom(['output/',SysID,'/',SysFile,'/',AlgName,'/'],FileDir),
	concat_atom([FileDir,SysFile,'_',ObsNum,'_',AlgName,'_bee.txt'],OutFile),
	open(OutFile,write,FID),
	close(FID).
	
getFinalConesFile(SysFile, AlgName, ObsNum, OutFile) :-
	atom_string(SysFile,SysFileStr),
	split_string(SysFileStr,"_","",[SysID|_]),
	concat_atom(['output/',SysID,'/',SysFile,'/',AlgName,'/'],FileDir),
	concat_atom([FileDir,SysFile,'_',ObsNum,'_',AlgName,'_finalcones.txt'],OutFile),
	open(OutFile,write,FID),
	close(FID).

	
%(SysFile,ObsNum,AlgorithmName,PreprocessTime,InitCones,ConesRemoved,MC,MCTime).
%('c880_0',1,nc,0,0,0,1,3.2116036415100098).
