:- module('dataWriter', [ sysInfoToFile/3, mcToFile/8, getOutFile/4, getFinalConesFile/4, getBeeResultsFile/4]).

getFinalConesFile(SysFile, AlgName, ObsNum, OutFile) :-
	atom_string(SysFile,SysFileStr),
	split_string(SysFileStr,"_","",[SysID|_]),
	concat_atom(['output/',SysID,'/',SysFile,'/',AlgName,'/'],FileDir),
	concat_atom([FileDir,SysFile,'_',ObsNum,'_',AlgName,'_bee.txt'],OutFile),
	open(OutFile,write,FID),
	close(FID).
	
getBeeResultsFile(SysFile, AlgName, ObsNum, OutFile) :-
	atom_string(SysFile,SysFileStr),
	split_string(SysFileStr,"_","",[SysID|_]),
	concat_atom(['output/',SysID,'/',SysFile,'/',AlgName,'/'],FileDir),
	concat_atom([FileDir,SysFile,'_',ObsNum,'_',AlgName,'_finalcones.txt'],OutFile),
	open(OutFile,write,FID),
	close(FID).

sysInfoToFile(AlgName, SysFile, ObsNum) :-
	getOutFile(SysFile, AlgName, ObsNum, OutFile),
	open(OutFile,write,FID),
	close(FID).
	
mcToFile(AlgName, SysFile, ObsNum, PPTime, ConesCnt, ConesRemoved, MC, MCTime) :-
	getOutFile(SysFile, AlgName, ObsNum, OutFile),
	open(OutFile,append,FID),
	format(FID, "('~w',~w,~w,~w,~w,~w,~w,~w).\n", [SysFile, ObsNum, AlgName, PPTime, ConesCnt, ConesRemoved, MC, MCTime]),
	close(FID).
	
getOutFile(SysFile, AlgName, ObsNum, OutFile) :-
	atom_string(SysFile,SysFileStr),
	split_string(SysFileStr,"_","",[SysID|_]),
	concat_atom(['output/',SysID,'/',SysFile,'/',AlgName,'/'],FileDir),
	concat_atom([FileDir,SysFile,'_',ObsNum,'_',AlgName,'_results.txt'],OutFile).
	
%(SysFile,ObsNum,AlgorithmName,PreprocessTime,InitCones,ConesRemoved,MC,MCTime).
%('c880_0',1,nc,0,0,0,1,3.2116036415100098).
