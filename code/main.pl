:- module('main', 	[ 	
			
			solveObs_no_cones/3, 
			solveObs_pessimistic/3, 
			solveObs_weak_optimistic/3,
			solveObs_strong_optimistic/3,
			solveObs_smart_weak_optimistic/3,
			solveObs_smart_strong_optimistic/3,
			
			createNewOutputFolder/1,
			createScriptFile/3,
			raw2results/1,
			raw2csv/1
			
			]).

%user:file_search_path(bee, '/home/orel/Desktop/Research/apps_dev/bee20140212_edited').
%user:file_search_path(bee, '/home/orel/Desktop/Research/apps_dev/bee20140212').
user:file_search_path(bee,'/home/orel/Desktop/Downloads/Bee-Compiler').
%user:file_search_path(bee,'/home/sternron/orel/Dropbox/Orel-Research-SFM/apps_dev/BEE/Bee-Compiler').

:- use_module(mainAux).
:- use_module(dataReader).
:- use_module(scriptBuilder).
:- use_module(raw2results).
:- use_module(raw2csv).

:- use_module(sysDiag_no_cones).
:- use_module(sysDiag_pessimistic).
:- use_module(sysDiag_weak_optimistic).
:- use_module(sysDiag_strong_optimistic).
:- use_module(sysDiag_smart_weak_optimistic).
:- use_module(sysDiag_smart_strong_optimistic).

:- writeln('USAGE: solveObs_no_cones(SysFile, ObsID, ObsIndex).').
:- writeln('       solveObs_pessimistic(SysFile, ObsID, ObsIndex).').
:- writeln('       solveObs_weak_optimistic(SysFile, ObsID, ObsIndex).').
:- writeln('       solveObs_strong_optimistic(SysFile, ObsID, ObsIndex).').
:- writeln('       createScriptFile(SysID, ObsID, Timeout).').


/*
The brute-force approach doesn't use cones at all.
*/
solveObs_no_cones(SysFile, ObsID, ObsIndex) :-
	getSysID(SysFile, SysID),
	loadSystem(SysID, SysFile, System),
	loadObs(SysID, ObsID, ObsIndex, Obs), %writeln(System),nl,writeln(Obs),
	diagnostics_no_cones(SysFile, System, Obs).

/*
In the pessimistic approach for finding MC diagnoses,
we use a cone abstraction that contains only cones that can be grounded by assuming a single fault.
As an instance of the pessimistic approach, we considered only cones whose dominator has the “flip” behavior mode.
*/
solveObs_pessimistic(SysFile, ObsID, ObsIndex) :-
	getSysID(SysFile, SysID),
	loadSystem(SysID, SysFile, System), %writeln(System),
	loadObs(SysID, ObsID, ObsIndex, Obs), 
	diagnostics_pessimistic(SysFile, System, Obs).
/*
The weak optimistic approach starts by finding MC abstract diagnosis.
Then, every faulty cone is tested, to see if it can be grounded by assuming that only one of its constituent components is faulty.
If all faulty cones can be grounded using only a single faulty component per cone, then a grounded MC diagnoses is found.
Otherwise, the search for abstract MC diagnoses is restarted on the original cone abstraction,
except of that faulty cone which is broken to its constituent components.
This is repeated until either all cones are broken or a groundable abstract MC diagnosis is found.
*/
solveObs_weak_optimistic(SysFile, ObsID, ObsIndex) :-
	getSysID(SysFile, SysID),
	loadSystem(SysID, SysFile, System),
	loadObs(SysID, ObsID, ObsIndex, Obs),
	diagnostics_weak_optimistic(SysFile, System, Obs).
/*
The strong optimistic approach is slightly more elaborated.
If an abstract MC diagnosis cannot be grounded using a single faulty component,
then the search continues for other abstract MC diagnoses (without breaking any cone).
This continues until either a groundable MC diagnoses is found with a single fault per component,
or until all abstract MC diagnoses are considered.
Then, a single cone is selected and broken, and the process restarts with the updated cone abstraction.
*/
solveObs_strong_optimistic(SysFile, ObsID, ObsIndex) :-
	getSysID(SysFile, SysID),
	loadSystem(SysID, SysFile, System),
	loadObs(SysID, ObsID, ObsIndex, Obs),
	diagnostics_strong_optimistic(SysFile, System, Obs).

solveObs_smart_weak_optimistic(SysFile, ObsID, ObsIndex) :-
	getSysID(SysFile, SysID),
	loadSystem(SysID, SysFile, System),
	loadObs(SysID, ObsID, ObsIndex, Obs),
	diagnostics_smart_weak_optimistic(SysFile, System, Obs).	
	
solveObs_smart_strong_optimistic(SysFile, ObsID, ObsIndex) :-
	getSysID(SysFile, SysID),
	loadSystem(SysID, SysFile, System),
	loadObs(SysID, ObsID, ObsIndex, Obs),
	diagnostics_smart_strong_optimistic(SysFile, System, Obs).	

/*
Creates output folder.
*/
createNewOutputFolder(SysID) :- createOutputFolder(SysID).	
/*
Generates the experiments script.
*/
createScriptFile(SysID,ObsID,Timeout) :- generateScript(SysID, ObsID, Timeout).

/*
Analyzes the raw data and creates a results file.
*/
raw2results(SysID) :- analyzeRawData(SysID).

/*
Collects all raw data to one csv file.
*/
raw2csv(SysID) :- collectRawData(SysID).





