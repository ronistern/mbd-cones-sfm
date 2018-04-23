:- module('dataReader', [ loadSystem/3, loadObs/4 ]).

:- use_module(sysPreprocess).
:- use_module(sysPPReaderWriter).

%Loads the system. Preprocesses it if needed.
loadSystem(SysID, SysFile, System) :-
	getDataLoc(SysID, DataLoc),
	concat_atom([DataLoc, SysFile, '.syspp'], SysPP_Path),
	(exists_file(SysPP_Path) ->
		readPPSystem(SysPP_Path, System)
	;
		(
			concat_atom([DataLoc, SysFile, '.sys'], Sys_Path),
			(exists_file(Sys_Path) ->
				(
					writeln('-------------------------'),
					writeln('Preprocessing...'),
					preprocessSystemFile(Sys_Path, _PreTime),!,
					writeln('-------------------------'),
					readPPSystem(SysPP_Path,System)
				)
			;
				(writef('System file (%w) not found !\n',[Sys_Path]),!,fail)
			)
		)
	).

%Returns the folder of systems and observations instances
getDataLoc(SysID, DataLoc) :-
	working_directory(CWD, CWD),
	concat_atom([CWD, '/input/', SysID, '/'], DataLoc).

%Loads the observation.
loadObs(SysID, ObsID, ObsIndex, Obs) :-
	getDataLoc(SysID, DataLoc),
	concat_atom([DataLoc, SysID, '_', ObsID, '.obs'], ObsFile),
	open(ObsFile, read, OBStream),
	read_nth1_obs(ObsIndex, OBStream, Obs, []).

%Gets the nth observation from observations file
read_nth1_obs(ObsIndex, Stream, OBS, Opts) :-
	read_term(Stream,OBS1,Opts),
	(OBS1 = end_of_file -> (writef("Can't find observation %w",[ObsIndex]), fail) ; true),
	(OBS1=(_,ObsIndex,_) -> OBS=OBS1 ; read_nth1_obs(ObsIndex,Stream,OBS,Opts)).

