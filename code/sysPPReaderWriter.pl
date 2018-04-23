% System Reader/Writer
% Author: Amit Metodi
% Date: 13/12/2011

:- module(sysPPReaderWriter, [ readSystem/3, readPPSystem/2, writePPSystem/2, writePPSystem/3 ]).

readSystem(FileName, system(ID,Ins,Outs,Gates), GatesModes):-!,
     open(FileName, read, FID),!,
     read_term(FID, ID, []),
     read_term(FID, Ins, []),
     read_term(FID, Outs, []),
     read_term(FID, Gates1, []),
     close(FID),
     fixGates(Gates1,Gates,GatesModes).

fixGates([[Type,ID,Modes,Output|Inputs]|Gates],[(ID,Output,Inputs,FixType)|FixGates],[mode(ID,Modes)|RestModes]):-!,
     ((fixGateType(Type,FixType,1,InsCnt),length(Inputs,InsCnt)) ->
         fixGates(Gates,FixGates,RestModes)
     ;
         throw(bad_gate([Type,ID,Output|Inputs]))
     ).
fixGates([],[],[]):-!.

fixGateType(buffer,buffer,1,1):-!.
fixGateType(inverter,inverter,1,1):-!.
fixGateType(Type,NType,1,Ins):-!,
    splitType(Type,NType,Cnt),!,
    atom_to_term(Cnt,Ins,_).

splitType(Type,and,Cnt):-
    atom_concat('and',Cnt,Type),!.
splitType(Type,or,Cnt):-
    atom_concat('or',Cnt,Type),!.
splitType(Type,xor,Cnt):-
    atom_concat('xor',Cnt,Type),!.
splitType(Type,NType,Cnt):-
    atom_concat('n',Rest,Type),!,
    splitType(Rest,NType1,Cnt),!,
    atom_concat('n',NType1,NType).


readPPSystem(FileName, system(ID, Ins, Outs, Gates, Sections, Cones)):-!,
     open(FileName, read, FID),!,
     read_term(FID, ID, []),
     read_term(FID, Ins, []),
     read_term(FID, Outs, []),
     read_term(FID, Gates, []),
     read_term(FID, Sections, []),
     read_term(FID, Cones, []),
     close(FID).

writePPSystem(FileName, system(ID, Ins, Outs, Gates, Sections, Cones),Time):-!,
     open(FileName, write, FID),!,
     write(FID,'% Preprocess time: '),
     write(FID,Time),
     write(FID,' ms.\n'),
     write(FID,'% System ID\n'),
     write(FID,ID), write(FID,'.\n'),
     write(FID,'% System Inputs\n'),
     writeRowsList(Ins,FID,20),
     write(FID,'% System Outputs\n'),
     writeRowsList(Outs,FID,20),
     write(FID,'% System Gates\n'),
     writeNewLineList(Gates,FID),
     write(FID,'% System Sections\n'),
     writeNewLineList(Sections,FID),
     write(FID,'% System Cones\n'),
     writeNewLineList(Cones,FID),
     close(FID).

writePPSystem(FileName, System):-!,
     writePPSystem(FileName, System, -).


writeRowsList([X|List],FID,ItemsPerRow):-
     write(FID,'[\n'),
     writeItem(X,FID),
     writeRowsList(List,1,ItemsPerRow,FID).
writeRowsList([],_,_,FID):-!,
     write(FID,'\n].\n').
writeRowsList(List,ItemsPerRow,ItemsPerRow,FID):-!,
     write(FID,',\n'),
     writeRowsList(List,0,ItemsPerRow,FID).
writeRowsList([X|List],Indx,ItemsPerRow,FID):-!,
     (Indx==0 ; write(FID,',')),
     writeItem(X,FID),
     Indx1 is Indx + 1,
     writeRowsList(List,Indx1,ItemsPerRow,FID).


writeNewLineList([X|List],FID):-!,
     write(FID,'[\n'),
     writeItem(X,FID),
     writeNewLineList_(List,FID).
writeNewLineList_([X|List],FID):-!,
     write(FID,',\n'),
     writeItem(X,FID),
     writeNewLineList_(List,FID).
writeNewLineList_([],FID):-!,write(FID,'\n].\n').


writeItem([],FID):-!,
    write(FID,'[]').
writeItem([X|Xs],FID):-!,
    write(FID,'['),
    writeItem(X,FID),
    writeList(Xs,FID).
writeItem((X,Xs),FID):-!,
    write(FID,'('),
    writeItem(X,FID),
    writeTuple(Xs,FID).
writeItem(X,FID):-!,
    write(FID,X).
writeList([X|Xs],FID):-!,
    write(FID,','),
    writeItem(X,FID),
    writeList(Xs,FID).
writeList([],FID):-!,
    write(FID,']').
writeTuple((X,Xs),FID):-!,
    write(FID,','),
    writeItem(X,FID),
    writeTuple(Xs,FID).
writeTuple((X),FID):-!,
    write(FID,','),
    writeItem(X,FID),
    write(FID,')').

% readSystem('D:/SAT/SATCompiler/Problems/SysDiag4/Data/c7552.sys',PureSystem), sysPreprocess(PureSystem,System),writePPSystem('D:/c7552.syspp',System).