% System diagnostic preprocess
% Author: Amit Metodi
% Date: 16/12/2011

:- module(sysPreprocess, [preprocessSystemFile/2, sysPreprocess/2]).

:- use_module(sysPPReaderWriter).
:- use_module('aux/auxListsv2').
:- use_module('aux/auxLiteralsv2').
:- use_module('aux/auxSrtListsv2').
:- use_module('aux/auxListsNew').

%% ----- Preprocess System File -----

preprocessSystemFile(SysFile, PreTime):-!,
     concat_atom([SysFile,'pp'],PPSysFile),
     readSystem(SysFile,PureSystem,GatesModes),!,
     get_time(Time1),
     sysPreprocess(PureSystem,System),
     get_time(Time2),
     addModesToSystem(System,GatesModes,SystemWithModes),
     PreTime is Time2 - Time1,
     printSystemInformation(SystemWithModes),!,
     writef('Preprocess time: %w secs.\n',[PreTime]),flush,
     writePPSystem(PPSysFile,SystemWithModes,PreTime),!.

addModesToSystem(system(ID, Ins, Outs, Gates, Sections, Cones),GatesModes,system(ID, Ins, Outs, GatesWithModes, Sections, Cones)) :-
	addModesToGates(Gates, GatesModes, GatesWithModes).
	
addModesToGates([], _, []).
addModesToGates([(ID,Out,Ins,LogicType,GateType)|Gates], GatesModes, [(ID,Out,Ins,LogicType,GateType,GateModes)|GatesWithModes]) :-
	getGateModes(ID, GatesModes, GateModes),
	addModesToGates(Gates, GatesModes, GatesWithModes).
	
getGateModes(ID, [mode(ID,GateModes)|_], GateModes):- !.
getGateModes(ID, [_|GatesModes], GateModes) :- getGateModes(ID, GatesModes, GateModes).
     
printSystemInformation(system(ID, Ins, Outs, Gates, Sections, Cones)):-!,
     writef('System: %w\n',[ID]),
     length(Ins,InsLen),
     writef('|IN|=%w\n',[InsLen]),
     length(Outs,OutsLen),
     writef('|OUT|=%w\n',[OutsLen]),
     length(Gates,GatesLen),
     writef('|COMPS|=%w\n',[GatesLen]),
     length(Sections,SectionsLen),
     writef('|Sections|=%w\n',[SectionsLen]),
     length(Cones,ConesLen),
     writef('|Cones|=%w\n',[ConesLen]),
     countGatesType(Gates,dominated,0,DominatedLen),!,
     writef('|Dominated Gates|=%w\n',[DominatedLen]),
     sumSections(Sections,0,Sum),
     writef('|Section Sum|=%w\n',[Sum]).

countGatesType([(_ID,_Out,_Ins,_LogicType,GateType,_Modes)|Gates],GType,SoFar,Cnt):-!,
     (GateType==GType ->
         SoFar1 is SoFar + 1,
         countGatesType(Gates,GType,SoFar1,Cnt)
     ;
         countGatesType(Gates,GType,SoFar,Cnt)
     ).
countGatesType([],_,Cnt,Cnt):-!.

sumSections([],S,S):-!.
sumSections([(_,_,X)|Ss],C,S):-!,
      C1 is C + X,
      sumSections(Ss,C1,S).
      
      
%% ----- Preprocess System -----

% Var = (VarID, Out@Gate, In@Gate[])
% Gate = (GateID, Out:Var, Inputs:Var[], SectionID, Depth, GateType, LogicType)

% Section = (SectionID, GatesIDs, MaxUnHealthy)
% Cone = (DominatorID, DominatedIDs[])
% GateByDepth = (ID,Out,Ins,LogicType,GateType)
% GateType = { free, dominator, dominated }
% LogicType = { and, or, xor, nand, nor, buffer, inverter }

sysPreprocess(system(ID,Ins,Outs,Gates),system(ID, SIns, SOuts, GatesByDepth, Sections, Cones)):-!,
    sort(Ins,SIns), sort(Outs,SOuts),
    %% Create Map for Gates and Variables
    mapGates(Gates,MapGates,MapVars),!,
    %% Propagate depth gates
    assignGatesDepth(MapGates),!,
    %% get Gates sorted by depth
    getGatesByDepth(MapGates,GatesByDepth),!,
    % Create Sections
    out2initSection(SOuts,MapVars,Sections1),!,
    sort(Sections1,SSections1),
    splitIntersectSections(SSections1,Sections2),!,
    renumberSections(Sections2,1,SectionsWos),!,
    % link gates to sectionsIDs
    linkGates2Sections(SectionsWos,MapGates),!,
    % add number of outputs in section and get sections with outputs
    addSectionsOuts(SectionsWos, MapGates, Sections, SectionsWouts),!,
    % get cones
    findConesInSections(SectionsWouts,MapGates,MapVars, Cones),!,
    % mark dominator/dominated gates
    markDominateGates(Cones,MapGates),!,
    % mark free gates (not in cones)
    markFreeGates(GatesByDepth).



%%% Map vars to gates and Gates to vars

mapGates(Gates,Gates2IO,IO2Gates):-
   empty_assoc(EmptyGates2IO),
   getGatesVars(Gates,VGates,EmptyGates2IO,Gates2IO),

   empty_assoc(EmptyIO2Gates),
   gatesTOio2vgatesMap(VGates,EmptyIO2Gates,IO2Gates),

   updateVarGates(VGates,IO2Gates).


getGatesVars([(ID,Output,Inputs,Type)|Gates],[(ID,Output,Inputs,Type,Var)|VGates],CurGates2IO,Gates2IO):-!,
   put_assoc(ID,CurGates2IO,Var,NGates2IO),
   getGatesVars(Gates,VGates,NGates2IO,Gates2IO).
getGatesVars([],[],Gates2IO,Gates2IO):-!.

gatesTOio2vgatesMap([(_ID,Output,Inputs,_Type,GateVar)|Gates],CurMap,NewMap):-!,
    gatesTOio2vgatesMap_asOutput(Output,GateVar,CurMap,CurMap1),
    gatesTOio2vgatesMap_asInputs(Inputs,GateVar,CurMap1,CurMap2),
    gatesTOio2vgatesMap(Gates,CurMap2,NewMap).
gatesTOio2vgatesMap([],NewMap,NewMap):-!.

gatesTOio2vgatesMap_asOutput(Output,Gate,CurMap,NewMap):-!,
    (get_assoc(Output, CurMap, (Output,OutGate,_InGates)) ->
         !,OutGate=Gate,!,
         NewMap=CurMap
    ;
         put_assoc(Output, CurMap, (Output,Gate,[]),NewMap)
    ).

gatesTOio2vgatesMap_asInputs([Input|Inputs],Gate,CurMap,NewMap):-!,
    gatesTOio2vgatesMap_asInput(Input,Gate,CurMap,CurMap1),!,
    gatesTOio2vgatesMap_asInputs(Inputs,Gate,CurMap1,NewMap).
gatesTOio2vgatesMap_asInputs([],_,NewMap,NewMap):-!.

gatesTOio2vgatesMap_asInput(Input,Gate,CurMap,NewMap):-!,
    (get_assoc(Input, CurMap, (Input,OutGate,InGates)) ; InGates=[]),!,
    put_assoc(Input, CurMap, (Input,OutGate,[Gate|InGates]),NewMap).


updateVarGates([(ID,Output,Inputs,LogicType,Var)|VGates],IO2Gates):-!,
    get_assoc_list([Output|Inputs],IO2Gates,[VOutput|VInputs]),!,
    Var=(ID,VOutput,VInputs,_Section,_Depth,_GateType,LogicType),!,
    updateVarGates(VGates,IO2Gates).
updateVarGates([],_):-!.


%%% propagate gates depth
assignGatesDepth(MapGates):-
    assoc_to_values(MapGates,ListGates),
    propagateDepth(ListGates,[]).
    
    
propagateDepth([Gate|Gates],RestGates):-
    Gate=(_ID,_Output,Inputs,_SectionID,Depth,_Type),
    (getInputsMaxDepth(Inputs,0,MaxDepth) ->
        Depth is MaxDepth + 1,
        propagateDepth(Gates,RestGates)
    ;
        propagateDepth(Gates,[Gate|RestGates])
    ).
propagateDepth([],[]):-!.
propagateDepth([],Gates):-!,propagateDepth(Gates,[]).

getInputsMaxDepth([(_,OutGate,_)|Inputs],LocalMax,MaxDepth):-!,
    (var(OutGate) ->
        getInputsMaxDepth(Inputs,LocalMax,MaxDepth)
    ;
        OutGate=(_ID,_O,_In,_Sec,Depth,_Type),!,
        \+ var(Depth),
        NewMax is max(Depth,LocalMax),
        getInputsMaxDepth(Inputs,NewMax,MaxDepth)
    ).
getInputsMaxDepth([],MaxDepth,MaxDepth):-!.



%% get Gates by Depth
getGatesByDepth(MapGates,GatesByDepth):-!,
    gateByDepthList(MapGates,USGatesByDepth,[]),!,
    sort(USGatesByDepth,SGatesByDepth),!,
    removeGatesDepth(SGatesByDepth,GatesByDepth).

gateByDepthList(t(_Key,Val,_,L,R), List, Rest) :-!,
    Val=(ID,(Out,_),VInputs,_Section,Depth,GateType,LogicType),
    getVarsIDlist(VInputs,Ins),!,
    gateByDepthList(L, List, [(Depth,ID,Out,Ins,LogicType,GateType)|More]),!,
    gateByDepthList(R, More, Rest).
gateByDepthList(t, List, List).

getVarsIDlist([(ID,_)|Vars],[ID|IDs]):-!,
    getVarsIDlist(Vars,IDs).
getVarsIDlist([],[]):-!.

removeGatesDepth([(_,X)|Xs],[X|XXs]):-!,
    removeGatesDepth(Xs,XXs).
removeGatesDepth([],[]):-!.

%%% find health bits per output / init sections

out2initSection([O|Outs],IO2Gates,[(GateCnt,O,GatesIDs,[O])|Out2ISection]):-!,
    (get_assoc(O, IO2Gates, OutVar) ->
        getGatesIDsFromOutputs([OutVar],t,USGatesIDs-[],_),!,
        sort(USGatesIDs,GatesIDs),
        length(GatesIDs,GateCnt)
    ;
        GateCnt=0, GatesIDs=[]
    ),!,
    out2initSection(Outs,IO2Gates,Out2ISection).
out2initSection([],_,[]):-!.

getGatesIDsFromOutputs([(Var,OutGate,_InGates)|Vars],CurIncludedIDs,GatesIDsH-GatesIDsT,IncludedIDs):-!,
    ((var(OutGate) ; get_assoc(Var,CurIncludedIDs,_)) ->
         getGatesIDsFromOutputs(Vars,CurIncludedIDs,GatesIDsH-GatesIDsT,IncludedIDs)
    ;
         OutGate=(GateID,_OutVar,InVars,_),
         GatesIDsH=[GateID|GatesIDsM1],
         put_assoc(Var,CurIncludedIDs,x,IncludedIDs1),
         getGatesIDsFromOutputs(InVars,IncludedIDs1,GatesIDsM1-GatesIDsM2,IncludedIDs2),!,
         getGatesIDsFromOutputs(Vars,IncludedIDs2,GatesIDsM2-GatesIDsT,IncludedIDs)
    ).
getGatesIDsFromOutputs([],IncludedIDs,GatesIDs-GatesIDs,IncludedIDs):-!.


%% split sections so no common gates

splitIntersectSections([Section1|Sections],USections):-!,
    splitIntersectSections(Sections,Section1,MSections,USection1),!,
    (USection1 \== drop ->
        USections=[USection1|MUSections],
        splitIntersectSections(MSections,MUSections)
    ;
        splitIntersectSections(MSections,USections)
    ).
splitIntersectSections([],[]):-!.

splitIntersectSections(Sections,(0,_,[],_),Sections,drop):-!.
splitIntersectSections([(S2size,S2id,S2gates,S2Os)|MSections],(S1size,S1id,S1gates,S1Os),UpSections,FinalSection1):-!,
    srtlistIntersect(S1gates,S2gates,NS1gates,NS2gates,NCgates),
    (NCgates=[] -> % no common
          UpSections=[(S2size,S2id,S2gates,S2Os)|RUpSections],!,
          splitIntersectSections(MSections,(S1size,S1id,S1gates,S1Os),RUpSections,FinalSection1) ;
    (NS2gates=[] -> % Section 2 <= Section 1
          length(NS1gates,NS1size),
          atom_concat('u',S1id,NS1id),
          srtlistUnify(S1Os,S2Os,NS2Os),!,
          UpSections=[(S2size,S2id,S2gates,NS2Os)|RUpSections],
          splitIntersectSections(MSections,(NS1size,NS1id,NS1gates,S1Os),RUpSections,FinalSection1) ;
    (NS1gates=[] -> % Section 1 <= Section 2
          length(NS2gates,NS2size),
          atom_concat('u',S2id,NS2id),
          srtlistUnify(S1Os,S2Os,NS1Os),!,
          UpSections=[(NS2size,NS2id,NS2gates,S2Os)|RUpSections],
          splitIntersectSections(MSections,(S1size,S1id,S1gates,NS1Os),RUpSections,FinalSection1) ;
    %% Sum In Common
          length(NCgates,NCsize),
          atom_concat(S1id,S2id,NCid),
          atom_concat('u',S1id,NS1id),
          atom_concat('u',S2id,NS2id),
          NS1size is S1size - NCsize,
          NS2size is S2size - NCsize,
          srtlistUnify(S1Os,S2Os,NSCOs),!,
          UpSections=[(NCsize,NCid,NCgates,NSCOs),(NS2size,NS2id,NS2gates,S2Os)|RUpSections],
          splitIntersectSections(MSections,(NS1size,NS1id,NS1gates,S1Os),RUpSections,FinalSection1)
    ))).
splitIntersectSections([],Section1,[],Section1):-!.

%% renumber sections

renumberSections([(_Size,_OldId,GatesIds,Os)|Sections],I,[(NewId,GatesIds,RefOs)|NSections]):-!,
    atom_concat('section',I,NewId),
    I1 is I + 1,
    length(Os,RefOs),
    renumberSections(Sections,I1,NSections).
renumberSections([],_,[]):-!.


%% Link Gates to SectionsIDs
linkGates2Sections([(SectionID,GatesIds,_)|Sections],MapGates):-!,
    linkGates2SectionID(GatesIds,MapGates,SectionID),!,
    linkGates2Sections(Sections,MapGates).
linkGates2Sections([],_):-!.

linkGates2SectionID([ID|GatesIds],MapGates,SectionID):-!,
    get_assoc(ID,MapGates,(ID,_Out,_Ins,SectionID,_MoreInfo)),!,
    linkGates2SectionID(GatesIds,MapGates,SectionID).
linkGates2SectionID([],_,_):-!.


%% calcualte sections Inputs and outputs
addSectionsOuts([(SectionID, SectionGates, SectionOsRef)|SectionsWos],MapGates,[(SectionID, SectionGates, MaxUnHelathy)|Sections], [(SectionID, SectionGates, USOutsIDs)|OSections]):-!,
%addSectionsOuts([(SectionID, SectionGates, OutsCnt)|Sections],MapGates,[(SectionID, SectionGates, USOutsIDs)|OSections]):-!,
    sectionsOutputs(SectionGates,SectionID,MapGates,USOutsIDs),
    %sort(USOutsIDs,OutsIDs),
    length(USOutsIDs,OutsCnt),
    MaxUnHelathy is min(OutsCnt,SectionOsRef),
%    MaxUnHelathy is OutsCnt,
    addSectionsOuts(SectionsWos,MapGates,Sections,OSections).
addSectionsOuts([],_,[],[]):-!.

sectionsOutputs([GateID|SectionGates],SectionID,MapGates,OutsIDs):-!,
    get_assoc(GateID,MapGates,(_,(Var,_,InAtGates),_)),!,
    ((InAtGates=[_|_], allGateInSection(InAtGates,SectionID)) ->
        sectionsOutputs(SectionGates,SectionID,MapGates,OutsIDs)
    ;
        OutsIDs=[Var|MOutsIDs],
        sectionsOutputs(SectionGates,SectionID,MapGates,MOutsIDs)
    ).
sectionsOutputs([],_,_,[]):-!.

allGateInSection([Gate|InAtGates],SectionID):-!,
    Gate= (_, _, _, SectionID,_),!,
    allGateInSection(InAtGates,SectionID).
allGateInSection([],_):-!.


%%% Find Cones in sections
findConesInSections([(_SectionID, GatesIDs, Outs)|Sections],Gates2IO,IO2Gates, DominatorsH):-!,
    (Outs=[O] ->
        get_assoc(O,IO2Gates,(_,(OutGate,_),_)),
        srtlistIntersect(GatesIDs,[OutGate],Domed,[],[OutGate]),!,
        (Domed=[] ->
            DominatorsH=DominatorsM
        ;
            DominatorsH=[(OutGate,Domed)|DominatorsM]
        )
    ;
        addGates2DomsMap(GatesIDs,Gates2IO,t,DomMap),!,
        updateDomMap(GatesIDs,DomMap,NDomMap),!,
        reverseDomMap(GatesIDs,NDomMap,t,RDomMap),!,
        getDominatorsPairs(RDomMap,Dominators),!,
        list2diflist(Dominators,DominatorsH-DominatorsM)
    ),!,
    findConesInSections(Sections,Gates2IO,IO2Gates,DominatorsM).
findConesInSections([],_,_,[]):-!.


% Var = (VarID, Out@Gate, In@Gate[])
% Gate = (GateID, Out:Var, Inputs:Var[], SectionID)
% Section = (SectionID, GatesIDs, InsIDs, OutsIDs, Dominators)


addDom2Map((GateID,(_Out,_,OutAsInGates),_Ins,SectionID,_),CurDomMap,DomMap,DomBy):-!,
   (get_assoc(GateID,CurDomMap,DomBy) ->
       DomMap=CurDomMap
   ;
       (OutAsInGates=[] ->
            DomBy=[],
            put_assoc(GateID,CurDomMap,DomBy,DomMap)
       ;
       (OutAsInGates=[DomGate] ->
            DomGate=(GateID2,_,_,_,Depth,_),
            addDom2Map(DomGate,CurDomMap,CurDomMap2,DomBy2),
            sort([(Depth,GateID2)|DomBy2],DomBy),
            put_assoc(GateID,CurDomMap2,DomBy,DomMap)
       ;
            addDoms2Map(OutAsInGates,SectionID,CurDomMap,CurDomMap2,DomBy),
            put_assoc(GateID,CurDomMap2,DomBy,DomMap)
       ))
   ).


addDoms2Map([Gate|Gates],CurSectionID,CurDomMap,DomMap,DomBy):-!,
   Gate=(GateID,_Outs,_Ins,SectionID,Depth,_MoreInfo),!,
   (SectionID==CurSectionID ->
       addDom2Map(Gate,CurDomMap,DomMapT,DomBy1),!,
       (Gates=[] ->
           DomBy=DomBy1,
           DomMap=DomMapT
       ;
           addDoms2Map(Gates,CurSectionID,DomMapT,DomMap,DomBy2),
           sort([(Depth,GateID)|DomBy1],DomBy1b),
           srtlistIntersect(DomBy1b,DomBy2,DomBy)
       )
   ;
       DomBy=[],
       DomMap=CurDomMap
   ).


addGates2DomsMap([GateID|GatesIDs],GatesMap,CurDomMap,DomMap):-!,
   get_assoc(GateID,GatesMap,Gate),!,
   addDom2Map(Gate,CurDomMap,DomMapT,_),!,
   addGates2DomsMap(GatesIDs,GatesMap,DomMapT,DomMap).
addGates2DomsMap([],_,DomMap,DomMap):-!.



updateDomMap([GateID|GatesIDs],DomMap,NDomMap):-!,
   get_assoc(GateID,DomMap,ND),!,
   (ND=[] ->
       updateDomMap(GatesIDs,DomMap,NDomMap)
   ;
       append(_,[(_,DomBy)],ND),
       put_assoc(GateID,DomMap,[DomBy],DomMap2),!,
       updateDomMap(GatesIDs,DomMap2,NDomMap)
   ).
updateDomMap([],DomMap,DomMap):-!.





reverseDomMap([Gid|GatesIDs],DomMap,CurRDomMap,RDomMap):-!,
    (get_assoc(Gid,DomMap,[DomBy]) ->
         (get_assoc(DomBy,CurRDomMap,DomL) ->
             put_assoc(DomBy,CurRDomMap,[Gid|DomL],CurRDomMap2)
         ;
             put_assoc(DomBy,CurRDomMap,[Gid],CurRDomMap2)
         ),
         reverseDomMap(GatesIDs,DomMap,CurRDomMap2,RDomMap)
    ;
         reverseDomMap(GatesIDs,DomMap,CurRDomMap,RDomMap)
    ).
reverseDomMap([],_,RDomMap,RDomMap):-!.


getDominatorsPairs(RDomMap,Dominators):-!,
        assoc_to_pairlist(RDomMap,Dominators,[]).
        
assoc_to_pairlist(t(Key,Val,_,L,R), List, Rest) :-!,
        assoc_to_pairlist(L, List, [(Key,Val)|More]),!,
        assoc_to_pairlist(R, More, Rest).
assoc_to_pairlist(t, List, List).



%% mark dominator/dominated gates List
markDominateGates([(DominatorID,Domianteds)|Cones],GatesMap):-!,
    get_assoc(DominatorID,GatesMap,(_ID,_Out,_Ins,_SectionID,_Depth,dominator,_LogicType)),!,
    markDominatedGates(Domianteds,GatesMap),!,
    markDominateGates(Cones,GatesMap).
markDominateGates([],_):-!.

markDominatedGates([DominatedID|Domianteds],GatesMap):-!,
    get_assoc(DominatedID,GatesMap,(_ID,_Out,_Ins,_SectionID,_Depth,dominated,_LogicType)),!,
    markDominatedGates(Domianteds,GatesMap).
markDominatedGates([],_):-!.

%% mark free gates
markFreeGates([(_ID,_Out,_Ins,_LogicType,free)|Gates]):-!,
    markFreeGates(Gates).
markFreeGates([_|Gates]):-!,
    markFreeGates(Gates).
markFreeGates([]):-!.
