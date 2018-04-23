% Author: Amit Metodi
% Date: 22/10/2011

/**/
:- module(auxSrtListsv2, [
                      srtlistIntersect/5,
                      srtlistIntersect/3,
                      srtlistUnify/3
                      ]).
/*
srtlistIsMember(X,[Y|Ys]):-!,
    (X==Y,! ; X @> Y,!, srtlistIsMember(X,Ys)).

srtlistInsert([Y|Ys],X,NYs):-!,
    (Y @> X ->
        NYs=[X,Y|Ys]
    ;
        NYs=[Y|MYs],
        srtlistInsert(Ys,X,MYs)
    ).
srtlistInsert([],X,[X]):-!.
*/

%% srtlistIntersect(X,Y,C)
%% C = X/\Y
srtlistIntersect([X|Xs],[X|Ys],[X|Cs]):-!,
    srtlistIntersect(Xs,Ys,Cs).
srtlistIntersect([X|Xs],[Y|Ys],Cs):-!,
    (X @> Y ->
        srtlistIntersect([X|Xs],Ys,Cs) ;
%    (X @< Y ->
        srtlistIntersect(Xs,[Y|Ys],Cs)
    ).
srtlistIntersect([],_,[]):-!.
srtlistIntersect(_,[],[]):-!.


%% srtlistIntersect(X,Y,NX,NY,C)
%% NX = X\Y , NY = Y\X , C = X/\Y
srtlistIntersect([X|Xs],[X|Ys],NXs,NYs,[X|Cs]):-!,
    srtlistIntersect(Xs,Ys,NXs,NYs,Cs).
srtlistIntersect([X|Xs],[Y|Ys],NXs,NYs,Cs):-!,
    (X @> Y ->
        NYs=[Y|MYs],
        srtlistIntersect([X|Xs],Ys,NXs,MYs,Cs) ;
%    (X @< Y ->
        NXs=[X|MXs],
        srtlistIntersect(Xs,[Y|Ys],MXs,NYs,Cs)
    ).
srtlistIntersect([],Ys,[],Ys,[]):-!.
srtlistIntersect(Xs,[],Xs,[],[]):-!.

srtlistUnify([X|Xs],[X|Ys],[X|Zs]):-!,
    srtlistUnify(Xs,Ys,Zs).
srtlistUnify([X|Xs],[Y|Ys],[X|Zs]):-
    X @< Y,!,
    srtlistUnify(Xs,[Y|Ys],Zs).
srtlistUnify([X|Xs],[Y|Ys],[Y|Zs]):-
    X @> Y,!,
    srtlistUnify([X|Xs],Ys,Zs).
srtlistUnify([],Ys,Ys):-!.
srtlistUnify(Xs,[],Xs):-!.


/*
srtlistMinus([X|Xs],[X|Ys],Zs):-!,
   srtlistMinus(Xs,Ys,Zs).
srtlistMinus([X|Xs],[Y|Ys],[X|Zs]):-
   X @< Y ,!,
   srtlistMinus(Xs,[Y|Ys],Zs).
srtlistMinus([X|Xs],[Y|Ys],Zs):-
   X @> Y ,!,
   srtlistMinus([X|Xs],Ys,Zs).
srtlistMinus([],_,[]):-!.
srtlistMinus(Xs,[],Xs):-!.
*/