:- module(auxLiteralsv2, [ litUnify/2, litPureLit_/3, 
			    litAsgnTrue/1, litAsgnFalse/1,
			    litAsgnTrues/1, litAsgnFalses/1,
			    litCountTrues/2, litCountTrues/3,
			    litCountFalses/2, litCountFalses/3,
			    litIsEqual/2
			  ]).

litUnify(A,B):-
    (ground(A) ->
        (ground(B) -> !, A=:=B ;
        litPureLit_(B,Bl,Bop),
        (Bop == '+' -> Bl is A ; Bl is -A)) ;
    (ground(B) ->
        litPureLit_(A,Al,Aop),
        (Aop == '+' -> Al is B ; Al is -B) ;
    litPureLit_(A,Al,Aop),
    litPureLit_(B,Bl,Bop),!,
    (Aop==Bop ->
         Al=Bl
    ;
         (Al @> Bl -> Al= -Bl;
         (Al @< Bl -> Bl= -Al;
         !,fail))
    ))).
    
    
litPureLit_(X,X,+):-var(X),!.
litPureLit_(-(X),X,-):-var(X),!.
litPureLit_(-(-(X)),Xl,Xop):-!,litPureLit_(X,Xl,Xop).


litAsgnTrue(X):-X is 1,!.
litAsgnTrue(-(X)):-!,litAsgnFalse(X).

litAsgnFalse(X):-X is -1, !.
litAsgnFalse(-X):-!,litAsgnTrue(X).

litAsgnTrues([X|Xs]):-
    litAsgnTrue(X),!,
    litAsgnTrues(Xs).
litAsgnTrues([]).

litAsgnFalses([X|Xs]):-!,
    litAsgnFalse(X),!,
    litAsgnFalses(Xs).
litAsgnFalses([]).


litCountTrues(Xs,Cnt):-!,
    litCountTrues(Xs,0,Cnt).

litCountTrues([X|Xs],I,Cnt):-!,
    ((ground(X),  X =:= 1) ->
        I1 is I + 1,
        litCountTrues(Xs,I1,Cnt)
    ;
        litCountTrues(Xs,I,Cnt)
    ).
litCountTrues([],Cnt,Cnt):-!.


litCountFalses(Xs,Cnt):-!,
    litCountFalses(Xs,0,Cnt).
    
litCountFalses([X|Xs],I,Cnt):-!,
    ((ground(X), X=:= -1) ->
        I1 is I + 1,
        litCountFalses(Xs,I1,Cnt)
    ;
        litCountFalses(Xs,I,Cnt)
    ).
litCountFalses([],Cnt,Cnt):-!.



litIsEqual(A,B) :-
    % when A is True/False
    (ground(A) ->
         !,ground(B),!, A=:=B ;
    % when B is True/False (A isn't ground)
    (ground(B) ->
         !,fail ;
    % when A and B literals
    litPureLit_(A,Al,Aop),
    litPureLit_(B,Bl,Bop),!,
    Al==Bl, Aop==Bop
    )).