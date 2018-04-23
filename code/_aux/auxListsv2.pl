% Functions on Lists
% Author: Amit Metodi
% Last Updated: 23/10/2011

/**/
:- module(auxListsv2, [ list2diflist/2, diflistAppendBack/3,
                      listDropFrom/3, listKeepFrom/3,
                      listSplit/4, listOddEvenSplit/3,
                      listListOf/3,
                      list2assoc/2,
                      listPrint/1,
                      get_assoc_list/3
                      ]).


% -------------------------------------
% | List to Different List            |
% -------------------------------------
list2diflist([X|Xs],[X|LH]-LT):-list2diflist(Xs,LH-LT).
list2diflist([],L-L).

% -------------------------------------
% | Different List append to back     |
% -------------------------------------
diflistAppendBack(Item,[]-[],[Item|LT]-LT).
diflistAppendBack(Item,LH-[Item|LT],LH-LT).

% -------------------------------------
% | List dorp/keep/split              |
% -------------------------------------
listDropFrom(0,List,List).
listDropFrom(1,[_|List],List).
listDropFrom(2,[_,_|List],List).
listDropFrom(3,[_,_,_|List],List).
listDropFrom(4,[_,_,_,_|List],List).
listDropFrom(5,[_,_,_,_,_|List],List).
listDropFrom(I,[_,_,_,_,_,_|List],SList):-
    I1 is I - 6,!,
    listDropFrom(I1,List,SList).

listKeepFrom(0,_,[]).
listKeepFrom(1,[X|_],[X]).
listKeepFrom(2,[X1,X2|_],[X1,X2]).
listKeepFrom(3,[X1,X2,X3|_],[X1,X2,X3]).
listKeepFrom(4,[X1,X2,X3,X4|_],[X1,X2,X3,X4]).
listKeepFrom(5,[X1,X2,X3,X4,X5|_],[X1,X2,X3,X4,X5]).
listKeepFrom(I,[X1,X2,X3,X4,X5,X6|Xs],[X1,X2,X3,X4,X5,X6|KXs]):-
    I6 is I - 6,!,
    listKeepFrom(I6,Xs,KXs).

listSplit(0,L,[],L).
listSplit(1,[X|L],[X],L).
listSplit(2,[X1,X2|L],[X1,X2],L).
listSplit(3,[X1,X2,X3|L],[X1,X2,X3],L).
listSplit(4,[X1,X2,X3,X4|L],[X1,X2,X3,X4],L).
listSplit(5,[X1,X2,X3,X4,X5|L],[X1,X2,X3,X4,X5],L).
listSplit(I,[X1,X2,X3,X4,X5,X6|RL],[X1,X2,X3,X4,X5,X6|NL],L):-
    I6 is I - 6,!,
    listSplit(I6,RL,NL,L).
    
    
% -------------------------------
% | Split Odd Even              |
% -------------------------------
% split L=[a0,a1,a2,a3...] to two lists L1=[a0, a2...] and L1=[a1, a3...]
listOddEvenSplit([A,B|Xs],[A|As],[B|Bs]):-
       listOddEvenSplit(Xs,As,Bs).
listOddEvenSplit([A],[A],[]).
listOddEvenSplit([],[],[]).

% -------------------------------
% | list to Assoc               |
% -------------------------------
list2assoc(List,Assoc):-!,
   list2assoc(List,t,Assoc).
list2assoc([X|Xs],CurAssoc,Assoc):-!,
   put_assoc(X,CurAssoc,X,CurAssoc2),!,
   list2assoc(Xs,CurAssoc2,Assoc).
list2assoc([],Assoc,Assoc):-!.



% -------------------------------
% | Create List of 'X's         |
% -------------------------------
listListOf(0,_,[]).
listListOf(1,X,[X]).
listListOf(2,X,[X,X]).
listListOf(3,X,[X,X,X]).
listListOf(4,X,[X,X,X,X]).
listListOf(I,X,[X,X,X,X,X|Xs]):-
    I1 is I - 5,
    listListOf(I1,X,Xs).



listPrint([X|Xs]):-writeln(X),!,listPrint(Xs).
listPrint([]):-!.


get_assoc_list([], _, []):-!.
get_assoc_list([ID|IDs], VarMap, [Val|Vals]):-!,
        get_assoc(ID, VarMap, Val),!,
        get_assoc_list(IDs,VarMap,Vals).