%S is the string, the posible colors are
%blue  red  green yellow black cyan  white  magenta
pT(S, Color):-
    ansi_format([bold, fg(Color)], S, []).

printList([]):-
    pT(" ",cyan),!.
printList([X|Xs]):-
    pT("1",X),
    printList(Xs).

printFactories_aux(10):-!.
printFactories_aux(X):-
    findall((C,Y),
    fac(X,C,Y),L),
    pT(" Factory-",cyan),
    pT(X,cyan),
    pT(": ",cyan),
    realList(L,L2),
    printList(L2),
    X1 is X + 1,
    printFactories_aux(X1).

printCenter():-
    findall((C,Y),
    center(C,Y),L),
    pT(" Center:",cyan),
    realList(L,L2),
    printList(L2).

printFactories():-
    pT("\n\n",black),
    printFactories_aux(1).

printPlayerList([]):-!.
printPlayerList([X|L]):-
    pT(" Player":X,cyan),
    printPlayerList(L).
