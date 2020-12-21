canReachHorizontal(P,R,C,C):- board(P,R,C,1),!.
canReachHorizontal(Player,R,C,Goal):-
    C < Goal,%moverme a la derecha
    board(Player,R,C,1),%esta activa?
    C1 is C + 1,
    canReachHorizontal(Player,R,C1,Goal).
canReachHorizontal(Player,R,C,Goal):-
    C > Goal,
    board(Player,R,C,1),%esta activa?
    C1 is C - 1,
    canReachHorizontal(Player,R,C1,Goal).

canReachVertical(P,R,C,R):- board(P,R,C,1),!.
canReachVertical(Player,R,C,Goal):-
    R < Goal,%moverme up
    board(Player,R,C,1),%it is active?
    R1 is R + 1,
    canReachVertical(Player,R1,C,Goal).
canReachVertical(Player,R,C,Goal):-
    R > Goal,%move down
    board(Player,R,C,1),%it is active?
    R1 is R - 1,
    canReachVertical(Player,R1,C,Goal).

horizontalNext(Player,R,C,A):-
    findall(1,
           (member(Goal,[0,1,2,3,4]),
            Goal =\= C,
            canReachHorizontal(Player,R,C,Goal)),
            Nexts),
    length(Nexts,A).
verticalNext(Player,R,C,A):-
    findall(1,
           (member(Goal,[0,1,2,3,4]),
            Goal =\= R,
            canReachVertical(Player,R,C,Goal)),
            Nexts),
    length(Nexts,A).

setpoint(Player,C):-
    C > 0,
    points(Player,C1),
    Cf is C1 + C,
    retractall(points(Player,C1)),
    T=..[points,Player,Cf],
    assert(T).
setpoint(Player,C):-
    points(Player,C1),
    myabs(C,X),
    Cf is C1 - X,
    retractall(points(Player,C1)),
    T=..[points,Player,Cf],
    assert(T).


updatePoints(Player,R,C):-
    horizontalNext(Player,R,C,C1),
    verticalNext(Player,R,C,C2),
    (C1 > 0 -> Z is C1 + 1;
    Z is 0),
    (C2 > 0 -> Y is C2 + 1;
    Y is 0),
    A is Z + Y,
    (A > 0 -> setpoint(Player,A);
    setpoint(Player,1)).

finalPoints(0):-!.
finalPoints(Player):-
    points(Player,I),
    findall(1,(checkCompleteRows(Player,C1),
               C1 > 0),L1),
    length(L1,K1),
    findall(1,(checkCompleteCollumns(Player,C2),
               C2 > 0),L2),
    length(L2,K2),
    findall(1,(checkCompleteColors(Player,C3),
               C3 > 0),L3),
    length(L3,K3),
    Rpoints is K1 * 2,
    Cpoints is K2 * 7,
    Clrpoints is K3 * 10,
    Fpoints is I + Rpoints + Cpoints + Clrpoints,
    retractall(points(Player,_)),
    T=..[points,Player,Fpoints],
    assert(T),
    P1 is Player - 1,
    finalPoints(P1),!.












