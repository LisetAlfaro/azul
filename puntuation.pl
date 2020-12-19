canReachHorizontal(_,_,C,C):-!.
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

canReachVertical(_,R,-,R):-!.
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
           (member(Goal,[1,2,3,4,5]),
            Goal =\= C,
            canReachHorizontal(Player,R,C,Goal)),
            Nexts),
    length(Nexts,A).
verticalNext(Player,R,C,A):-
    findall(1,
           (member(Goal,[1,2,3,4,5]),
            Goal =\= R,
            canReachVertical(Player,R,C,Goal)),
            Nexts),
    length(Nexts,A).

setpoint(Player,C):-
    points(Player,C1),
    Cf is C1 + C,
    retractall(points(Player,C1)),
    T=..[points,Player,Cf],
    assert(T).

updatePoints(Player,R,C):-
    horizontalNext(Player,R,C,C1),
    verticalNext(Player,R,C,C2),
    (C1 > 0 -> Z is C1 + 1;
    Z = 0),
    (C2 > 0 -> Y is C2 + 1;
    Y = 0),
    A is Z+Y,
    (A > 0 -> plus(A,1,A1),setpoint(Player,A1);
    setpoint(Player,1)).
