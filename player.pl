:-consult(utils).
:-consult(game).
:-dynamic(fplayer/5).

%general field pattern cell(PositionX, positionY, color)
wall(0,0,blue). wall(0,1,yellow). wall(0,2,red). wall(0,3,green). wall(0,4,magenta).
wall(1,0,magenta). wall(1,1,blue). wall(1,2,yellow). wall(1,3,red). wall(1,4,green).
wall(2,0,green). wall(2,1,magenta). wall(2,2,blue). wall(2,3,yellow). wall(2,4,red).
wall(3,0,red). wall(3,1,green). wall(3,2,magenta). wall(3,3,blue). wall(3,4,yellow).
wall(4,0,yellow). wall(4,1,red). wall(4,2,green). wall(4,3,magenta). wall(4,4,blue).

%This are the values to rest in the floor floor(tileCount,totalDamage)
floordamage(1,1). floordamage(2,2). floordamage(3,4). floordamage(4,6).
floordamage(5,8). floordamage(6,11). floordamage(7,14).

%boardGenerator(Player,row,collumn,ocupated)
boardGenerator(0,_,_):-!.
boardGenerator(P,-1,_):-
    P1 is P-1,
    boardGenerator(P1,4,4).
boardGenerator(P,R,-1):-
    R1 is R-1,
    boardGenerator(P,R1,4).
boardGenerator(P,R,C):-
    T =.. [board,P,R,C,0],
    assert(T),
    C1 is C-1,
    boardGenerator(P,R,C1).

% stairPlayer(player,capacity,color,ocupated)
stairsGenerator(0,_):-!.
stairsGenerator(X,0):-
    plus(Y,1,X),
    stairsGenerator(Y,5).
stairsGenerator(Player,Capacity):-
    T=..[stairPlayer,Player,Capacity,0,0],
    assert(T),
    plus(1,NewCap,Capacity),
    stairsGenerator(Player,NewCap).

generatePlayers:-
    boardGenerator(4,4,4),
    stairsGenerator(4,5),
    assert(points(1,0)),
    assert(points(2,0)),
    assert(points(3,0)),
    assert(points(4,0)),
    assert(actual(0)).

winners(X) :-
    findall(Y,points(_,Y),L),
    greaters(L,M),
    findall(X,Z,points(Z,M),X).

%see all the posibilities
posibleMove(L):-
    findall((X,Y,Z),
    (
        fac(X,Y,Z),
        (Z =\= 0)),
        L1),
    findall((0,M,N),
    (
        center(M,N),
        N=\=0),
        L2),
    append(L1,L2,L).

checkSpecialTile(_):- specialTile(X), X =\= 0.
checkSpecialTile(Player):-
    retractall(specialTile(_)),
    T=..[specialTile,Player],assert(T).

updateFloor(Player,Color,Count):-
    findall(Z,(playerFloor(Player,_,Z),Z > 0),L),
    addList(L,S),
    addSpecialTile(Player,S,D),
    plus(D,R,7),%how much I need to get to 7
    min(R,Count,A),%How much I will add
    playerFloor(Player,Color,X),%now many tiles of this color are now
    plus(X,A,C),
    retractall(playerFloor(Player,Color,_)),
    T=..[playerFloor,Player,Color,C],
    assert(T),
    Rest = Count - A,
    addToCover(Rest,Color),write("Finished add to floor"),!.

%gets from the center, ask about especial Tile
takeTiles(Player,0,Color,Count):-
    pT("\n Gets":Count,cyan),
    pT(Color,Color),
    pT(" from the Center",cyan),
    retractall(center(Color,Count)),
    T=..[center,Color,0],
    assert(T),
    checkSpecialTile(Player),
    !.
%gets from the factory all the tiles of one color
takeTiles(_,Ind,Color,Count):-
    pT("\n Gets":Count,cyan),
    pT(Color,Color),
    pT(" from the Factory"-Ind,cyan),
    retractall(fac(Ind,Color,Count)),
    cleanFactory(Ind).

updateStair(Player,Color,A,0):-
    color(Color,Number),
    retractall(stairPlayer(Player,A,_,_)),
    T=..[stairPlayer,Player,A,Number,A],
    assert(T).
updateStair(Player,Color,A,C):-
    C > 0,
    color(Color,Number),
    retractall(stairPlayer(Player,A,_,_)),
    plus(C,X,A),
    T=..[stairPlayer,Player,A,Number,X],
    assert(T).
updateStair(Player,Color,A,C):-
    C < 0,
    color(Color,Number),
    retractall(stairPlayer(Player,A,_,_)),
    T=..[stairPlayer,Player,A,Number,A],
    assert(T),
    myabs(C,R),
    updateFloor(Player,Color,R).

%if I complete a stair,then delete the old record and create a new one
choise(Player,Color,L):-
    pickCero(L,A),
    updateStair(Player,Color,A,0).

% if There is a rest or the row is uncomplete then I will re-record the row and add the rest of the tiles to the floor
choise(Player,Color,L):-
    greaterThird(L,(A,C)),
    updateStair(Player,Color,A,C),pT("And go to stair"-A,cyan).

onStairs(Player,Color,Count):-
    stairPlayer(Player,Count,_,0),
    C is Count-1,
    wall(C,Collumn,Color),
    board(Player,C,Collumn,0),
    color(Color,Number),
    T =.. [stairPlayer,Player,Count,Number,Count],
    retractall(stairPlayer(Player,Count,_,_)),
    assert(T),
    pT(" and put it in the stair ":Count,cyan).
onStairs(Player,Color,Count):-
    color(Color,Number),
    findall(
        (Ca,Co,Diference),
        (
            stairPlayer(Player,Ca, Number,Co),
            (Ca > Co),
            Diference is Ca-Co-Count,
            (Diference > -3)),
            L),
    choise(Player,Color,L).
onStairs(Player,Color,Count):-
    findall(
        (Ca,X),
        (stairPlayer(Player,Ca,_,0),
            plus(Count,X,Ca)),
        L),
    greaterSecond(L,(A,B)),
    updateStair(Player,Color,A,B),
     pT(" and going to the stair ":A,cyan).
onStairs(Player,Color,Count):-
    color(Color,Number),
    findall(
        (Ca,Co,Diference),
        (stairPlayer(Player,Ca, Number,Co),
            (Ca > Co),
            Diference is Ca-Co-Count,
            (Diference < -2)),
        L),
    choise(Player,Color,L).
onStairs(Player,Color,Count):-
    updateFloor(Player,Color,Count),
     pT(" and put it in the floor ",cyan).

selectRandom([_],0):-!.
selectRandom(L,R):-
    length(L,Le),
    random(1,Le,R).

makeMove(Player):-
    pT("\n Player"-Player,cyan),
    posibleMove(L),
    selectRandom(L,R),
    takeIndex(R,L,(A,B,C)),
    takeTiles(Player,A,B,C),
    onStairs(Player,B,C),
    !.

next(Y):-
    actual(X),
    order([X,Y,_,_]).
next(Y):-
    actual(X),
    order([_,X,Y,_]).
next(Y):-
    actual(X),
    order([_,_,X,Y]).
next(Y):-
    actual(X),
    order([Y,_,_,X]).










