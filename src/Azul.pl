:- consult(player).
:- consult(game).
:- consult(utils).
:- consult(printing).
:- consult(puntuation).
:- dynamic(center/2).

%TODO:remove from bag and cover the tiles that I take
stractFromBag(L2):-
    findall((X,Y),
    bag(X,Y),P),
    realList(P,L),
    random_permutation(L,R),
    take(R,36,L2),
    takefromBag(L2).
stractFromCover(L2,Q):-
    findall((X,Y),
    cover(X,Y),P),
    realList(P,L),
    random_permutation(L,R),
    take(R,Q,L2),
    takefromCover(L2).

storageFactori([],_):-!.
storageFactori([(X,Y)|L],I):-
    fac(I,X,Z),
    retractall(fac(I,X,Z)),
    T=..([fac,I,X,Y]),
    assert(T),
    storageFactori(L,I).

%Recive the list of groups to put in the factories and storage it
storageFactories([],_) :- !.
storageFactories(_,0) :- !.
storageFactories([X|L],Y) :-
    storageFactori(X,Y),
    plus(Y,1,W),
    storageFactories(L,W).

%extract the tiles from the top of the bag.
fillFactories() :-
    stractFromBag(Lb),
    length(Lb,K),
    !,
    S is 36 - K,
    stractFromCover(Lc,S),
    append(Lb,Lc,L),
    repart(L,R),
    storageFactories(R,1),
    !.

% modify the order fallowed for the spetial tile.
whoStart():-
    specialTile(X),
    order(L),
    sortOrder(X,L,J),
    retractall(order(_)),
    T=..([order,J]),
    assert(T).

%indicates when there's not tiles to pick up.
stillTiles(0):-
    specialTile(X),
    X =\= 0,
    findall(C, fac(_, _, C), L1),
    allCero(L1),
    findall(C2, center(_, C2), L2),
    allCero(L2).
stillTiles(1).

%simulate the factori offert
takeFromFact(_):- stillTiles(X),X =:=0,!.
takeFromFact(X):-
    retractall(actual(_)),
    T=..[actual,X],
    assert(T),
    makeMove(X),
    next(N),takeFromFact(N),!.

factoriOffert():-
    whoStart(),
    order([X,Y,W,Z]),
    pT("\nOrder":X-Y-W-Z,cyan),!,
    printFactories(),
    printCenter(),
    retractall(specialTile(_)),
    T=..[specialTile,0],assert(T),
    takeFromFact(X),!.

tile(P,Ca,Co):-
    color(Color,Co),
    wall(Ca,X,Color),
    retractall(board(P,Ca,X,0)),
    assert(board(P,Ca,X,1)),
    updatePoints(P,Ca,X).

toTheWall([]):-!.
toTheWall([(A,B,C)|L]):-
    Ca is B-1,
    tile(A,Ca,C),
    retractall(stairPlayer(A,B,C,_)),
    T=..[stairPlayer,A,B,0,0],assert(T),
    toTheWall(L).

tiledToTheWall():-
    findall((P,Ca,Co),
    stairPlayer(P,Ca,Co,Ca),L),
    toTheWall(L).

prepareRound():-
    stractFromBag(Lb),
    length(Lb,K), !,
    S is 36 - K,
    stractFromCover(Lc,S),
    append(Lb,Lc,L),
    repart(L,R),
    storageFactories(R,1),!.

floorDamageCalculator(0):-!.
floorDamageCalculator(P):-
    findall((Clr,Z),(playerFloor(P,Clr,Z),Z > 0),L),
    pT("\n Al jugador"-P,red),
    gSecondList(L,L1),
    addList(L1,C),
    addSpecialTile(P,C,D),
    floordamage(D,Df),
    plus(Df,X,0), setpoint(P,X),
    pT(" se le restan":Df,red),
    P1 is P - 1,
    cleanFloor(P,L),
    floorDamageCalculator(P1).

startGame():-
    finishedGame(),
    retractall(specialTile(_)),
    assert(specialTile(0)),
    pT("\n ++++Ended Simulation++++ \n",cyan).
startGame():-
    pT("...Start Round...\n", cyan),
    prepareRound(),
    factoriOffert(),
    tiledToTheWall(),
    floorDamageCalculator(4),
    startGame(),!.

deleteAll():-
    retractall(color(_)),
    retractall(fac(_,_,_)),
    retractall(playerFloor(_,_,_)),
    retractall(center(_,_)),
    retractall(bag(_,_)),
    retractall(cover(_,_)),
    retractall(specialTile(_)),
    retractall(order(_)),
    retractall(wall(_,_,_,_)),
    retractall(board(_,_,_,_)),
    retractall(points(_,_)),
    retractall(stairPlayer(_,_,_,_)),
    retractall(actual(_)).
conclution():-
    finalPoints(4),
    winners(X),
    pT("\n And the winners are: \n",cyan),
    printPlayerList(X).


%the game is generated, the round start
azul():-
    pT("----Azul---- (4 players)\n",blue),
    generateGame(),
    generatePlayers(),
    startGame(),
    conclution(),
    deleteAll(),!.
















