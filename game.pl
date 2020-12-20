
%This are the colors
color(red,1).
color(blue,2).
color(yellow,3).
color(magenta,4).
color(green,5).
color(cyan,6).

% this is a doble for who's generate X tiles of type P with Y colors,starting for X1 and Y1
fGenerator(X, _, X, _, _) :-!.
fGenerator(X, Y, X1, Y, P) :- !,
    Z is X1 + 1,
    fGenerator(X, Y, Z, 0,P).
fGenerator(X, Y, X1, Y1, P) :-
    N is Y1+1,
    color(C,N),
    I is X1+1,
    T =..[P,I,C,0],
    assert(T),
    fGenerator(X, Y, X1, N,P).

%this generate variables with Place(color,count) format
placeGenerator(_,Y,Y,_):-!.
placeGenerator(Place,Y,Y1,Count):-
    N is Y1+1,
    color(C,N),
    T =..[Place,C,Count],
    assert(T),
    placeGenerator(Place,Y,N,Count).

%take from the bag the list of objects
takefromBag([]):-!.
takefromBag([X|L]):-
    bag(X,Y),
    Y2 is Y-1,
    retractall(bag(X,_)),
    assert(bag(X,Y2)),
    takefromBag(L).

%take from the cover the list of objects
takefromCover([]):-!.
takefromCover([X|L]):-
    cover(X,Y),
    Y2 is Y-1,
    retractall(cover(X,_)),
    assert(cover(X,Y2)),
    takefromCover(L).
addToCover(Count,Color):-
    Count > 0,
    cover(Color,C),
    Total is Count + C,
    T=..[cover,Color,Total],
    retractall(cover(Color,_)),
    assert(T).
addToCover(_,_):-!.

moveToCenter([]).
moveToCenter([(X,Y)|L]):-
    center(X,Z),
    plus(Z,Y,S),
    retractall(center(X,_)),
    assert(center(X,S)),
    moveToCenter(L).

removeOfFactory(_,[]):-!.
removeOfFactory(Id,[(X,_)|L]):-
    retractall(fac(Id,X,_)),
    T=..[fac,Id,X,0] ,
    assert(T),
    removeOfFactory(Id,L).

cleanFactory(Id):-
    findall((X,Y),
    (fac(Id,X,Y),
    Y =\= 0),
    P),
    moveToCenter(P),
    removeOfFactory(Id,P).
% factories will be filled fac(ind,color,count),
% playerfloor(player,color,count) generated the center, the top
% and the bag with this format:(color, count) generated the fields
% of the 4 players, the order, the espetial tile to mark the first
% player and the players points

generateGame():-
    fGenerator(9,5,0,0,fac),
    fGenerator(4,5,0,0,playerFloor),
    placeGenerator(cover,5,0,0),
    placeGenerator(center,5,0,0),
    placeGenerator(bag,5,0,20),
    assert(order([1,2,3,4])),
    random(1,5,X),
    assert(specialTile(X)).

addSpecialTile(X,D1,D):-
    D1 < 7,
    specialTile(X),
    D2 is D1 + 1,
    D is -1 * D2.
addSpecialTile(_,D1,D):-
    D is -1 * D1.

finishedGame():-
    findall(1,(member(P,[1,2,3,4]),
    checkCompleteRows(P,C),C > 0),L),
    length(L,K),K > 0.
finishedGame():-
    findall(1,(bag(_,C1),C1 =\= 0),L1),
    length(L1,K1),K1 =:= 0,
    findall(1,(cover(_,C2),C2 =\= 0),L2),
    length(L2,K2),K2 =:= 0.



















