%This are the colors
color(r,1).
color(b,2).
color(y,3).
color(w,4).
color(g,5).

%general field pattern cell(PositionX, positionY, colorId)
cell(0,0,b). cell(0,1,y). cell(0,2,r). cell(0,3,g). cell(0,4,w).
cell(1,0,w). cell(1,1,b). cell(1,2,y). cell(1,3,r). cell(1,4,g).
cell(2,0,g). cell(2,1,w). cell(2,2,b). cell(2,3,y). cell(2,4,r).
cell(3,0,r). cell(3,1,g). cell(3,2,w). cell(3,3,b). cell(3,4,y).
cell(4,0,y). cell(4,1,r). cell(4,2,g). cell(4,3,w). cell(4,4,b).

%This are the values to rest in the floor floor(tileCount,totalDamage)
floor(1,1). floor(2,2). floor(3,4). floor(4,6).
floor(5,8). floor(6,11). floor(7,14).

% this is a doble for who's generate X tiles of type P with Y colors,starting for X1 and Y1
genFac(X, _, X, _, _) :-!.
genFac(X, Y, X1, Y, P) :- !,Z is X1 + 1, genFac(X, Y, Z, 0,P).
genFac(X, Y, X1, Y1, P) :- N is Y1+1, color(C,N), I is X1+1, T =..[P,I,C,0], assert(T),
    genFac(X, Y, X1, N,P).

%this generate variables with Place(color,count) format
genPlace(_,Y,Y,_):-!.
genPlace(Place,Y,Y1,Count):- N is Y1+1,color(C,N), T =..[Place,C,Count], assert(T),genPlace(Place,Y,N,Count).

% generate the 4 tables according to
% pattern(player,row,collumn,Occupated) format
% recibes max players count, max rows count, max collumn count,
% ActualPlayer, ActualRow ,ActualCollumn,Occupated(1 or 0))
genPattern(PT, _, _, PT, _, _, _) :- !.
genPattern(PT, RT, CT, AP, RT, _, Count):- !,Z is AP +1, genPattern(PT, RT, CT, Z, 0, 0, Count).
genPattern(PT, RT, CT, AP , AR, CT, Count):- !, Z is AR +1, genPattern(PT, RT, CT, AP, Z, 0, Count).
genPattern(PT, RT, CT, AR, AP, AC, Count):- N is AC +1, color(C,N), R is AR +1, P is AP+1,
    T =..[pattern,P,R,C,Count], assert(T),
    genPattern(PT, RT, CT, AR, AP, N, Count).

% factories will be filled fac(ind,color,count),
% playerfloor(player,color,count) generated the center, the top
% and the bag with this format:(ind, color, count) generated the fields
% of the 4 players, the order, the espetial tile to mark the first
% player, la celda ocupada que no se que es!!! the players points

generateGame():- genFac(9,5,0,0,fac),genFac(4,5,0,0,playerFloor),
    genPlace(cover,5,0,0),genPlace(center,5,0,0),
    genPlace(bag,5,0,20), genPattern(4,5,5,0,0,0,0),
    assert(order([1,2,3,4])),random(1,5,X), assert(specialTile(X)),assert(ocupatedCell(0,-1,-1)),
    assert(pointsP(1,0)),assert(pointsP(2,0)),assert(pointsP(3,0)),assert(pointsP(4,0)).

%altern two lists, element by element
%altern([],L,L).
%altern(L,[],L).
%altern([X],[Y],[Y,X]).
%altern([X|L],[Y|M],N):-altern(L,M,O), append([Y,X],O,N).

% delete the element in the X position and left it in Y, the rest of the
% list is in L
%dIndex(X,[Y|L],X,Y,L).
%dIndex(X,[F|G],Xa,Y,L):- S is Xa+1, dIndex(X,G,S,Y,R),append([F],R,L).

%hace una lista de Y elementos todos X
genList(_,0,[]):-!.
genList(X,Y,[X|N]):- plus(Y1,1,Y),genList(X,Y1,N).

% gived P[(X,Y)] create a list T where there are repliacted an Y times
% each X number.
realList([],[]).
realList([(X,Y)|P],T):- genList(X,Y,M),realList(P,G),append(M,G,T).

stractFromBag(L2):- findall((X,Y),bag(X,Y),P),realList(P,L),random_permutation(L,R),take(R,36,L2).
stractFromCover(L2,Q):- findall((X,Y),cover(X,Y),P),realList(P,L),random_permutation(L,R),take(R,Q,L2).

% Triuph if the third argument is the result of take the X first
% elements of the first argument
take(_,0,[]).
take([],_,[]).
take([Y|L1],X,[Y|L2]):- plus(Z,1,X),take(L1,Z,L2),!.

repart([],[]).
repart(L,[H|J]):- take(L,4,H),append(H,X,L),repart(X,J),!.

%storage functions with three arguments
%storage([],_,_).
%storage([(X,Y)|L],F,I):- T =..([F,I,X,Y]), assert(T),storage(L,F,I),!.

%extract the tiles from the top of the bag
fillFactories() :- stractFromBag(Lb),length(Lb,K),!, S is 36 - K, stractFromCover(Lc,S), append(Lb,Lc,L),repart(L,R),write(R),!.

%stablish the order there is not working as predicade.
sortOrder(_,[],[]).
sortOrder(X,[X|Y],[X|Y]).
sortOrder(X,[Y|L],Z):- append(L,[Y],R), sortOrder(X,R,Z).

% modify the order fallowed for the spetial tile
whoStart():- specialTile(X), order(L), sortOrder(X,L,J),
    retractall(order(_)),T=..([order,J]), assert(T).

%C is the color
% O =..([F,I,_,R]),findall(R,O,[P|_]),D =..([F,I,X,P]),retract(D)\

prepareRound():- whoStart(),fillFactories().

tile():-!.

startRound():-prepareRound().

%the game is generated, the round start
azul():- generateGame(),startRound(),!.
















