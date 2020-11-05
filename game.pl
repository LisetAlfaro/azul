
%This are the colors
color(r,1).
color(b,2).
color(y,3).
color(w,4).
color(g,5).

% this is a doble for who's generate X tiles of type P with Y colors,starting for X1 and Y1
fGenerator(X, _, X, _, _) :-!.
fGenerator(X, Y, X1, Y, P) :- !,Z is X1 + 1, fGenerator(X, Y, Z, 0,P).
fGenerator(X, Y, X1, Y1, P) :- N is Y1+1, color(C,N), I is X1+1, T =..[P,I,C,0], assert(T),fGenerator(X, Y, X1, N,P).

%this generate variables with Place(color,count) format
placeGenerator(_,Y,Y,_):-!.
placeGenerator(Place,Y,Y1,Count):- N is Y1+1,color(C,N), T =..[Place,C,Count], assert(T),placeGenerator(Place,Y,N,Count).

% generate the 4 tables according to
% pattern(player,row,collumn,Occupated) format
% recibes max players count, max rows count, max collumn count,
% ActualPlayer, ActualRow ,ActualCollumn,Occupated(1 or 0))
wallGenerator(PT, _, _, PT, _, _, _) :- !.
wallGenerator(PT, RT, CT, AP, RT, _, Count):- !,Z is AP +1, wallGenerator(PT, RT, CT, Z, 0, 0, Count).
wallGenerator(PT, RT, CT, AP , AR, CT, Count):- !, Z is AR +1, wallGenerator(PT, RT, CT, AP, Z, 0, Count).
wallGenerator(PT, RT, CT, AR, AP, AC, Count):- N is AC +1, color(C,N), R is AR +1, P is AP+1, T =..[wall,P,R,C,Count], assert(T),wallGenerator(PT, RT, CT, AR, AP, N, Count).

% factories will be filled fac(ind,color,count),
% playerfloor(player,color,count) generated the center, the top
% and the bag with this format:(color, count) generated the fields
% of the 4 players, the order, the espetial tile to mark the first
% player and the players points

generateGame():- fGenerator(9,5,0,0,fac), fGenerator(4,5,0,0,playerFloor), placeGenerator(cover,5,0,0), placeGenerator(center,5,0,0), placeGenerator(bag,5,0,20), wallGenerator(4,5,5,0,0,0,0), assert(order([1,2,3,4])), random(1,5,X), assert(specialTile(X)).

selectRandomColorformFactorie(F,Co):- findall((C,Y),fac(F,C,Y),L),length(L,K),plus(K,1,Ks),random(1,Ks,Co).

selectRandomColorformCenter(F,Co):- findall((C,Y),fac(F,C,Y),L),length(L,K),plus(K,1,Ks),random(1,Ks,Co).

makeMove(X):-random(1,11,R),
    R =:= 11 -> print('taking tiles from the center',yellow).
