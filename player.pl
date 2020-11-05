:-consult(utils).
:-consult(game).

%general field pattern cell(PositionX, positionY, color)
wall(0,0,b). wall(0,1,y). wall(0,2,r). wall(0,3,g). wall(0,4,w).
wall(1,0,w). wall(1,1,b). wall(1,2,y). wall(1,3,r). wall(1,4,g).
wall(2,0,g). wall(2,1,w). wall(2,2,b). wall(2,3,y). wall(2,4,r).
wall(3,0,r). wall(3,1,g). wall(3,2,w). wall(3,3,b). wall(3,4,y).
wall(4,0,y). wall(4,1,r). wall(4,2,g). wall(4,3,w). wall(4,4,b).

%This are the values to rest in the floor floor(tileCount,totalDamage)
floordamage(1,1). floordamage(2,2). floordamage(3,4). floordamage(4,6).
floordamage(5,8). floordamage(6,11). floordamage(7,14).

points(1,0).  points(2,0). points(3,0). points(4,0).

%triuph if the first player is X, initally is
first(X):- order(X,_,_,_).
actual(0).

winners(X) :- findall(Y,points(_,Y),L),greaters(L,M),findall(X,Z,points(Z,M),X).

completedRow(I):-findall(R,wall(I,R,_,1),L),compact(L,T),member((_,5),T),!.
selectRandomColor(F,L):-findall((C,Y),fac(F,C,Y),L).

makeMove(X):-random(1,11,R),
    R =:= 11 -> print(X,R);

.

next(Y):- actual(X),order(X,Y,_,_).
next(Y):- actual(X),order(_,X,Y,_).
next(Y):- actual(X),order(_,_,X,Y).
next(Y):- actual(X),order(Y,_,_,X).



