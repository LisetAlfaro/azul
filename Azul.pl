:- consult(player).
:- consult(game).
:- consult(utils).

stractFromBag(L2):- findall((X,Y),bag(X,Y),P),realList(P,L),random_permutation(L,R),take(R,36,L2).
stractFromCover(L2,Q):- findall((X,Y),cover(X,Y),P),realList(P,L),random_permutation(L,R),take(R,Q,L2).

storageFactori([],_):-!.
storageFactori([(X,Y)|L],I):- fac(I,X,Z),retractall(fac(I,X,Z)),T=..([fac,I,X,Y]),assert(T),storageFactori(L,I).

%Recive the list of groups to put in the factories and storage it
storageFactories([],_) :- !.
storageFactories(_,0) :- !.
storageFactories([X|L],Y) :-storageFactori(X,Y),plus(Y,1,W),storageFactories(L,W).

%storage functions with three arguments
%storage([],_,_).
%storage([(X,Y)|L],F,I):- T =..([F,I,X,Y]), assert(T),storage(L,F,I),!.

%extract the tiles from the top of the bag
fillFactories() :- stractFromBag(Lb),length(Lb,K),!, S is 36 - K, stractFromCover(Lc,S), append(Lb,Lc,L),repart(L,R),storageFactories(R,1),!.

% modify the order fallowed for the spetial tile
whoStart():- specialTile(X), order(L), sortOrder(X,L,J),
    retractall(order(_)),T=..([order,J]), assert(T).

stillTiles(X):- findall(Z,fac(_,_,Z),P),findall(Y,center(_,Y =:= 0),J).

takeFromFact(X):stillTiles(0).
takeFromFact(X):-makeMove(X),next(N),stillTiles(),!,takeFromFact(N).

factoriOfert():-fillFactories(),order(X,_,_,_),takeFromFact(X).

prepareRound().

tiledToTheWall().

startRound():-factoriOfert(),tiledToTheWall(),prepareRound().

%the game is generated, the round start
azul():- generateGame(), whoStart(),startRound(),!.








