%altern two lists, element by element
altern([],L,L).
altern(L,[],L).
altern([X],[Y],[Y,X]).
altern([X|L],[Y|M],N):-
    altern(L,M,O),
    append([Y,X],O,N).

% delete the element in the X position and left it in Y, the rest of the
% list is in L.
dIndex(X,[Y|L],X,Y,L).
dIndex(X,[F|G],Xa,Y,L):-
    S is Xa+1,
    dIndex(X,G,S,Y,R),
    append([F],R,L).

%return the element in the list
takeIndex(0,[R|_],R):-!.
takeIndex(X,[_|L],Z):-
    X1 is X-1,
    takeIndex(X1,L,Z).

%hace una lista de Y elementos todos X.
genList(_,0,[]):-!.
genList(X,Y,[X|N]):-
    plus(Y1,1,Y),
    genList(X,Y1,N).

% gived P[(X,Y)] create a list T where there are repliacted an Y times
% each X number.
realList([],[]).
realList([(X,Y)|P],T):-
    genList(X,Y,M),
    realList(P,G),
    append(M,G,T).

% Triuph if the third argument is the result of take the X first
% elements of the first argument.
take(_,0,[]).
take([],_,[]).
take([Y|L1],X,[Y|L2]):-
    plus(Z,1,X),
    take(L1,Z,L2),
    !.

%compact a list in another list which elements are tuples with
%(element,count)format.
compact([],[]).
compact([Y|L],[(Y,C)|R]):-
    delete(L,Y,H),
    length([Y|L],Ki),
    length(H,Kf),
    plus(Kf,C,Ki),
    compact(H,R).

% get a list and form sublists with 4 elements each one, the sublists
% are compacted.
repart([],[]).
repart(L,[C|J]):-
    take(L,4,H),
    append(H,X,L),
    compact(H,C),
    repart(X,J),
    !.

%stablish the order there is not working as predicade.
sortOrder(_,[],[]).
sortOrder(X,[X|Y],[X|Y]).
sortOrder(X,[Y|L],Z):-
    append(L,[Y],R),
    sortOrder(X,R,Z).

%gets the bigger element of a list.
greaters([X],X).
greaters([X|Xs],X):-
    greaters(Xs,Y),
    X > Y,
    !.
greaters([X|Xs],Y):-
    greaters(Xs,Y),
    Y >= X,
    !.
% return the first element of the tuple that contains a 0 as the third
% element in a list
pickCero([(Ca,_,0)|_],Ca):- !.
pickCero([_|L],R):- pickCero(L,R).

%return the tuple with the grater second element
greaterSecond([X],X).
greaterSecond([(X,Y)|Xs],(X,Y)):-
    greaterSecond(Xs,(_,W)),
    Y > W,
    !.
greaterSecond([(_,Y)|Xs],(V,W)):-
    greaterSecond(Xs,(V,W)),
    W >= Y,
    !.
%return the tuple with the grater third element
greaterThird([(A,_,C)],(A,C)):-!.
greaterThird([(Ca,_,D)|L],(Ca,D)):-
    greaterThird(L,(_,C)),
    D > C,
    !.
greaterThird([(_,_,D)|L],(Ca,C)):-
    greaterThird(L,(Ca,C)),
    C >= D,
    !.

myabs(X,Y):-
    X < 0 -> Y is -X;
    Y = X.

%stractSecondItem([],[]).
%stractSecondItem([(_,Y)|Ls],[Y|L]):- stractSecondItem(Ls,L).

%stractFirstItem([],[]).
%stractFirstItem([(X,_)|Ls],[X|L]):- stractFirstItem(Ls,L).

%gFirstList(Y,[(X,Y)|_],X).
%gFirstList(Y,[(_,_)|L],X):- gFirstList(Y,L,X).

gSecondList([],[]):-!.
gSecondList([(_,Z)|L],[Z|Y]):- gSecondList(L,Y).

allCero([0]):-!.
allCero([0|P]):-
    allCero(P),
    !.

addList([],0):-!.
addList([X|L],C):-
    addList(L,C1),
    C is C1 + X.
min(A,B,C):-
    A > B -> C is B;
    C is A.














