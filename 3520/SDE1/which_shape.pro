/*  SDE1
    CPSC 3520
    Spring 2019
*/


/* Check for contiguous substring of u and return length */
uA(0, [], []).
uA(L, [AH|AT], []) :- AH == "u", uA(L2, AT, []), L is L2+1.


/* Check for contiguous substring of r and return length */
rA(0, [], []).
rA(L, [AH|AT], []) :- AH == "r", rA(L2, AT, []), L is L2+1.


/* Check for contiguous substring of d and return length */
dA(0, [], []).
dA(L, [AH|AT], []) :- AH == "d", dA(L2, AT, []), L is L2+1.


/* Check for contiguous substring of l and return length */
lA(0, [], []).
lA(L, [AH|AT], []) :- AH == "l", lA(L2, AT, []), L is L2+1.


/* determines if list has u, r, d, and l */
sq([], []).
sq([AH|AT], []) :-
  member("u", [AH|AT]), member("r", [AH|AT]),
  member("d", [AH|AT]), member("l", [AH|AT]).


/* Counts num of variable */
count([],_,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X, count(T,X,Z).


/* Compare */
compareS([], []).
compareS([H|T], [A|B]) :-
  H == A, compareS(T, B).


/* Check is Square */
countSq(List, U, R) :-
  count(List, "u", U), count(List, "r", R), count(List, "d", D),
  count(List, "l", L), U == R, U == D, U == L, R == D, R == L, L == D.

sqA([], []).
sqA([AH|AT], []) :-
  member("u", [AH|AT]), member("r", [AH|AT]),
  member("d", [AH|AT]), member("l", [AH|AT]),
  countSq([AH|AT], X, Y), grect(X, Y, R), !, compareS(R, [AH|AT]).


/* Checks if rectangle */
countRct(List, U, R) :-
  count(List, "u", U), count(List, "r", R), count(List, "d", D),
  count(List, "l", L), U == D, R == L.

rctA([], []).
rctA([AH|AT], []) :-
  member("u", [AH|AT]), member("r", [AH|AT]),
  member("d", [AH|AT]), member("l", [AH|AT]),
  countRct([AH|AT], X, Y), grect(X, Y, R), !, compareS(R, [AH|AT]).


/* Generates a string list for rectangle */
addU(1, ["u"]).
addU(X, ["u"|UT]) :- X1 is X-1, addU(X1, UT).

addR(1, ["r"]).
addR(X, ["r"|UT]) :- X1 is X-1, addR(X1, UT).

addD(1, ["d"]).
addD(X, ["d"|UT]) :- X1 is X-1, addD(X1, UT).

addL(1, ["l"]).
addL(X, ["l"|UT]) :- X1 is X-1, addL(X1, UT).

grect(A, B, C) :-
  addU(A, C1), addR(B, C2), addD(A, C3), addL(B, C4),
  append(C1, C2, C5), append(C3, C4, C6), append(C5, C6, C).


/* Generates a string list for rectangle */
addM(1, ["m30"]).
addM(X, ["m30"|UT]) :- X1 is X-1, addM(X1, UT).

addP(1, ["p240"]).
addP(X, ["p240"|UT]) :- X1 is X-1, addP(X1, UT).

gtri(A, R) :-
  addU(A, C1), addM(A, C2), addP(A, C3),
  append(C1, C2, C5), append(C3, [], C6), append(C5, C6, R).


/* Check for contiguous substring of m30 and return length */
m30A(0, [], []).
m30A(L, [AH|AT], []) :- AH == "m30", m30A(L2, AT, []), L is L2+1.


/* Check for contiguous substring of p240 and return length */
p240A(0, [], []).
p240A(L, [AH|AT], []) :- AH == "p240", p240A(L2, AT, []), L is L2+1.


/* Checks if Triangle */
countTri(List, U) :-
  count(List, "u", U), count(List, "m30", M), count(List, "p240", P),
  U == M, U == P, M == P.

eqtriA([], []).
eqtriA([AH|AT], []) :-
  AH == "u",
  member("u", [AH|AT]), member("m30", [AH|AT]),
  member("p240", [AH|AT]),
  countTri([AH|AT], X), gtri(X, R), compareS(R, [AH|AT]).


/* Shifts list by one and assigns to W */
head(AH, H) :- H = AH.
del(H, [H|T], T).
one_shift([AH|AT], W) :-
  head(AH, H), del(H, [AH|AT], T), append(T, [H], W).


/* Assigns all cases except input to Y */
again(_, [], L, L) :- !.
again(H, [H|T], L, S) :-
  one_shift(H, Y), S1 is S+1, again(Y, T, L, S1).

all_shifts(A, R, L, S) :-
  one_shift(A, X), again(X, R, L, S).


/* Similar to all_shifts except calculates length */
start_shifts(L, AS) :-
  length(L, X),
  all_shifts(L, AS, X, 1).


/* Assigns all cases including the input to R */
all_cases(A, R) :-
  start_shifts(A, B), append([A], B, R).


/* Checks if Square */
goThroughS(Cases, R) :- tsqA(Cases, [], R).
trySq([], _).
trySq([AH|AT], R) :-
  goThroughS(AH, R), trySq(AT, R).

tsqA([], [], _).
tsqA([AH|AT], [], R) :-
  member("u", [AH|AT]), member("r", [AH|AT]),
  member("d", [AH|AT]), member("l", [AH|AT]),
  countSq([AH|AT], X, Y), grect(X, Y, R).

try_all_sqA([AH|AT]) :-
  trySq([AH|AT], R),
  nl, write('cyclic shift:'), format(" ~q", [R]),
  write(' is a square'), nl.


/* Checks if Rectangle */
goThroughR(Cases, R) :- trctA(Cases, [], R).
tryRect([], _).
tryRect([AH|AT], R) :-
  goThroughR(AH, R), tryRect(AT, R).

trctA([], [], _).
trctA([AH|AT], [], R) :-
  member("u", [AH|AT]), member("r", [AH|AT]),
  member("d", [AH|AT]), member("l", [AH|AT]),
  countRct([AH|AT], X, Y), grect(X, Y, R).

try_all_rctA([AH|AT]) :-
  tryRect([AH|AT], R),
  nl, write('cyclic shift:'), format(" ~q", [R]),
  write(' is a rectangle'), nl, nl.


/* Checks if equilateral triangle */
goThroughT(Cases, R) :- teqtriA(Cases, [], R).
tryTri([], _).
tryTri([AH|AT], R) :-
  goThroughT(AH, R), tryTri(AT, R).

teqtriA([], [], _).
teqtriA([AH|AT], [], R) :-
  member("u", [AH|AT]), member("m30", [AH|AT]),
  member("p240", [AH|AT]),
  countTri([AH|AT], U), gtri(U, R).

try_all_eqtriA([AH|AT]) :-
  tryTri([AH|AT], R),
  nl, write('cyclic shift:'), format(" ~q", [R]),
  write(' is an equilateral triangle'), nl, nl.
