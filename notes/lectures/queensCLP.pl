% Nqueens problem solved in swi prolog using FD solver
% initial code from Triska's page

:- use_module(library(clpfd)).

solve(N, Vars) :-
    length(Vars, N),
    Vars ins 1..N,
    all_safe(Vars).


all_safe([]).
all_safe([A|L]) :-
    safeQueens(A,L,1),
    all_safe(L).

safeQueens(_,[],_).
safeQueens(A,[B|L],Dist) :-
    A #\= B,
    A - B #\= Dist,
    B - A #\= Dist,
    Dist1 #= Dist + 1,
    safeQueens(A, L, Dist1).

stats :-
    statistics(runtime,[_,X]),
    T is X/1000,
    nl,                           % write to screen
    write('run time: '),
    write(T), write(' sec.'), nl.

s1(N,Q) :- solve(N,Q), label(Q), stats.
s2(N,Q) :- solve(N,Q), labeling([ff],Q), stats.
s3(N,Q) :- solve(N,Q), labeling([ffc],Q), stats.
s4(N,Q) :- solve(N,Q), labeling([ffc,bisect],Q), stats.
s5(N,Q) :- solve(N,Q), labeling([ff,bisect],Q), stats.
s6(N,Q) :- solve(N,Q), labeling([ff,down],Q), stats.
