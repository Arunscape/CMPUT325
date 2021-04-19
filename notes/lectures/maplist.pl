% helper predicates used in sudoku-with-own-pred.pl

%-----------------------------------------------------------
% Given a number N, generate an NxN grid (a list of sublists 
% representing rows)

grid(N,Result) :- grid0(N,N,Result).
   % first N is the number of rows for recursive calls; second N 
   % is the length of each row

grid0(0,_,[]) :- !.
grid0(NN, N, [Row|Rest]) :-
    length(Row, N),
    NN1 is NN-1,     
    grid0(NN1, N, Rest).

% test
g(K) :- grid(4,K).

/*
 xtranspose(+G,-T_G): 

Given an NxN grid G (a list of rows), generate a transpose 
grid T_G, i.e, it is a list of N sublists representing columns of G - the front elements of sublists in G give the first column of T_G, and so on.

*/

xtranspose([], []) :- !.
xtranspose(G,[FirstCol|RestCol]) :-
    getCol(G, FirstCol, RestG),
         % FirstCol is bound to the first column of G
         % RestG is G with the front element of each row removed
    xtranspose(RestG,RestCol).

getCol(G, R, []) :-
    singleSub(G), !,
       % each sublist in G is singleton
    append(G, R).
       % the pre-defined append can also appends lists in a list

getCol([[A|R]|G], [A|R1], [R|RestG]) :-
       % 1st element of 1st row is A, which is the 1st element 
       % of 1st column; the resulting G is the one without 
       % the 1st elements 
    getCol(G,R1,RestG).

singleSub([]).
singleSub([[_]|R]) :- singleSub(R).

% tests
t1(R) :- xtranspose([[a,b],[c,d]], R).
t2(R) :- xtranspose([[a,b,c],[7,8,9],[1,2,3]], R).

%-----------------------------------------------------
% xall-distinct(L): L is a list of sublists each of which 
% contains distinct values

xall-distinct([]):- !.
xall-distinct([A|L]) :- all_distinct(A), xall-distinct(L).
