% Question 1

% credit: from example in eclass
xmember(A,[A|_]).
xmember(A,[B|L]) :- 
  A \== B, xmember(A,L).

%xmember(_, []) :- false.
%xmember(V, [V | _]) :- true. % if V is the head, then it exists
%xmember(V, [_ | R]) :-
%  xmember(V, R).

% , is and ; is or

setIntersect([], _, []). % if first list is empty return empty list
setIntersect([First | Rest], S2, [First | Output]) :-
  xmember(First, S2), setIntersect(Rest, S2, Output). % if First element of S1 is in S2 then add it to the output list, and check the rest of S1 against S2
setIntersect([_ | R], S2, A) :-
  setIntersect(R, S2, A).


% Question 2
swap([], []).
swap([A], [A]).
swap([A, B | R1], [B, A | R2]) :- % if A and B
  swap(R1, R2). 


%question 3
% credit: eclass notes
append([],L,L).
append([A|L],L1,[A|L2]) :- append(L,L1,L2).
flatten([],[]).
flatten([A|L],[A|L1]) :-
     xatom(A), flatten(L,L1).
flatten([A|L],R) :-
     flatten(A,A1), flatten(L,L1), append(A1,L1,R).

xatom(A) :- atom(A).
xatom(A) :- number(A).

isListxatom([]).
isListxatom([H | T]) :-
  xatom(H),
  isListxatom(T).


filter([], _, _, []).
filter(L, OP, N, Output) :-
  \+ isListxatom(L) , flatten_and_filter(L, OP, N, Output).
filter([F | R], greaterThan, N, [F | Output]) :-
  F > N, filter(R, greaterThan, N, Output).
filter([F | R], greaterThan, N, Output) :-
  F =< N, filter(R, greaterThan, N, Output).
filter([F | R], equal, N, [F | Output]) :-
  F == N, filter(R, equal, N, Output).
filter([F | R], equal, N, Output) :-
  F \== N, filter(R, equal, N, Output).
filter([F | R], lessThan, N, [F | Output]) :-
  F < N, filter(R, lessThan, N, Output).
filter([F | R], lessThan, N, Output) :-
  F >= N, filter(R, lessThan, N, Output).
flatten_and_filter(L, OP, N, Output) :-
  flatten(L, L1), filter(L1, OP, N, Output).
