/*
The four square problem:
Given a natural number N, Sol is bound to a list of 4 or fewer numbers
so that the sum of their squares equals N
*/

f_sq(N,Sol) :-
   length(L,4),
   L ins 0..N,
   constraint0(N,L),
   label(L),
   removeZero(L,Sol).

constraint0(N,[S1,S2,S3,S4]) :-
   N #= S1*S1 + S2*S2 + S3*S3 + S4*S4,
   S1 #=< S2, S2 #=< S3, S3 #=< S4.
     % this removes some redundant solutions

removeZero([],[]).
removeZero([0|L],L1) :-
   !,
   removeZero(L,L1).
removeZero([A|L],[A|L1]) :-
   removeZero(L,L1).

% find all solutions.
fsq_all(N,L) :- findall(Sol,f_sq(N,Sol),L).

