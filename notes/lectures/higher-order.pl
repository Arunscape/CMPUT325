/* Higher-order predicates; some used in sudoku.pl

call/1

?- call(atom(a)).   % equivalent to goal: ?- atom(a).
true.

?- call(atom,a).    % same call as above
true.

?- call(4 > 5).
false.

?- call(4 < 5).
true.

?- call( N #> 5).
N in 6..sup.   
   % intuitively, supremum denotes the infinity, similarly for infirmum. 


With call/1, we can define maplist/1, similar to mapcar in lisp

-----------------------------------------
maplist(_, []).
maplist(Pred, [A|As]) :-
        call(Pred(A)),
        maplist(Pred, As).
-----------------------------------------

In fact, maplist/N have been defined in swi prolog.

?- maplist(atom, [a,b,c]).   % atom is a unary predicate
true.

?- maplist( >=, [1,2,3], [1,1,2]). % >= is a binary predicate
true.

?- maplist(>(3), [1,2]).   %like "curried function" in lisp
true.                      %equivalent to goal: ?- 3>1, 3>2.

?- maplist( #>=(4), [A,B,C]).
clpfd: (A in inf..4),
clpfd: (B in inf..4),
clpfd: (C in inf..4).


% In solving sudoku, we used the predicate transpose/2

?- transpose([[A,B,C],[D,E,F],[G,H,I]],K).
K = [[A, D, G], [B, E, H], [C, F, I]].

*/




