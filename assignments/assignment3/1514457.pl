% Question 1

:- begin_tests(question1).
test(setIntersect) :-
  setIntersect([a,b,c,d,e,g],[b,a,c,e,f,q],S),
  S = [a,b,c,e].
:- end_tests(question1).
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
:- begin_tests(question2).
test(swap1) :-
  swap([a,1,b,2], W),
  W = [1,a,2,b].
test(swap2) :-
  swap([a,1,b], W),
  W = [1,a,b].
:- end_tests(question2).
swap([], []).
swap([A], [A]).
swap([A, B | R1], [B, A | R2]) :- % if A and B
  swap(R1, R2). 


%question 3
% credit: eclass notes
xappend([],L,L).
xappend([A|L],L1,[A|L2]) :- xappend(L,L1,L2).


:- begin_tests(question3).
test(greater) :-
  doOP(3, greaterThan, 1).
test(equal) :-
  doOP(42, equal, 42).
test(less) :-
  doOP(3, lessThan, 5).
test(filtergreater) :-
  filter([3,4,[5,2],[1,7,3]],greaterThan,3,W),
  W = [4,5,7].
test(filterequal) :-
  filter([3,4,[5,2],[1,7,3]],equal,3,W),
  W = [3,3].
test(filterless) :-
  filter([3,4,[5,2],[1,7,3]],lessThan,3,W),
  W = [2,1].
:- end_tests(question3).
doOP(X, greaterThan, Y) :- X > Y.
doOP(X, lessThan, Y) :- X < Y.
doOP(X, equal, Y) :- X =:= Y.


filter([], _, _, []).
filter([F | R], OP, N, [F | Output]) :- % if a number and it matches condition, include in output
  number(F),
  doOP(F, OP, N),
  filter(R, OP, N, Output).

filter([F | R], OP, N, Output) :- % if not a number, its a nested list. Filter that and whatever comes next and combine it
  \+ number(F),
  filter(F, OP, N, Out1),
  filter(R, OP, N, Out2),
  xappend(Out1, Out2, Output).

filter([_ | R], OP, N, Output) :- % at this point, the number does not satisfy the condition, so yeet it
  filter(R, OP, N, Output).


:- begin_tests(question4).
test(countone) :-
  countOne(a, [a,b,c,a,e,f,a,h], 3).
test(countall) :-
  countAll([a,b,e,c,c,b],N),
  N = [[a,1],[e,1],[b,2],[c,2]].

test(incrementcount) :-
  incrementCount(x, [], [[x,1]]).
test(incrementcount2) :-
  incrementCount(x, [[x,1]], [[x,2]]).
test(incrementCount3) :-
  incrementCount(x, [[a,1], [b,2]], [[a,1], [b,2], [x,1]]).
test(incrementcount4) :-
  incrementCount(x, [[a,1], [x,1], [b,2]], [[a,1], [x,2], [b,2]]).
:- end_tests(question4).

countOne(_, [], 0).
countOne(X, [X | Rest], Output) :-
  countOne(X, Rest, OutputRest),
  Output is 1 + OutputRest.
countOne(X, [Y | Rest], Output) :-
  X \== Y,
  countOne(X, Rest, Output).

incrementCount(X, [], [[X,1]]).
incrementCount(X, [[X, CountX] | R], [[X, CountXPlusOne] | R]) :-
  CountXPlusOne is CountX + 1.
incrementCount(Y, [[X, CountX] | R], [[X, CountX] | T]) :-
  X \= Y,
  incrementCount(Y, R, T).







:- begin_tests(question5).
test(sub) :-
  sub([a,[a,d],[e,a]],[[a,2]],L),
  L= [2,[2,d],[e,2]].
:- end_tests(question5).

:- begin_tests(question7).
test(convert1) :-
  convert([e,e,a,e,b,e],R),
  R = [c,c].
test(convert2) :-
  convert([e,e,a,e,b,e],R),
  R = [q,c,c].
test(convert3) :-
  convert([e,a,e,e],R),
  R = [c].
test(convert4) :-
  convert([e,q,a,e,b,q,e,a,e],R),
  R = [q,a,e,b,q,c].
test(convert5) :-
  convert([a,q,e,l,q,r,e,q,b,e],R),
  R = [c,q,e,l,q,c,q,c].
test(convert6) :-
  convert([q,e,q,b,q,e,l,q,a,e],R),
  R = [q,e,q,c,q,e,l,q,c].
:- end_tests(question7).
