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
test(incrementcount3) :-
  incrementCount(x, [[a,1], [b,2]], [[a,1], [b,2], [x,1]]).
test(incrementcount4) :-
  incrementCount(x, [[a,1], [x,1], [b,2]], [[a,1], [x,2], [b,2]]).
test(bubble_sort) :-
  bubble_sort([[a,1], [b,2], [c,3], [d,4]], [[a,1], [b,2], [c,3], [d,4]]).
test(bubble_sort) :-
  bubble_sort([[b,2], [d,4], [c,3], [a,1]], Result),
  is_sorted(Result).
:- end_tests(question4).

% https://kti.mff.cuni.cz/~bartak/prolog/sorting.html
% Bubble sort is a really  simple sorting algorithm.
% In fact, it was one of the first ones I learned 
% I figure since the lists are small, this won't actually be too bad
is_sorted([]).
is_sorted([[_,_]]).
is_sorted([[_, Acount], [B,Bcount] | R]) :-
  Acount =< Bcount,
  is_sorted([[B, Bcount] | R]).

bubble_sort(L, L) :- is_sorted(L).
bubble_sort(L, Output) :-
  xappend(X, [[A, Acount],[B, Bcount] | Y], L),
  Acount > Bcount,
  xappend(X, [[B, Bcount], [A, Acount] | Y], R),
  bubble_sort(R, Output).


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

countAll([], []).
countAll([F | R], SortedOutput) :-
  countAll(R, IntermediateOutput),
  incrementCount(F, IntermediateOutput, Output),
  bubble_sort(Output, SortedOutput).





:- begin_tests(question5).
test(sub) :-
  sub([a,[a,d],[e,a]],[[a,2]],L),
  L= [2,[2,d],[e,2]].
:- end_tests(question5).

xatom(A) :- atom(A).
xatom(A) :- number(A).
% assume xi is an atom
% ei is any expression
% S is in the form [ [xi, ei], ...]
% L1 is L with the substitutions
sub([], _, []).
sub([Xi | R], [[Xi, Ei] | S], [Ei | Output]) :- % substitute atom
  sub(R, [[Xi, Ei] | S], Output).
sub([Xi | R], S, [Xi | Output]) :- % is an atom but doesn't match
  xatom(Xi),
  sub(R, S, Output).
sub([L | R], S, [Output | ROutput]) :- % not an atom
  sub(L, S, Output),
  sub(R, S, ROutput).


% question 6
%
node(a).
node(b).
node(c).

edge(a,b).
edge(b,c).
edge(c,a).
clique(L) :- 
  findall(X,node(X),Nodes),
  subset(L,Nodes), allConnected(L).

subset([], _).
subset([X|Xs], Set) :-
  append(_, [X|Set1], Set),
  subset(Xs, Set1).


:- begin_tests(question7).
test(convert1) :-
  convert([e,e,a,e,b,e],R),
  R = [c,c].
test(convert2) :-
  convert([e,q,a,b,e,e],R),
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

end_q_exists([q | _]).
end_q_exists([X | R]) :-
  X \= q,
  end_q_exists(R).

convert(Term, Result) :-
  convertHelper(Term, Result, false).
convertHelper([], [], _).
convertHelper([q | R], [q | Output], false) :- % found a q, quote starts
  end_q_exists(R),
  convertHelper(R, Output, true).
convertHelper([q | R], [q | Output], false) :- % no end q exists
  \+ end_q_exists(R),
  convertHelper(R, Output, false).
convertHelper([q | R], [q | Output], true) :- % found a second q, quote ends
  convertHelper(R, Output, false).
convertHelper([e | R], Output, false) :- % discard e not in quote
  convertHelper(R, Output, false).
convertHelper([X | R], [X | Output], true) :- % keep any character that is not q as-is in a quote
  X \= q,
  convertHelper(R, Output, true).
convertHelper([X | R], [c | Output], false) :- % anything not in a quote is changed to c
  X \= q,
  X \= e,
  convertHelper(R, Output, false).
