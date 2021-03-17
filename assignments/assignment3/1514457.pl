% Question 1

xmember(_, []) :- false.
xmember(V, [V | _]) :- true. % if V is the head, then it exists
xmember(V, [_ | R]) :-
  xmember(V, R).

% , is and ; is or

setIntersect([], _, []). % if first list is empty return empty list
setIntersect([First | Rest], S2, [First | Output]) :-
  xmember(First, S2), setIntersect(Rest, S2, Output). % if First element of S1 is in S2 then add it to the output list, and check the rest of S1 against S2
setIntersect([_ | R], S2, A) :-
  setIntersect(R, S2, A).


% Question 2
swap([], []).
swap([A], [A]).
swap([A, B | R1], [B, A | R2]) :-
  swap(R1, R2).
