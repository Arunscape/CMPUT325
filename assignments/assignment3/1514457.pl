/*
Name: Arun Woosaree
Student Number: 1514457
Course: CMPUT 325
Section: B1
Assignment 3
*/

/* Question 1
 setIntersect(+S1,+S2,-S3)
 this takes in 2 lists of atoms, S1 and S2.
 S3 is a list of atoms that are in both S1 and S2
 S1 is always a list of distinct atoms.

 How it works:

 If S1 is empty, then S3 is also empty (base case)

 If the head if S1 is in S2 then add that to the output
 and recurse with the tail of S1,

 else just recurse with the tail of S1, not adding the head of S1 to the output.

*/
%:- begin_tests(question1).
%test(setIntersect) :-
%  setIntersect([a,b,c,d,e,g],[b,a,c,e,f,q],S),
%  S = [a,b,c,e].
%:- end_tests(question1).

setIntersect([], _, []). % if first list is empty return empty list
setIntersect([First | Rest], S2, [First | Output]) :-
  xmember(First, S2), setIntersect(Rest, S2, Output). % if First element of S1 is in S2 then add it to the output list, and check the rest of S1 against S2
setIntersect([_ | R], S2, A) :-
  setIntersect(R, S2, A).

/*
 xmember(+L, -L1)
 the xmember predicate is taken from the course notes
 it returns true if a given element is found in a list
*/
xmember(A,[A|_]).
xmember(A,[B|L]) :- 
  A \== B, xmember(A,L).


/* Question 2
 swap(+L, -R)
 this takes in a list L, and makes it so that R
 has every 2 elements swapped with each other

 How it works:
 if L is empty, then the output is also empty (base case)

 if there are an odd number of elements, another base case to consider
 is when there is only one element left in the list. We just put that one
 element in the output also.

 if there are 2 or more elements, we swap the first 2,
 and then recurse on the tail of the list

*/
%:- begin_tests(question2).
%test(swap1) :-
%  swap([a,1,b,2], W),
%  W = [1,a,2,b].
%test(swap2) :-
%  swap([a,1,b], W),
%  W = [1,a,b].
%:- end_tests(question2).

swap([], []).
swap([A], [A]).
swap([A, B | R1], [B, A | R2]) :-
  swap(R1, R2). 



/* Question 3
 filter(+L,+OP,+N,-L1)
 this takes in L, a possibly nested list of numbers, and outputs L1,
 which has all of the numbers in L that meet certain criteria.
 The criteria is defined by OP and N, where OP is "greaterThan", "equal",
 or "lessThan", and N is a number.

 How it works:
 we check if the condition is matched using doOP, which is defined below.

 if L is empty then L1 is also empty (base case)

 if the head of L is a number and the condiion is satisfied,
 we add it to the output

 if the head of L is not a number, it must be a nested list.
 filter that, and also whatever comes next, and then combine the results
 with xappend

 otherwise, that means the head of L is a number that does not satisfy the
 condition, so we filter the tail of L and do not add the head to the output
*/
%:- begin_tests(question3).
%test(filtergreater) :-
%  filter([3,4,[5,2],[1,7,3]],greaterThan,3,W),
%  W = [4,5,7].
%test(filterequal) :-
%  filter([3,4,[5,2],[1,7,3]],equal,3,W),
%  W = [3,3].
%test(filterless) :-
%  filter([3,4,[5,2],[1,7,3]],lessThan,3,W),
%  W = [2,1].
%:- end_tests(question3).

filter([], _, _, []).
filter([F | R], OP, N, [F | Output]) :-
  number(F),
  doOP(F, OP, N),
  filter(R, OP, N, Output).

filter([F | R], OP, N, Output) :-
  \+ number(F),
  filter(F, OP, N, Out1),
  filter(R, OP, N, Out2),
  xappend(Out1, Out2, Output).

filter([_ | R], OP, N, Output) :-
  filter(R, OP, N, Output).


/*
 xappend(+L1, +L2, -O)
 the xappend predicate is taken from course notes
 it takes in 2 lists and outputs L2 appended to L1
 */
xappend([],L,L).
xappend([A|L],L1,[A|L2]) :- xappend(L,L1,L2).

/*
 doOP(+X, +OP, +Y)
 X and Y are numbers, and OP is either "equal", "greaterThan", or "lessThan"
 it returns true if the condition is satisfied

 How it works: 
 if OP is greaterThan, it applies the > operator to X and Y (X > Y)
 if OP is equal, it applies the =:= operator to X and Y (X =:= Y)
 if OP is lessThan, it applies the < operator to X and Y (X < Y)
 */
%:- begin_tests(doop).
%test(greater) :-
%  doOP(5, greaterThan, 3).
%test(equal) :-
%  doOP(42, equal, 42).
%test(less) :-
%  doOP(3, lessThan, 5).
%:- end_tests(doop).

doOP(X, greaterThan, Y) :- X > Y.
doOP(X, lessThan, Y) :- X < Y.
doOP(X, equal, Y) :- X =:= Y.

/* Question 4
 countAll(+L,-N)
 given L, a flat list of atoms, the number of occurences of each atom is counted
 N is a list of pairs of the form [a,n], where a is the atom and n is the
 number of occurences. N is sorted in increasing order by n. If n is tied,
 there is no tie breaking functionality based on what a is.

 How it works:
 if L is empty, then N is also empty
 
 else, we recurse on the tail of L, and save the results in a temporary variable
 we use that temporary variable to keep track of the counts of each atom
 using the incrementCount predicate defined below,
 and finally we sort the results by n, using our good old friend bubble sort,
 an inefficient, but simple sorting algorithm.

*/
%:- begin_tests(question4).
%test(countall) :-
%  countAll([a,b,e,c,c,b],N),(
%  N = [[a,1],[e,1],[b,2],[c,2]];
%  N = [[e,1],[a,1],[b,2],[c,2]]
%).% other permutations are possible, mine seems to return the second permutation
%:- end_tests(question4).

countAll([], []).
countAll([F | R], SortedOutput) :-
  countAll(R, IntermediateOutput),
  incrementCount(F, IntermediateOutput, Output),
  bubble_sort(Output, SortedOutput).


/* incrementCount(+X, +L, -O)
 this takes in an atom X, and a list of pairs of the form [a,n],
 where a is an atom and n is a count, and makes it so that O
 is the same list as L, except the count corresponding to X is incremented by 1

 How it works:
 if L is empty, i.e. X is not found in L, then we add a pair [X,1] to say it
 was counted once (base case)

 if X is found in L, then we increment the count by 1

 if X is not found in L, but L is not empty yet, recurse on the tail of L and
 look for it until the list is empty
*/
%:- begin_tests(incrementcount).
%test(incrementcount) :-
%  incrementCount(x, [], [[x,1]]).
%test(incrementcount2) :-
%  incrementCount(x, [[x,1]], [[x,2]]).
%test(incrementcount3) :-
%  incrementCount(x, [[a,1], [b,2]], [[a,1], [b,2], [x,1]]).
%test(incrementcount4) :-
%  incrementCount(x, [[a,1], [x,1], [b,2]], [[a,1], [x,2], [b,2]]).
%:- end_tests(incrementcount).
incrementCount(X, [], [[X,1]]).
incrementCount(X, [[X, CountX] | R], [[X, CountXPlusOne] | R]) :-
  CountXPlusOne is CountX + 1.
incrementCount(Y, [[X, CountX] | R], [[X, CountX] | T]) :-
  X \= Y,
  incrementCount(Y, R, T).

/* bubble_sort(+L, -O)
 it's our favourite sorting algorithm, bubble sort!
 actually, it's modified a bit, see here instead of just dealing with a
 list of numbers, we look at a list of pairs, and compare the number which is
 the second element in the pair. Otherwise, it's the exact same.
 I realize this is a horribly inefficient sorting algorithm, but also I
 figure that the list sizes we're dealing with are relatively small, so actually
 it shouldn't bee too bad

 How it works:
 if L is already sorted, O is the same as L (base case)
 is_sorted is defined below

 else, we do the normal bubble sort algorithm.
 we do a pass over the list, and swap elements that are out of order
 then, we repeat this until the list is sorted
*/
%:- begin_tests(bubblesort).
%test(bubble_sort) :-
%  bubble_sort([[a,1], [b,2], [c,3], [d,4]], [[a,1], [b,2], [c,3], [d,4]]).
%test(bubble_sort) :-
%  bubble_sort([[b,2], [d,4], [c,3], [a,1]], Result),
%  is_sorted(Result).
%:- end_tests(bubblesort).

bubble_sort(L, L) :- is_sorted(L).
bubble_sort(L, Output) :-
  xappend(X, [[A, Acount],[B, Bcount] | Y], L),
  Acount > Bcount,
  xappend(X, [[B, Bcount], [A, Acount] | Y], R),
  bubble_sort(R, Output).


/* is_sorted(+L)
 this is true if the list provided is sorted

 How it works:
 an empty list is sorted (base case)
 a list with only one element is sorted (base case)
 
 else we have 2 or more elements. check if the first element is less than the
 second element, and recurse on the tail of the list.
*/
is_sorted([]).
is_sorted([[_,_]]).
is_sorted([[_, Acount], [B,Bcount] | R]) :-
  Acount =< Bcount,
  is_sorted([[B, Bcount] | R]).


/* Question 5
 sub(+L,+S,-L1)
 this takes in a possibly nested list of atoms L, and a list of pairs S
 of the form [[x1, e1], ...].
 L1 is the same as L, except that any occurences of xi is replaced by ei.
 xi is always an atom
 ei is an arbitrary expression

 every xi is assumed to be distinct, and xi will never occur in ei

 How this works:
 If L is empty, then so is L1 (base case)
 
 If the head of L is an atom, we attempt to find a substitution for it
 with the find_substitution predicate defined below. If a substitution is
 not found, the atom is left as-is in the output

 Else, the head of L is not an atom. We call sub on both the head and the tail,
 and combine the outputs
*/
%:- begin_tests(question5).
%test(sub) :-
%  sub([a,[a,d],[e,a]],[[a,2]],L),
%  L= [2,[2,d],[e,2]].
%
%test(sub2) :-
%  sub([a,b,c], [[a,d], [b, c]], [d, c, c]).
%
%test(sub3) :-
%  sub([a,[b, [c, [d]]]], [[a,z], [z, v], [c, g], [d, p]] , [z,[b, [g, [p]]]]).
%
%test(subexpression) :-
%  sub([a, b, c], [[b, 4 < 1]], [a, 4<1, c]).
%:- end_tests(question5).

sub([], _, []).
sub([X | R], S, [Y | Output]) :- % is an atom
  atom(X),
  find_substitution(X, S, Y),
  sub(R, S, Output).
sub([L | R], S, [Output | ROutput]) :- % not an atom
  sub(L, S, Output),
  sub(R, S, ROutput).


/* find_substitution(+X, +S, -E)
 this takes in 2 arguments, X and S and outputs E
 X is an atom which we wish to substitute, and S is the list of pairs
 containing substitutions. This makes it so that E is the substituted
 value, or X is returned if it is not found.

 How it works:
 if the substitution list is empty, X is not substituted (base case)

 if a substitution is found, return E

 else, recurse on the tail of the substitution list and keep looking
 for a substitution
 
 */
find_substitution(X, [], X).
find_substitution(X, [[X, E] | _], E).
find_substitution(X, [[Xi, _] | S], Output) :-
  X \= Xi,
  find_substitution(X, S, Output).



/* Question 6
 allConnected(+L)
 this takes in a list, and it returns true if every node in L can reach every
 other node in L

 How it works:
 first, an empty list is all connected (base case)

 then, we check every element in L, and see if it is connected using the
 allConnected helper predicate defned below
*/
%:- begin_tests(question6).
%test(allconnected) :-
%  allConnected([a,b,c]).
%:- end_tests(question6).

allConnected([]).
allConnected([F | R]) :-
  allConnectedHelper(F, R),
  allConnected(R).

/* allConnectedHelper(-X, -L)
 this checks if node X is connected to all elements in L

 How it works:
 if L is empty, then it is all connected (base case)
 else we check if X is connected to the first element in L,
 and also every other element in L by recursing on the tail of L

 the connected predicate is defined below
 */
allConnectedHelper(_, []).
allConnectedHelper(X, [F | R]) :-
  connected(X, F),
  allConnectedHelper(X, R).

/*connected(+X,+Y)
 this is true if there is an edge from X to Y,
 or from Y to X
 We do this, instead of defining edge(X,Y) :- edge(Y,X).
 because then we'd get infinite recursion and bad things happening
 */
connected(X,Y) :-
  edge(X,Y);
  edge(Y,X).

/* clique(+L)
 The clique problem is a graph-theoretic problem of finding a subset of nodes
 where each is connected to every other node in the subset. 

 This code is given to us in the assignment
*/
% for the node example defined in eclass
%:- begin_tests(clique).
%test(clique) :-
%  clique([a,b,c]).
%:- end_tests(clique).

clique(L) :- 
  findall(X,node(X),Nodes),
  subset(L,Nodes), allConnected(L).

/* subset(+L, -O)
 given L, O is a list of subsets of L

 This code is given to us in the assignment
*/
subset([], _).
subset([X|Xs], Set) :-
  append(_, [X|Set1], Set),
  subset(Xs, Set1).


/* Question 7
 convert(+Term,-Result)
 this takes in a Term, which is a list of atoms
 anything between two q atoms are kept as-is,
 while any 'e' outside of two q atoms are dropped,
 and anything else outside of two q atoms are changed to c

 for an unmatched q, anything after is considered outside of a q pair, so
 'e' will be dropped, and anything else changed to c. This happens when there
 is an odd number of q atoms

 How it works:
 we call the convertHelper predicate defined below, which takes in the same
 2 arguments as convert. It also takes in a third argument, which is a boolean
 to help with keeping track of matching q atoms
*/
%:- begin_tests(question7).
%test(convert1) :-
%  convert([e,e,a,e,b,e],R),
%  R = [c,c].
%test(convert2) :-
%  convert([e,q,a,b,e,e],R),
%  R = [q,c,c].
%test(convert3) :-
%  convert([e,a,e,e],R),
%  R = [c].
%test(convert4) :-
%  convert([e,q,a,e,b,q,e,a,e],R),
%  R = [q,a,e,b,q,c].
%test(convert5) :-
%  convert([a,q,e,l,q,r,e,q,b,e],R),
%  R = [c,q,e,l,q,c,q,c].
%test(convert6) :-
%  convert([q,e,q,b,q,e,l,q,a,e],R),
%  R = [q,e,q,c,q,e,l,q,c].
%:- end_tests(question7).

convert(Term, Result) :-
  convertHelper(Term, Result, false).


/* convertHelper(+Term, -Result, +Qfound)
 this is the main logic for convert.
 this takes in a Term, which is a list of atoms
 anything between two q atoms are kept as-is,
 while any 'e' outside of two q atoms are dropped,
 and anything else outside of two q atoms are changed to c

 for an unmatched q, anything after is considered outside of a q pair, so
 'e' will be dropped, and anything else changed to c. This happens when there
 is an odd number of q atoms
 Read the comments for the convert predicate above to see what this does.

 This has the same input and output arguments as convert. It also takes in a
 third argument, which is a boolean to help with keeping track of matching q
 atoms

 How it works:
 if Term is empty, then so is the result (base case)
 
 next, we check the head of Term. if it is a q, then a quote is starting.
 we check if there is a matching q using the end_q_exists predicate defined below.
 we then recurse on the tail of Term, and also Qfound, the third argument is now
 true. We also add q to the output.

 if a q is found, but no end q exists, everything after it is subject to the
 substitution rules we defined, so we recurse on the rest of Term
 We also add q to the output

 if a q is found, and Qfound is true, then we found the end of a quote
 we recurse on the tail of Term, and also Qfound is now false
 We also add q to the output

 if an e is found, and Qfound is false, then we are not in a quote and should
 drop the e. We recurse on the tail of Term without adding e to the output.

 if we find any character that is not q inside a quote, we leave it untouched
 and recurse on the tail of Term. That atom is added to the output.

 if we find any character that is not q or e outside a quote, we change it to c
 and recurse on the tail of Term. c is added to the output.
*/
convertHelper([], [], _).
convertHelper([q | R], [q | Output], false) :-
  end_q_exists(R),
  convertHelper(R, Output, true).
convertHelper([q | R], [q | Output], false) :-
  \+ end_q_exists(R),
  convertHelper(R, Output, false).
convertHelper([q | R], [q | Output], true) :-
  convertHelper(R, Output, false).
convertHelper([e | R], Output, false) :-
  convertHelper(R, Output, false).
convertHelper([X | R], [X | Output], true) :-
  X \= q,
  convertHelper(R, Output, true).
convertHelper([X | R], [c | Output], false) :-
  X \= q,
  X \= e,
  convertHelper(R, Output, false).

/* end_q_exists(+L)
 this takes in a list of atoms, and is true if there is a q atom in the list
 it assumes that the first q in the list is already removed
 (otherwise it would always return true!)

 How it works:
 if q is found, true is returned, else, we recurse on the tail of L
 */
end_q_exists([q | _]).
end_q_exists([X | R]) :-
  X \= q,
  end_q_exists(R).
