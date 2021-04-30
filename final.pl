:- use_module(library(clpfd)).

% append(L1,L2,L3): append L1 and L2 to get L3
append([],L,L).
append([A|L],L1,[A|L2]) :- append(L,L1,L2).

% member(A,L): A is in list L
member(A,[A|_]).
member(A,[B|L]) :- A \== B, member(A,L).

cartesian([], _, []).
cartesian([A|N], L, M) :-
     pair(A,L,M1),
     cartesian(N, L, M2),
     append(M1, M2, M).
cartesian([a,b], [d,e], [[a,d], [a,e], [b,d], [b,e]]).

pair(_, [], []).
pair(A, [B|L], [[A,B]|N] ) :- pair(A, L, N).

% reverse(X,Y): Y is the reverse of input list X
reverse([], []).
reverse([A|L1], L2) :- reverse(L1, N), append(N, [A], L2).

% sum(L,N) will have N bound to the sum of the numbers in L.
sum([],0).
sum(N,N) :- number(N), !.
sum([A|L],S) :- sum(A,S1), sum(L,S2), S is S1 + S2.

% flatten(L,L1): flatten a list of atoms (atoms and numbers) L to a flat list L1.
flatten([],[]).
flatten([A|L],[A|L1]) :-
     xatom(A), !, flatten(L,L1).
flatten([A|L],R) :-
     flatten(A,A1), flatten(L,L1), append(A1,L1,R).

xatom(A) :- atom(A).
xatom(A) :- number(A).
% cut
x :- p, !, q.
x :-r.
% same as if p then q else r

% setIntersect(+S1,+S2,-S3)
% this takes in 2 lists of atoms, S1 and S2.
% S3 is a list of atoms that are in both S1 and S2
% S1 is always a list of distinct atoms.
setIntersect([], _, []). % if first list is empty return empty list
setIntersect([First | Rest], S2, [First | Output]) :-
  xmember(First, S2), setIntersect(Rest, S2, Output). % if First element of S1 is in S2 then add it to the output list, and check the rest of S1 against S2
setIntersect([_ | R], S2, A) :-
  setIntersect(R, S2, A).
/*
 swap(+L, -R)
 this takes in a list L, and makes it so that R
 has every 2 elements swapped with each other
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

/*
 filter(+L,+OP,+N,-L1)
 this takes in L, a possibly nested list of numbers, and outputs L1,
 which has all of the numbers in L that meet certain criteria.
 The criteria is defined by OP and N, where OP is "greaterThan", "equal",
 or "lessThan", and N is a number.
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
  append(Out1, Out2, Output).

filter([_ | R], OP, N, Output) :-
  filter(R, OP, N, Output).

doOP(X, greaterThan, Y) :- X > Y.
doOP(X, lessThan, Y) :- X < Y.
doOP(X, equal, Y) :- X =:= Y.
/*
 countAll(+L,-N)
 given L, a flat list of atoms, the number of occurences of each atom is counted
 N is a list of pairs of the form [a,n], where a is the atom and n is the
 number of occurences. N is sorted in increasing order by n. If n is tied,
 there is no tie breaking functionality based on what a is.
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

incrementCount(X, [], [[X,1]]).
incrementCount(X, [[X, CountX] | R], [[X, CountXPlusOne] | R]) :-
  CountXPlusOne is CountX + 1.
incrementCount(Y, [[X, CountX] | R], [[X, CountX] | T]) :-
  X \= Y,
  incrementCount(Y, R, T).

bubble_sort(L, L) :- is_sorted(L).
bubble_sort(L, Output) :-
  xappend(X, [[A, Acount],[B, Bcount] | Y], L),
  Acount > Bcount,
  xappend(X, [[B, Bcount], [A, Acount] | Y], R),
  bubble_sort(R, Output).

is_sorted([]).
is_sorted([[_,_]]).
is_sorted([[_, Acount], [B,Bcount] | R]) :-
  Acount =< Bcount,
  is_sorted([[B, Bcount] | R]).

/*
 sub(+L,+S,-L1)
 this takes in a possibly nested list of atoms L, and a list of pairs S
 of the form [[x1, e1], ...].
 L1 is the same as L, except that any occurences of xi is replaced by ei.
 xi is always an atom
 ei is an arbitrary expression

 every xi is assumed to be distinct, and xi will never occur in ei
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

find_substitution(X, [], X).
find_substitution(X, [[X, E] | _], E).
find_substitution(X, [[Xi, _] | S], Output) :-
  X \= Xi,
  find_substitution(X, S, Output).

/*
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

subset([], _).
subset([X|Xs], Set) :-
  append(_, [X|Set1], Set),
  subset(Xs, Set1).

/*
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

% SEND + MORE = MONEY problem
encrypt(W1,W2,W3) :- 
   length(W1,N),           % if you need to know the lengths of words
   length(W3,N1),   
   append(W1,W2,W),
   append(W,W3,L),
   list_to_set(L,Letters), % remove duplicates, a predicate in list library
   [LeadLetter1|_] = W1,   % identify the leading letter to be set to non-zero
   [LeadLetter2|_] = W2,
   [LeadLetter3|_] = W3,
   !,                      % never need to redo the above
   Letters ins 0..9,
   all_distinct(Letters),
   LeadLetter1 #\= 0,
   LeadLetter2 #\= 0,
   LeadLetter3 #\= 0,
   digits_to_number(W1, Sum1),
   digits_to_number(W2, Sum2),
   digits_to_number(W3, Sum3),
   Sum1 + Sum2 #=  Sum3,
   label(Letters).

% This predicate converts a list of numbers to an
% actual number, assuming base10. We sum up
% each number, and multiply by 10 based on its
% position in the list
%:- begin_tests(digits).
%test(digits) :-
%  digits_to_number([1,2,3,4,5,6,7,8,9,0], 1234567890).
%:-end_tests(digits).
digits_to_number(Digits, Number) :-
  digits_to_number(Digits, 0, Number).
digits_to_number([], Sum, Sum).
digits_to_number([Digit | Rest], Sum, Number) :-
  PartialSum #= Sum*10 + Digit,
  digits_to_number(Rest, PartialSum, Number).

% subsetsum(L, N)
% This predicate takes in a list L, and an integer N,
% and it determines if there exists a subset of the list such that 
% the sum is equal to N
%:-end_tests(subsetsum).
subsetsum(L, N) :-
  same_length(L, Coefficients),
  Coefficients ins 0..1,
  maplist(applyCoeff, L, Coefficients, Output),
  sum(Output, #=, N),
  label(Coefficients).

applyCoeff(Coeff, X, Y) :- Y #= Coeff * X.

% assign(W1, W2)
% given facts about papers and reviewers as defined in the question,
%
% (They are of the form): 
% paper(ID, Co-author1, Co-author2, Subject)
% reviewer(Name, Subject1, Subject2)
%
% this predicate assigns reviewers to the lists W1 and W2,
% such that  no one reviews their own paper,
% and also each reviewer must have the subject in their fact
% No reviewer is assigned more than workLoadAtMost papers,
% and each paper is assigned to 2 reviewers. 
% The paper index corresponds to the indices in W1 and W2.
odd([A], O) :-
% For example, The names that appear at index 1 review paper 1.
% 
% How it works:
% Because this is a quite complex problem, we want to restrict the
% domain as much as possible as soon as possible. 
% Each reviewer is assigned a number between 0 and the total number of reviewers
% multiplied by workLoadAtMost times minus one. We do this so that we can use
% modulo arithmetic and the all_distinct predicate. My initial solution
% instead generated two lists of numbers and then made sure that no
% number appeared more than workLoadAtMost times in either, but that
% was way too slow, which is why we're using the modulo solution here.
%
% The domains are generated in the gen_domain predicate defined below
%
% Then, we make sure that once we have 2 lists of numbers,
% that these reviewers satisfy the constraints above using the
% constrain_paper predicate defined below
%
% Finally, we convert the reviewer numbers to the reviewer
% name using modulo arithmetic. We use maplist with
% the reviewer_to_int predicate defined below.
%
% Test cases:
% This smaller example works almost instantly on my machine
%paper(1,lily,xxx,ai).
%paper(2,peter,john,database).
%paper(3,ann,xxx,theory).
%paper(4,ken,lily,network).
%paper(5,kris,xxx,games).
%reviewer(lily,theory,network).
%reviewer(john,ai,theory).
%reviewer(peter,database,network).
%reviewer(ann,theory,network).
%reviewer(kris,theory,games).
%reviewer(ken,database,games).
%reviewer(bill,database,ai).
%reviewer(jim,theory,games).
%workLoadAtMost(2).
%?- assign(W1, W2).
%W1 = [john, ken, lily, peter, ken],
%W2 = [bill, bill, john, ann, jim].
%
% This larger example takes about a minute to run on my
% quad core laptop (i7-7700hq)
%paper(1,lily,xxx,ai).
%paper(2,peter,john,database).
%paper(3,ann,xxx,theory).
%paper(4,ken,lily,network).
%paper(5,kris,xxx,games).
%paper(6,jim,xxx,games).
%paper(7,bill,xxx,theory).
%paper(8,bill,lily,ai).
%paper(9,peter,ann,games).
%reviewer(lily,theory,network).
%reviewer(john,ai,theory).
%reviewer(peter,database,network).
%reviewer(ann,theory,network).
%reviewer(kris,theory,games).
%reviewer(ken,database,games).
%reviewer(bill,database,ai).
%reviewer(jim,theory,games).
%reviewer(kevin,theory,games).
%reviewer(paul,ai,network).
%workLoadAtMost(2).
%?- assign(W1, W2).
%W1 = [john, ken, lily, peter, ken, kris, lily, john, jim],
%W2 = [bill, bill, kris, ann, jim, kevin, ann, paul, kevin].
assign(W1, W2) :-
  papers(PaperIDs),
  gen_domain(W1, W2, N1, N2),
  append(N1, N2, N),
  all_distinct(N),
  maplist(constrain_paper, PaperIDs, N1, N2),
  label(N),
  maplist(reviewer_to_int, N1, W1),
  maplist(reviewer_to_int, N2, W2).


% This predicate does a couple things:
% It makes sure that W1 and W2 are the same length as the number of papers
% It also makes N1, and N2 which are lists of numbers
% which will then be converted into reviewer names for W1 and W2.
% These are also of the same length as the number of papers.
% The domain for each entry in N1 and N2 is restricted so that
% it is between 0 and the number of reviewers times workLoadAtMost - 1 times.
% That way, we can do modulo arithmetic, and all_distinct to assign
% reviewers to papers. 
gen_domain(W1, W2, N1, N2) :-
  count_papers(NumPapers),
  count_reviewers(NumReviewers),
  workLoadAtMost(Max),
  length(W1, NumPapers),
  length(W2, NumPapers),
  length(N1, NumPapers),
  length(N2, NumPapers),
  R is NumReviewers * Max,
  Range is R - 1,
  N1 ins 0..Range,
  N2 ins 0..Range.

% This predicate takes in 2 numbers, each corresponding to one reviewers,
% and it ensures that the constraints defined in the assignment question are
% satisfied.
%
% We check that the first number is less than the second number because it
% reduces the search space, even though the overall answer would not change.
% Then, we convert the reviewer numbers to the reviewer names, and make sure
% that no author is reviewing their own paper, and that the reviewers
% assigned have that subject area in their definitions
%
% This predicate is used with maplist in the assignPaper predicate, so
% that the papers at each index is checked with this predicate.
constrain_paper(Index, Rev1Num, Rev2Num) :-
  Rev1Num #< Rev2Num,
  count_reviewers(NumReviewers),
  Rev1Mod #= Rev1Num mod NumReviewers,
  Rev2Mod #= Rev2Num mod NumReviewers,
  Rev1Mod #< Rev2Mod,
  Rev1Mod #\= Rev2Mod,
  paper(Index, Author1, Author2, Subject),
  reviewer_to_int(Rev1Num, Rev1Name),
  reviewer_to_int(Rev2Num, Rev2Name),
  reviewer(Rev1Name, Sub1, Sub2),
  reviewer(Rev2Name, Sub3, Sub4),
  Author1 \= Rev1Name, 
  Author1 \= Rev2Name, 
  Author2 \= Rev1Name, 
  Author2 \= Rev2Name,
  one_of_subject(Subject, Sub1, Sub2, Sub3, Sub4).

% This predicate takes a reviewer number, and converts it to a reviewer name
% Modulo arithmetic is done, because we expanded the range of numbers for
% reviewers, and multiple numbers need to map to a single reviewer.
% For example, if there were 10 reviewers, the numbers 1 and 11 would map to the 
% second reviewer in the list.
reviewer_to_int(Int, Name) :-
  reviewers(L),
  count_reviewers(NumReviewers),
  Index #= Int mod NumReviewers,
  nth0(Index, L, Name).

% This predicate counts how many papers there are
count_papers(Count) :-
  aggregate_all(count, paper(_,_,_,_), Count).

% This predicate counts how many reviewers there are
count_reviewers(Count) :-
  aggregate_all(count, reviewer(_, _, _), Count).

% This predicate makes L a list of all of the paper IDs
papers(L) :-
  findall(ID, paper(ID, _, _, _), L).

% This predicate makes L a list of all the reviewer names
reviewers(L) :-
  findall(R, reviewer(R, _, _), L).

% This predicate makes sure that both reviewers have the subject in one of
% their subject fields
one_of_subject(S, S1, _, S3, _) :-
  S = S1,
  S = S3.
one_of_subject(S, _, S2, _, S4) :-
  S = S2,
  S = S4.
one_of_subject(S, _, S2, S3, _) :-
  S = S2,
  S = S3.
one_of_subject(S, S1, _, _, S4) :-
  S = S1,
  S = S4.

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

%Given a list L of elements, each of which represents a course and its enrollment, e.g.[cmput325, [john, lily, ken,...]], define a predicate
%courses_taken(+L,+Name,-Courses)

courses_taken([], _, []).
courses_taken([[Course, List] | L], Name, [Course | Rest]) :-
    member(Name, List),
    !,
    courses_taken(L, Name, Rest).
courses_taken([_ | L], Name, R) :-
    courses_taken(L, Name, R).

  %I NEED TO USE THE BATHROOM
  % find a way to remove every 2nd element
odd([], []).
odd([A], [A]).
odd([A, _], [A]).
odd([A, B | T], O) :-
  odd(T, [A | O]).

remove_even(L, O) :-
  remove_even_helper(L, O, 0).

remove_even_helper([], [], _).
remove_even_helper([H | T], O, Index) :-
  Index mod 2 is 0,
  remove_even_helper(T, [H | O], NewIndex),
  NewIndex is Index + 1.
remove_even_helper([H | T], O, Index) :-
  \+ Index mod 2 is 0,
  remove_even_helper(T, O, NewIndex),
  NewIndex is Index + 1.

palindrome([]).
palindrome([_]). % ok because it's always gonna match to a letter if the input is correct
palindrome(L) :-
  L = [H | T],
  reverse(L, Rev),
  Rev = [RH | RT],
  H = RH.
  % todo chop off last element


gen_matrix(N, M, Mtr).
