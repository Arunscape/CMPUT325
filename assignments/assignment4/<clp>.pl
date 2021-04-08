% Question 1
% Part 1 (5 examples of removal of domain values)
% Note: we are using the grid notation outlined in the introduction here.
% for example, A1 is the top left corner, and I9 is the bottom right corner
%
% Example 1
% Let's look at I4
% We start with a domain of 123456789
% The numbers 135 are taken by other squares in row I
% We remove these from the domain to get 246789
% The numbers 12369 are taken by other squares in the same 3x3 grid
% We remove these from the domain to get 478
% The numbers 123678 are taken by other squares in column 4
% We remove these from the domain to get 4
% Thus I4 must be 4 since we removed all other possible values from the domain

% Example 2
% Let's look at A4
% We start with a domain of 123456789
% The numbers 1359 are taken by other squares in row A
% We remove these from the domain to get 124678
% The numbers 23568 are taken by other squares in the same 3x3 grid
% We remove these from the domain to get 147
% The numbers 123678 and 4 (see example 1) are taken by other squares in column 4
% We remove these from the domain to get 9
% Thus A4 must be 9 since we removed all other possible values from the domain

% Example 3
% Let's look at I6
% We start with a domain of 123456789
% The numbers 135 are taken by other squares in row I
% also, 4 is in the same row, see example 1
% We remove these from the domain to get 26789
% The numbers 12369 are taken by other squares in the same 3x3 grid
% We remove these from the domain to get 78
% The numbers 235689 are taken by other squares in column 6
% These are removed from the domain to get 7
% Thus I6 must be 7 since we removed all other possible values from the domain

% Example 4
% Let's look at H5
% We start with a domain of 123456789
% The numbers 2389 are taken by other squares in row H
% We remove these from the domain to get 14567
% The numbers 12369 and 4 and 7 (see examples 1, 3) are taken by other squares in the same 3x3 grid
% We remove these from the domain to get 5
% Thus H5 must be 5 since we removed all other possible values from the domain

% Example 5
% Let's look at G5
% We start with a domain of 123456789
% The numbers 2389 are taken by other squares in row G
% We remove these from the domain to get 14567
% The numbers 12369, and 4 and 7 and 5 (see examples 1, 3. 4) are taken by other squares in the 3x3 grid
% We remove these from the domain to get 8
% Thus G5 must be 8 since we removed all other possible values from the domain

% Question 1 part 2
% I went ahead and enumerated all of the possible values for all of the squares
% and found that it is indeed not possible to solve this puzzle using AC-3 alone.
% The closest you get using this algorithm, is that the domain for a few squares
% are reduced to only two possibilities. However, none of them can be reduced to
% just one possibility using AC-3 alone
% One obvious example of a domain value that cannot possibly be removed by AC-3
% is the number 4. By simply observing the puzzle, (and also by enumerating
% all of the possible values for each square), you quickly find out that the
% number 4 cannot ever be eliminated from any of the squares using just AC-3
% because none of the given squares contains the value 4. That is, in the initial
% state of the board, the number 4 never appears once in the entire board.
% So, there is no row, column, or 3x3 grid where the number 4 can be eliminated
% from the domain for a particular square, since is does not appear anywhere on
% the board initially. This alone does not prove that the number 4 cannot be
% removed from any square by AC-3 however. It would still be possible for the
% value 4 to be removed from the domain of a square, if some other square's
% domain could be reduced to the number 4. However, none of the squares in this
% case are reduced to a single value using AC-3 alone, so we are stuck with the
% given values, and none of those values is the number 4, so 4 cannot be removed
% from the domain of any square using just the AC-3 algorithm.


:- use_module(library(clpfd)).
% Question 2

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

digits_to_number(Digits, Number) :-
  digits_to_number(Digits, 0, Number).
digits_to_number([], Sum, Sum).
digits_to_number([Digit | Rest], Sum, Number) :-
  PartialSum #= Sum*10 + Digit,
  digits_to_number(Rest, PartialSum, Number).

:- begin_tests(question2).
test(sendmoney) :-
  encrypt([S,E,N,D], [M,O,R,E], [M,O,N,E,Y]),
  S = 9,
  E = 5,
  N = 6,
  D = 7,
  M = 1,
  O = 0,
  R = 8,
  Y = 2.

test(itsme) :-
  E = 0,
  I = 1,
  M = 3,
  S = 8,
  T = 2.

:-end_tests(question2).

% question 3
subsetsum(L, N) :-
  same_length(L, Coefficients),
  Coefficients ins 0..1,
  maplist(applyCoeff, L, Coefficients, Output),
  sum(Output, #=, N),
  label(Coefficients).

applyCoeff(Coeff, X, Y) :- Y #= Coeff * X.

% question 4
paper(1,lily,xxx,ai).
paper(2,peter,john,database).
paper(3,ann,xxx,theory).
paper(4,ken,lily,network).
paper(5,kris,xxx,games).

reviewer(lily,theory,network).
reviewer(john,ai,theory).
reviewer(peter,database,network).
reviewer(ann,theory,network).
reviewer(kris,theory,games).
reviewer(ken,database,games).
reviewer(bill,database,ai).
reviewer(jim,theory,games).


workLoadAtMost(2).

%assign(W1,W2) :-
%  [Reviewer1 | RestR1] = W1,
%  [Reviewer2 | RestR2] = W2,
%  paper(Index, Author1, Author2, Subject),
%  % author cannot be own reviwer
%  Reviewer1 #\= Author1,
%  Reviewer2 #\= Author1,
%  Reviewer1 #\= Author2,
%  Reviewer2 #\= Author2,
%  % reviewer must match subject area
%  reviewer(Reviewer1, Subject11, Subject12),
%  reviewer(Reviewer2, Subject21, Subject22),
%    (Subject11 #= Subject, Subject21 #= Subject);
%    (Subject11 #= Subject, Subject22 #= Subject);
%    (Subject12 #= Subject, Subject21 #= Subject);
%    (Subject12 #= Subject, Subject22 #= Subject);
%  % each paper assigned to 2 reviewers
%  Reviewer1 #\= Reviewer2,
%  % no reviewer assigned more than k papers
%  workLoadAtMost(Max),
%  check_max_reviews(W1, W2, Max),
%  assign(RestR1, RestR2),
%  label(W1),
%  label(W2).
%
%check_max_reviews(W1, W2, Max, Reviewer) :-
%  append(W1, W2, W),
%  countoccurences(W, Reviewer, 0, Max).
%
%
countoccurences([], _, Count, Max) :-
  Count #=< Max.

countoccurences([H | T], H, Count, Max) :-
  countoccurences(T, H, Count + 1, Max).

countoccurences([H | T], Reviewer, Count, Max) :-
  H #\= Reviewer,
  countoccurences(T, Reviewer, Count, Max).


% get the domain for reviewers 

assignHelper(W1, W2) :-
  same_length(W1, W2),
  count_reviewers(NumReviewers),
  R1 ins 1..NumReviewers,
  R2 ins 1..NumReviewers,
  maplist(constrain_workload(R1, R2
  label(R1),
  label(R2),
  maplist(nth1(ReviewerIndex, R1, ), W1)
  



count_reviewers(Count) :-
  aggregate_all(count, paper(_,_,_,_), Count).

constrain_workload(R1, R2, Reviewer) :-
  workLoadAtMost(Max),
  append(R1, R2, R),
  countoccurences(R, Reviewer, 0, Max).

constrain_workloads(R1, R2) :-
  append(R1, R2, R),
  list_to_set(R, [R1 | Rs]),
  constrain_workload([R1 | Rs], 

