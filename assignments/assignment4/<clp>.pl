/*
Name: Arun Woosaree
Student Number: 1514457
Course: CMPUT 325
Section: B1
Assignment 4
*/

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
% encrypt(W1, W2, W3)
%
% This is a generalized cryptarithmetic puzzle solver
% This assumes W1 and W2 are of the same length
% The first part of the code (everything before the cut)
% and the Letters ins 0..9 part was given to us on eclass
%
% How it works:
% First, we get the length of the lists, and we also make a set
% of the letters. We make sure that all the letters are distinct.
% We restrict the domain of each letter to be within 0 and 9 inclusive, 
% since that's what we use for base10 arithmetic. 
% We also check that the lead letters are not zero.
% We then convert each list of letters to an actual number
% using the digits_to_number predicate defined below.
% Finally, we add the constraint that the
% first number plus the second number equals the third number
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
  encrypt([I,T], [I,S], [M,E]),
  E = 0,
  I = 1,
  M = 3,
  S = 8,
  T = 2.

:-end_tests(question2).
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

:- begin_tests(digits).
test(digits) :-
  digits_to_number([1,2,3,4,5,6,7,8,9,0], 1234567890).
:-end_tests(digits).
digits_to_number(Digits, Number) :-
  digits_to_number(Digits, 0, Number).
digits_to_number([], Sum, Sum).
digits_to_number([Digit | Rest], Sum, Number) :-
  PartialSum #= Sum*10 + Digit,
  digits_to_number(Rest, PartialSum, Number).


% Question 3
% subsetsum(L, N)
% This predicate takes in a list L, and an integer N,
% and it determines if there exists a subset of the list such that 
% the sum is equal to N
%
% How it works:
% We make a coefficients array, which is the same length as L
% The idea here is that if a subset sum exists, the equation for it
% is a sum of a bunch of unknown coefficients, multiplied by each element in L
% where each coefficient is either 0 or 1 is equal to N.
% We apply those coefficients, and then use clpfd to constain the
% coefficients and see if such a combination exists. 
:-begin_tests(subsetsum).
test(q1) :- subsetsum([-1, 1, 2, 3, 7,9,-2, -4],18).

test(q2) :- subsetsum([2062518654, 1772728182, 524053838, 3872006790, 4143885470,
                 473259798,  4214676334, 250497318, 217279934, 205333686],
                524053838).

test(q3) :- subsetsum([4029715910, 209644766, 137797718, 431825326, 873310310,
                 2232437246, 491614710, 644823502, 2747801862, 2767496478],
                1305135636).

test(q4) :- subsetsum([73126254, 682039078, 697267134, 2806596278, 2044349838,
           1818198982, 1432982238, 3234586198, 816273326, 312522342,
           4193226750, 4054437878, 138219470, 3100462854, 1779338014,
           411350934, 2579731950, 2628644262, 447849534, 3186898230],
                7450443154).

test(q5) :- \+ subsetsum([3794350174, 3146379734, 1029552430, 355507686, 3586069886,
                 1203202934,3984660814, 1176600198, 2343794846, 2612878102,
                 1893648238, 859170086, 4200167870, 3342557366, 1435820942,
                 3808719302, 2191669470, 4008123478, 2469367214],
                25424705316).

test(q6) :- \+ subsetsum([3829957942, 555555555,
                 802593294, 2471297606, 353176414, 1878866134,
                 1081248998, 2349383806,757580406, 380816462,
                 2374890398, 2600583702, 993315438,753136678,
                 1429919670, 160577166, 217518278, 2455720926,
                 3969905510, 3052126462, 4288070902, 937285838],
                19333806124).
:-end_tests(subsetsum).
subsetsum(L, N) :-
  same_length(L, Coefficients),
  Coefficients ins 0..1,
  maplist(applyCoeff, L, Coefficients, Output),
  sum(Output, #=, N),
  label(Coefficients).

% This predicate applied a coefficient to a number
% This is used in the subset sum problem,
% see above for a description of how it's used
applyCoeff(Coeff, X, Y) :- Y #= Coeff * X.

% Question 4
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
