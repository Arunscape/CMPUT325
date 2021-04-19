% Solution to Winter 2021, Assigment 4, Question 4
% by Ifaz Kabir

:- use_module(library(clpfd)).

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

% DO NOT DISTRIBUTE OR SHARE

%%%%%%%%%%%%%%%%%%%%
% Overall Solution %
%%%%%%%%%%%%%%%%%%%%
% We are trying to write a predicate assign(W1,W2) such that
% W1 and W2 are reviewers of the papers, in paper order.
% Satisfying the constraints that
% 1. Reviewers can only review papers they have expertise in.
% 2. Reviewers cannot review papers they wrote.
% 3. Reviewers cannot review a paper twice.
% 4. No reviewer is assigned more papers than the workload

% We will solve the problem using CLPFD, but we are given reviewer names instead
% of numbers. So we must convert reviewer names into numbers. We will create a
% list [[0,[reviewer0,subjects]],[0,[reviewer1,subjects]],...], so that we can
% easily convert between reviewers and numbers. In the following code, we will
% refer to the reviewer number as "reviewer index".

% Also, we are given a workload K, so no reviewer index should show up more than
% K times. We will adapt our solution so that we can use all_distinct to solve
% the problem. Given a list of variables R, the all_distinct(R) ensures that
% numbers appear at most once. We can use this for the workload by representing
% reviewers with K different numbers. So if K = 3, and we have reviewer indexes
% 0,...,9, we will use the numbers 0,...,29 to represent reviewers. Reviewer 0
% will be represented by 0,10,20, Reviewer 1 will be 1,11,21, etc. Now using
% all_distinct will ensure that a reviewer can show up multiple times in our
% solution but not more than K times. In the code, you'll see this as "extended
% reviewer index".

%%%%%%%%%%%%%%%%%%%%
% Helper functions %
%%%%%%%%%%%%%%%%%%%%
dom_union(N,D,D\/N).

numbers_to_dom([X],X).
numbers_to_dom([X|Xs],X\/D) :- numbers_to_dom(Xs,D).

notin_list(_,[]).
notin_list(V,[X|Xs]) :- V #\= X, notin_list(V,Xs).

notins_list([],_).
notins_list([V|Vs],Xs) :- notin_list(V,Xs), notins_list(Vs,Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%
% Indexed Reviewer List %
%%%%%%%%%%%%%%%%%%%%%%%%%

% Takes a list [X0,X1,X2,...] and produces [[0,X0],[1,X1],[2,X2],...]
index_list([],_,[]).
index_list([X|Xs],N,[[N,X]|NXs]) :- N1 is N + 1, index_list(Xs,N1,NXs).

% Produces the list of reviewers: [[0,[reviewer0,subjects]],[0,[reviewer1,subjects]],...]
reviewers(L,RN) :-
    findall([R,[A1,A2]], reviewer(R,A1,A2), Rs),
    index_list(Rs,0,L), length(L,RN).

%%%%%%%%%%%%%%%%%%%%%%%%%
% Conversion Predicates %
%%%%%%%%%%%%%%%%%%%%%%%%%
%------------------------------%
% Reviewers for a subject area %
%------------------------------%
% Takes a subject area A, and converts it into a list of reviewer indexes RIs using the reviewer list RL
areaReviewerIndex(A,RL,RIs) :-
    findall(N,(member([N,[_,RA]],RL),member(A,RA)),RIs).

% Takes a subject area A, and converts it into a domain of reviewer indexes D using the reviewer list RL
areaReviewerDom(A,RL,D) :-
    areaReviewerIndex(A,RL,RIs),
    numbers_to_dom(RIs,D).

%---------------------------%
% Authors to Reviewer Index %
%---------------------------%
% Convert a list of authors into reviewer indexes
% If an author is not a reviewer, they are skipped
authorReviewerIndexes(Aus, RL, RIs) :-
    findall(RI, (member(Au,Aus), member([RI,[Au,_]],RL)), RIs).

%---------------------------------%
% Reviewer Index to Reviewer Name %
%---------------------------------%
index_to_reviewer(RI, RL, R) :-
    member([RI,[R,_]], RL), !.

% Extended Reviewer Index to Reviewer.
eRI_to_R(RL,RN,ERI,R) :-
    RI is ERI mod RN,
    member([RI,[R,_]], RL), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%
% Constraints Predicates %
%%%%%%%%%%%%%%%%%%%%%%%%%%

% Given a paper index PI, constraint (non-extended) reviewer index RI to a
% domain of non-author reviewers that have expertise in the paper's subject
% area.
paperReviewerConstraints(PI,RL,RI) :-
    paper(PI,Au1,Au2,A), !,
    areaReviewerDom(A,RL,D),
    authorReviewerIndexes([Au1,Au2],RL,ARIs),
    ....
% Note: The above is not the fastest solution since we are using reviewer index
% instead of extended reviewer index for constraints

% paperERIConstraints(ERIs1,ERIs2,RL,RN,1) will go through the list of variables
% ERIs1 and ERIs2 in lock-step, and make sure that the same reviewer has the
% reviewer expertise, does not review the a paper twice, and is not an author.
paperERIConstraints([],[],_,_,_).
paperERIConstraints([ERI1|ERIs1],[ERI2|ERIs2],RL,RN,PI) :-
    RI1 #= ERI1 mod RN,
    RI2 #= ERI2 mod RN,
    ...,
    paperReviewerConstraints(PI,RL,RI1), % can be made faster
    paperReviewerConstraints(PI,RL,RI2), % can be made faster
    PI1 is PI + 1,
    paperERIConstraints(ERIs1,ERIs2,RL,RN,PI1).


%%%%%%%%%%%%
% Solution %
%%%%%%%%%%%%
assign_ERIs(ERIs1,ERIs2,RL,RN) :-
    workLoadAtMost(WL),
    findall(PI, paper(PI,_,_,_), Ps),
    length(Ps,N),
    reviewers(RL,RN),
    !,
    ERIN is (WL * RN) - 1,
    length(ERIs1,N),
    length(ERIs2,N),
    append(ERIs1,ERIs2,ERIs),
    ERIs ins 0..ERIN,
    all_distinct(ERIs),
    paperERIConstraints(ERIs1,ERIs2,RL,RN,1), !.


assign(W1,W2) :-
    assign_ERIs(E1,E2,RL,RN),
    append(E1,E2,E), !,
    label(E),
    maplist(eRI_to_R(RL,RN), E1, W1),
    maplist(eRI_to_R(RL,RN), E2, W2), !.

test1() :- statistics(runtime,[T0|_]), assign(W1,W2),
        statistics(runtime, [T1|_]), T is T1 - T0, format('test took ~3d sec.~n', [T]), !.

% DO NOT DISTRIBUTE OR SHARE
