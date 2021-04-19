/* Taken from swi Prolog clpfd manual

 -Arithmetics constraints

 #=, #\=, #>, #<, #>=, #=<

 -Boolean operators

  #\/,  #/\,  #\,  #==>, #<==>

 -These operators can be reified (turn true/false into 1/0)

  Example.  

  ?- X=4, 1 #= 1 #==> D.
  X = 4,
  D = 1.

  ?- X #= 4 #<==> B, X #\= 4.
  B = 0,
  X in inf..3\/5..sup.

 -Global Constraints 
     These constraints have builtin constraint
     propagation, in general more efficient than user-defined
     counterpart. E.g., we can define 
*/

all_distinct0([]).
all_distinct0([_]).
all_distinct0([A|L]) :-
    differ(A,L),
    all_distinct0(L).
differ(_,[]).
differ(A,[B|L]) :-
    A #\= B,
    differ(A,L).

% tests - we know both goals below should fail. Try them.
% ?- d1(I) fails immediately, but ?- d0(I) takes a long time.
% Does arc-consistency help prune search space? No.

d0(L) :- length(L,100), L ins 1..99, all_distinct0(L), label(L).
d1(L) :- length(L,100), L ins 1..99, all_distinct(L), label(L).


/* Other Global Constraints

sum(+Xs, +RelOp, ?Value)

where Xs is a list of integers or domain variables, RelOp is a 
relational symbol and Value is an integer or a domain variable. 
True if sum(Xs) RelOp Value. Cannot be reified.

For example, sum(L,#>=,5) means the sum of the integers/domain 
variables in L is greater than or equal to 5.
*/


% An example of using reification:
% We want to find out how many elements
% of a list L satisfy a condition.

% test
t(I,Vars,N) :-
    length(Vars,4),
    Vars ins 1..10,    % some arbitrary domain
    occur(I,Vars,N),   % I occurs in Vars N times
    label(Vars).

occur(I,Vars,N) :-
    generate_list(I,Vars,L),
     % if an element in Vars equals I, put 1 into L;
     % otherwise 0 into L; then we can use sum/3, e.g.,
     % to count how many of L are 1
    sum(L,#=,N).

generate_list(_,[],[]).
generate_list(I,[A|R],[T|S]) :-
    (I #= A #==> T#=1),
    (I #\= A #==> T#=0),
       generate_list(I,R,S).

/*
  scalar_product(+Cs, +Vs, +Rel, ?Expr)

  True iff the scalar product of Cs and Vs is in relation 
  Rel to Expr. Cs is a list of integers, Vs is a list of 
  variables and integers. Rel is #=, #\=, #<, #>, #=< or #>=.
*/

% test

q1([A,B]) :-
    L = [A,B], L ins 5..6, scalar_product([1,2],L, #=, 17).
q2([A,B]) :-
    [A,B] ins 1..4, scalar_product([1,2,3],[2,A,B],#>,10),
    label([A,B]).

/* 
  global_cardinality(+Vs, +Pairs)

  Global Cardinality constraint. Vs is a list of finite domain 
  variables, Pairs is a list of Key-Num pairs, where Key is an 
  integer and Num is a finite domain variable. The constraint 
  holds iff each V in Vs is equal to some key, and for each 
  Key-Num pair in Pairs, the number of occurrences of Key in Vs 
  is Num. 
*/

q(Vs) :- Vs = [_,_,_,_],
	 global_cardinality(Vs, [1-2,3-_]), label(Vs).
/*
?- q(I).
I = [1, 1, 3, 3] ;
I = [1, 3, 1, 3] ;
I = [1, 3, 3, 1] ;
I = [3, 1, 1, 3] ;
I = [3, 1, 3, 1] ;
I = [3, 3, 1, 1].
*/

% enumerating all vectors of four of 0 or 1.
qq(Vs) :- Vs = [_,_,_,_], Vs ins 0..1, 
 	 global_cardinality(Vs, [0-_,1-_]), label(Vs).




