% you can start using # operators in your prolog program,

factorial(0, 1).
factorial(N, F) :-
    N #> 0,
    N1 #= N - 1,
    F #= N * F1,
    factorial(N1, F1).

/*  you can query 

?- factorial(10,F).
F = 3628800

?- factorial(N,3628800).
N = 10