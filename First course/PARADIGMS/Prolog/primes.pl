prime(2) :- !.
prime(Input) :-
	Input > 2,
  1 is Input mod 2,
  SqrtP is sqrt(Input),
  is_prime_1(3, Input, SqrtP), !.

is_prime_1(I, _, SqrtP):-
  I > SqrtP, !.

is_prime_1(I, P, SqrtP):-
  P mod I > 0,
  Next is I + 1,
  is_prime_1(Next, P, SqrtP), !.

composite(Input) :-
	not(prime(Input)), !.



my_between(A, A, B):-A =< B.
my_between(X, A, B):-A < B, A1 is A + 1, my_between(X, A1, B).

prime_divisors(1, []).
prime_divisors(N, [X|Divisors]) :-
		number(N),
		my_between(X, 2, N),
		0 is N mod X,
		N1 is N/X,
		prime_divisors(N1, Divisors), !.

empty(X, [H | T]) :- X =< H.
empty(X, []).

prime_divisors(N, [H | T]) :-
  not(number(N)),
  empty(H, T),
  prime_divisors(N1, T),
  N is H * N1.


my_func(IN, OUT, VAL) :-
		not(0 is IN mod OUT),
		VAL is IN.

my_func(IN, OUT, VAL) :-
		0 is IN mod OUT,
		VAL1 is IN / OUT,
		my_func(VAL1, OUT, WTF),
		VAL is WTF.


unique_prime_divisors(1, []).
unique_prime_divisors(N, [X|Divisors]) :-
		my_between(X, 2, N),
		0 is N mod X,
		my_func(N, X, VAL),
		N1 is VAL,
		unique_prime_divisors(N1, Divisors), !.



