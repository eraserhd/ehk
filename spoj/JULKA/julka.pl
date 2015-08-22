julka(T,M,K,N) :-
  N is (T-M) div 2,
  K is N+M.

p(0) :- halt.
p(C) :-
  readln([T]),
  readln([M]),
  julka(T,M,K,N),
  writeln(K),
  writeln(N),
  C1 is C-1,
  p(C1).

program :- p(10).

%% vim:set ft=prolog:
