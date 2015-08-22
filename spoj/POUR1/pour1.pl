:- dynamic seen/2.

readn(N, [N|Buffer], Buffer).
readn(N, [], Buffer) :-
  readln([N|Buffer]).

queue(q(X, X)).
push(E, q(X, [E | NewLast]), q(X, NewLast)).
pop(E, q([E | NewX], Last), q(NewX, Last)).
empty(q(X, Y)) :-
  X == Y.

spush(state(N,A,B), InQ, OutQ) :-
  ( seen(A,B) -> InQ = OutQ
  ; asserta(seen(A,B)),
    push(state(N,A,B), InQ, OutQ)
  ).

transfer_amount(_, B, A1, B1, A2, B2) :-
  X is min(A1, B - B1),
  A2 is A1 - X,
  B2 is B1 + X.

pour(_, _, _, Q, -1) :-
  empty(Q),
  !.
pour(_, _, C, Q, N) :-
  pop(state(N, A, B), Q, _),
  ( A = C ; B = C ),
  !.
pour(A, B, C, Q, N) :-
  pop(state(N1, A1, B1), Q, Q1),
  N2 is N1 + 1,
  spush(state(N2, A, B1), Q1, Q2),
  spush(state(N2, A1, B), Q2, Q3),
  spush(state(N2, 0, B1), Q3, Q4),
  spush(state(N2, A1, 0), Q4, Q5),
  transfer_amount(A, B, A1, B1, A2, B2),
  spush(state(N2, A2, B2), Q5, Q6),
  transfer_amount(B, A, B1, A1, B3, A3),
  spush(state(N2, A3, B3), Q6, Q7),
  pour(A, B, C, Q7, N).
pour(A, B, C, N) :-
  retractall(seen(_, _)),
  asserta(seen(0, 0)),
  queue(Q),
  push(state(0, 0, 0), Q, Q1),
  pour(A, B, C, Q1, N).

program(0, _) :- halt.
program(Count, Buffer) :-
  readn(A, Buffer, Buffer1),
  readn(B, Buffer1, Buffer2),
  readn(C, Buffer2, Buffer3),
  pour(A, B, C, N),
  write(N), !,
  nl,
  Count1 is Count - 1,
  program(Count1, Buffer3).

program :-
  prompt1(''),
  readn(Count, [], Buffer),
  program(Count, Buffer).

%% vim:set ft=prolog:
