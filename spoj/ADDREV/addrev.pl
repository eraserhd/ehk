
code_digits([], []).
code_digits([C|Codes], [D|Digits]) :-
  number_codes(D, [C]),
  code_digits(Codes, Digits).

digits(N, Digits) :-
  ( var(N) ->
     code_digits(Codes, Digits),
     number_codes(N, Codes)
  ; number_codes(N, Codes),
    code_digits(Codes, Digits)
  ).

reverse_added([], [], 0, []) :- !.
reverse_added([], [], Carry, [C|RestC]) :-
  C is Carry mod 10,
  Carry1 is Carry div 10,
  reverse_added([], [], Carry1, RestC).
reverse_added([A|RestA], [B|RestB], Carry, [C|RestC]) :-
  X is Carry + A + B,
  C is X mod 10,
  Carry1 is X div 10,
  reverse_added(RestA, RestB, Carry1, RestC).
reverse_added([], [B|RestB], Carry, C) :-
  reverse_added([0], [B|RestB], Carry, C).
reverse_added([A|RestA], [], Carry, C) :-
  reverse_added([A|RestA], [0], Carry, C).
reverse_added(A, B, C) :-
  digits(A, ADigits),
  digits(B, BDigits),
  reverse_added(ADigits, BDigits, 0, CDigits),
  digits(C, CDigits).

program(0) :- halt.
program(Count) :-
  readln([A, B]),
  reverse_added(A, B, C),
  write(C),
  nl,
  Count1 is Count - 1,
  program(Count1).

program :-
  readln([Count]),
  program(Count).

%% vim:set ft=prolog:
