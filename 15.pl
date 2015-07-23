%        a
%       b c
%      d e f
%     g h i j
%    k l m n o

row([a, b, d, g, k]).
row([c, e, h, l]).
row([f, i, m]).
row([a, c, f, j, o]).
row([b, e, i, n]).
row([d, h, m]).
row([d, e, f]).
row([g, h, i, j]).
row([k, l, m, n, o]).

adjacent([A, B, C]) :-
  row(Row),
  (  append([_, [A,B,C], _], Row)
  ;  append([_, [C,B,A], _], Row)
  ).

jump([A, B], Pegs, NewPegs) :-
  adjacent([A, B, C]),
  member(A, Pegs),
  member(B, Pegs),
  not(member(C, Pegs)),
  select(B, Pegs, NewPegs1),
  select(A, NewPegs1, NewPegs2),
  append([C], NewPegs2, NewPegs).

start([b, c, d, e, f, g, h, i, j, k, l, m, n, o]).

solve(Pegs, Moves) :-
  (  length(Pegs, 1) -> Moves = []
  ;  jump([A, B], Pegs, NextPegs),
     solve(NextPegs, NextMoves),
     append([[A, B]], NextMoves, Moves)
  ).
