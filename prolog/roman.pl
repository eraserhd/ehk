digit(i,1).
digit(v,5).
digit(x,10).
digit(l,50).
digit(c,100).
digit(d,500).
digit(m,1000).

upto(0,_,_) --> [].
upto(N,C,D) --> { C>0, C1 is C-1 }, [D], upto(N1,C1,D), { digit(D,V), N is N1+V }.

less(N,I,V) --> [I, V], { digit(I,IV), digit(V,VV), N is VV-IV }.

part(N,_,V,I) --> upto(N1,1,V), upto(N2,3,I), { N is N1+N2 }.
part(N,_,V,I) --> less(N,I,V).
part(N,X,_,I) --> less(N,I,X).

numeral(N) -->
  upto(A,3,m),
  part(B,m,d,c),
  part(C,c,l,x),
  part(D,x,v,i),
  { N is A+B+C+D }.

roman(N, R) :- phrase(numeral(N), R), !.

%% vim:set ft=prolog:
