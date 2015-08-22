d(84). d(68). d(76). d(70).
s(X,N):-d(X)->N=2;N=1.
k([],1).
k([C|R],N):-k(R,X),s(C,H),N is X*H.
p(0):-!.
p(N):-readln([W]),atom_codes(W,K),k(K,C),writeln(C),M is N-1,p(M).
program:-p(10).
