%1. Relación elemento, nos dice si un elemento está en una lista

elemento(E,[E|_]).
elemento(E,[_|L]) :- elemento(E,L).

%2.



%3. Relación palindromo.

%Añade un elemento a una lista.
concatena(E,[],[E]).
concatena(E,[HEAD|T], [HEAD|R]) :- concatena(E,T,R). 

%Nos da la reversa de una lista
reversa([],[]).
reversa([E],[E]).
reversa([HEAD|T], R) :- reversa(T,L), concatena(HEAD,L,R).

palindromo(L,R) :- reversa(L,R).

%4.

%5.

%6.

%7.