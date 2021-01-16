/*

*/

% 1.- Relación Traducir. 

%hacemos el diccionario
dicc(buddy,amigo).
dicc(you, tu).
dicc(are, eres).
dicc(a, un).
dicc(boy, chico).
dicc(make, has).
dicc(big, gran).
dicc(noise, ruido).
dicc(playing, jugando).
dicc(in, en).
dicc(the, la).
dicc(street, calle).
dicc(gonna, va_a).
dicc(be, ser).
dicc(man, hombre).
dicc(someday,algun_dia).
dicc(got, tienes).
dicc(mud, lodo).
dicc(on, en).
dicc(your, tu).
dicc(face, cara).
dicc(disgrace, desgracia).

%Auxiliar, concatena dos listas
append([], List, List).
append([H|T], List, [H|R]) :- append(T, List, R).

traducir([],[]).
traducir([H|T], L) :- dicc(H,X), append([X],L1,L), traducir(T,L1).


% 2.- Mundo de los cubos.


% 3.- AFND.

ei(q0). %estado inicial
ef(q0). %estado final

%transiciones del automata
delta(q0, a, q1).
delta(q1, b, q2).
delta(q2, a, q3).
delta(q2, a, q4).
delta(q2, a, q0).
delta(q4, a, q3).
delta(q3, a, q0).

%funcion auxiliar 
transicion(E, []) :- ef(E).
transicion(E,[H|T]) :- delta(E,H,EA), transicion(EA,T).

aceptar(S) :- transicion(q0,S).

% 4.- Relación Mezclar.