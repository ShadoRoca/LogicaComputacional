/*
*Nombre del equipo: Paradojas*
--Liprandi Cortes Rodrigo 317275605
--Tinoco Miguel Laura Itzel 316020189
-- *Práctica 06*
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
%definimos el mundo de los cubos
cubo(azul).
cubo(rojo).
cubo(gris).
cubo(verde).
cubo(amarillo).
cubo(morado).
sobre(gris,rojo).
sobre(rojo,azul).
sobre(amarillo,morado).

%definimos las reglas
hastaArriba(X) :- not(bloqueado(X)).
bloqueado(X) :- sobre(_,X).
hastaAbajo(X) :- not(sobre(X,_)).
mover(X,Y) :- not(bloqueado(X)), hastaArriba(Y).

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

% 4.- Relación Mezclar, mezcla dos listas previamente ordendas.
mezclar(L1,L2,L) :- ordenar(L1,X), ordenar(L2,Y), mezclarAux(X,Y,L).

%relacion auxiliar para mezclar
mezclarAux([],B,B).
mezclarAux(A,[],A).
mezclarAux([A|As], [B|Bs], [A|Rs]) :- A =< B, mezclarAux(As, [B|Bs], Rs),!.
mezclarAux([A|As], [B|Bs], [B|Rs]) :- mezclarAux([A|As], Bs, Rs).

%relación ordenar que ordena por insercion
ordenar([],[]).
ordenar([X|Xs],Ys) :- ordenar(Xs,Xs0), inserta(X,Xs0,Ys).

%relación auxiliar inserta un elemento en una lista ordenada.
inserta(X,[],[X]).
inserta(X,[Y|Ys],[X,Y|Ys]) :- X < Y.
inserta(X,[Y|Ys],[Y|Zs]) :- X >= Y, inserta(X,Ys,Zs).
