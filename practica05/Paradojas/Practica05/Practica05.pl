/*
*Nombre del equipo: Paradojas*
--Liprandi Cortes Rodrigo 317275605
--Tinoco Miguel Laura Itzel 316020189
-- *Práctica 04*
*/

%1.- Relación elemento, nos dice si un elemento está en una lista

elemento(E,[E|_]).
elemento(E,[_|L]) :- elemento(E,L).

%2. -Relación suma, nos devuelve la suma de los elementos de una lista

suma([], 0).
suma([X|Xs], S):- 
	suma(Xs, S2),
	S is S2 + X.


%3.- Relación palindromo.

%Añade un elemento a una lista.
concatena(E,[],[E]).
concatena(E,[HEAD|T], [HEAD|R]) :- concatena(E,T,R). 

%Nos da la reversa de una lista
reversa([],[]).
reversa([E],[E]).
reversa([HEAD|T], R) :- reversa(T,L), concatena(HEAD,L,R).

%Checa que la reversa de la lista sea igual a la lista.
palindromo(L,R) :- reversa(L,R).

%4.- Relación ocurrir, devuelve el numero de ocurrencias de un elemento en una lista

ocurrir([], _E, 0).
ocurrir([X|Xs], E, N):-
	X == E, !, ocurrir(Xs, E, N1),
	N is N1 + 1.
	
% si E es diferente de X
ocurrir([_X|Xs], E, N):- ocurrir(Xs, E, N).

%5.- Implementa cada una de las propiedades de relaciones de equivalencia sobre el mundo de los gatos.

%nuestro mundo de gatos
gato(salem).
gato(kevin).
gato(lince).

%definimos quienes son amigos
son_amigos(salem,kevin).
son_amigos(lince,salem).

%definimos quien es mas grande
es_masgrande(salem,kevin).
es_masgrande(kevin,lince).

%reflexividad
ser(X,X) :- gato(X). 
%simetria
amigos(X,Y) :- gato(X), gato(Y), son_amigos(X,Y) ; son_amigos(Y,X). %checamos si x es amigo de y, o y es amigo de x.
%transitividad
masgrande(X,Y) :- gato(X), gato(Y), es_masgrande(X,Y) ; es_masgrande(X,Z) , es_masgrande(Z,Y). %checamos si x es mas grande que y o si x es mas grande que alguien y si ese alguien es mas chico que x

%6.- Relación in_order que devuelve una lista con el recorrido in-order de un árbol binario
%Definimos la relación árbol binario
%arbol vacio = null
arbolB(null).
arbolB(nodo(Iz, _R, Dr)) :- arbolB(Iz), arbolB(Dr).

%Definimos la relación concatenaL la cual concatena dos listas.
concatenaL([],Ys,Ys).
concatenaL([X|Xs],Ys,[X|Zs]) :- concatenaL(Xs,Ys,Zs).

in_order(null,[]).
in_order(nodo(I,R,D), IRD):- 
		in_order(I, Ini),
		in_order(D, Ind),
		concatenaL(Ini, [R|Ind], IRD).


%7.- Implementa los hechos que representen a cada integrante y las reglas familiares necesarias para poder preguntar por
%    la relacion ’abuelo’.

personaje(autor).
personaje(viuda).
personaje(padre_autor).
personaje(hija_viuda).
personaje(nina).
personaje(nino).

es_padre(autor,padre_autor). %Padre del autor es su padre
es_padre(hija_viuda, autor). %Padre de la hija de la viuda es el autor
es_padre(nino, autor). %Padre del hijo del autor es el autor
es_padre(padre_autor, autor). %Padre del padre del autor es el autor
es_madre(hija_viuda, viuda). %Madre de la hija de la viuda es la viuda
es_madre(nina,hija_viuda). %La madre de la hija del padre del autor es la hija de la viuda
es_madre(autor, hija_viuda). %La madre del autor es la hija de la viuda
es_madre(nino, viuda). %La madre del hijo del autor es la viuda

padre(X,Y) :- personaje(X), personaje(Y), es_padre(X,Y). %Nos devuelve el padre de alguien
madre(X,Y) :- personaje(X), personaje(Y), es_madre(X,Y). %Nos devuelve la madre de alguien

ancestro(X,Y) :- personaje(X), personaje(Y), padre(X,Y) ; madre(X,Y). %Nos da el padre o la madre de alguien.

abuelo(X,Y) :- personaje(X), personaje(Y), ancestro(X,Z), ancestro(Z,Y). %El abuelo/a de alguien.
