%respuesta(Codigo,Intento,E,D)

respuesta(Codigo,Intento,E,D):-
    calcula_ED(Codigo,Codigo,Intento,E,D).
    % calcula_E(Codigo,Intento,E),
    % calcula_D(Codigo,Codigo,Intento,D).

calcula_ED(_,[],[],0,0).
calcula_ED(Codigo,[C|Codi],[I|Int],E,D):- C = I, !, calcula_ED(Codigo,Codi,Int,E2,D), E is E2 + 1.
calcula_ED(Codigo,[_|Codi],[I|Int],E,D):- member(I,Codigo), !, calcula_ED(Codigo,Codi,Int,E,D2), D is D2 + 1.
calcula_ED(Codigo,[_|Codi],[_|Int],E,D):- calcula_ED(Codigo,Codi,Int,E,D).

intentos([ [ [r,a,v,l], [1,1] ], [ [m,n,v,l], [1,0] ], [ [v,l,v,l], [0,0] ], [ [r,a,m,m], [1,1] ], [ [r,n,a,n], [2,2] ]]).

variables(Vars):- Vars = [r,a,v,l,n,m].

nuevoIntento(A):-
    A = [A1,B1,C1,D1],
    intentos(Intentos),
    variables(Vars),
    member(A1,Vars),
    member(B1,Vars),
    member(C1,Vars),
    member(D1,Vars),
    comprova(A,Intentos).

comprova(_,[]).
comprova(A,[[I,[E,D]]|Intentos]):- respuesta(A,I,E,D), comprova(A,Intentos).



% calcula_E([],[],0).
% calcula_E([C|Codi],[I|Int],E):- C = I, !, calcula_E(Codi,Int,E2), E is E2 + 1.
% calcula_E([_|Codi],[_|Int],E):- calcula_E(Codi,Int,E).

% calcula_D(_,[],[],0).
% calcula_D(Codigo,[C|Codi],[I|Int],D):- C \= I, member(I,Codigo), !, calcula_D(Codigo,Codi,Int,D2), D is D2 + 1.
% calcula_D(Codigo,[C|Codi],[I|Int],D):- C \= I,  calcula_D(Codigo,Codi,Int,D).





