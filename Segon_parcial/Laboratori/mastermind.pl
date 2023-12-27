checkE([],[],0).
checkE([I|Codigo],[I|Intento],E):-
    checkE(Codigo,Intento,E1),
    E is E1 + 1.
checkE([_|Codigo],[_|Intento],E):-
    checkE(Codigo,Intento,E).

checkD(_,_,[],0).

checkD(Codigo,[I|RestoCodigo],[I|Intento],D):-
    checkD(Codigo,RestoCodigo,Intento,D).

checkD(Codigo,[_|RestoCodigo],[I|Intento],D):-
    member(I,Codigo),
    checkD(Codigo,RestoCodigo,Intento,D1),
    D is D1 + 1.

checkD(Codigo,[_|RestoCodigo],[_|Intento],D):-
    checkD(Codigo,RestoCodigo,Intento,D).

                    %%% TOT LO DE ADALT ES POR UNIFICAR EN UN ÚNIC PREDICAT %%%
            
% calcula_ED(Codigo,[C|Codi],[I|Int],E,D):- C = I, !, calcula_ED(Codigo,Codi,Int,E2,D), E is E2 + 1.            %
% calcula_ED(Codigo,[_|Codi],[I|Int],E,D):- member(I,Codigo), !, calcula_ED(Codigo,Codi,Int,E,D2), D is D2 + 1. %
% calcula_ED(Codigo,[_|Codi],[_|Int],E,D):- calcula_ED(Codigo,Codi,Int,E,D).                                    %

                    %%% TOT LO DE ADALT ES POR UNIFICAR EN UN ÚNIC PREDICAT %%%

respuesta(Codigo,Intento,E,D):-
    checkE(Codigo,Intento,E),
    checkD(Codigo,Codigo,Intento,D).

intentos([ [ [r,a,v,l], [1,1] ], [ [m,n,v,l], [1,0] ], [ [v,l,v,l], [0,0] ], [ [r,a,m,m], [1,1] ], [ [r,n,a,n], [2,2] ]]).

variables(Vars):- Vars = [r,a,v,l,n,m].

nuevoIntento(A):-
    A = [A1,A2,A3,A4],
    variables(Vars),
    member(A1,Vars),
    member(A2,Vars),
    member(A3,Vars),
    member(A4,Vars),
    intentos(Intentos),
    checkCode(A,Intentos).

checkCode(_,[]).
checkCode(A,[[I,[E,D]]|Intentos]):- respuesta(A,I,E,D), checkCode(A,Intentos).