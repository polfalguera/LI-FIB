main:- EstadoInicial = [3,3,l], EstadoFinal = [0,0,r],
    between(1,1000,CosteMax), % Buscamos soluci ́on de coste 0; si no, de 1, etc.
    camino( CosteMax, EstadoInicial, EstadoFinal, [EstadoInicial], Camino ),
    reverse(Camino,Camino1), write(Camino1), write(' con coste '), write(CosteMax), nl, halt.
    camino( 0, E,E, C,C ). % Caso base: cuando el estado actual es el estado final.

camino( CosteMax, EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ):-
    CosteMax>0,
    unPaso( CostePaso, EstadoActual, EstadoSiguiente ), % En B.1 y B.2, CostePaso es 1.
    \+member( EstadoSiguiente, CaminoHastaAhora ),
    CosteMax1 is CosteMax-CostePaso,
    camino(CosteMax1,EstadoSiguiente,EstadoFinal, [EstadoSiguiente|CaminoHastaAhora], CaminoTotal).

%% predicat que reuneix TOTS els moviments => Cost màxim 2⁵ = 32 :: [misio esq, canb esq, barca]
%% observem que hi ha dos casos únics: o be que el # de M i C es igual a cada costat o be que a left M = 3 o right M = 3.

unPaso(1, [NM0,NC0,l], [NM1,NC1,r]) :-
    between(0,NM0,M), between(0,NC0,C),
    (1 is M+C; 2 is M+C), NM1 is NM0-M, NC1 is NC0-C, (0 is NC1; 0 is NM1; NM1 >= NC1), 
    NM2 is 3-NM0+M, NC2 is 3-NC0+C, (0 is NC2; 0 is NM2; NM2 >= NC2).

unPaso(1, [NM0,NC0,r], [NM1,NC1,l]) :-
    NM is 3-NM0, NC is 3-NC0,
    between(0,NM,M), between(0, NC,C),
    (1 is M+C; 2 is M+C), NM1 is NM0+M, NC1 is NC0+C, (0 is NC1; 0 is NM1; NM1 >= NC1), 
    NM2 is 3-NM0-M, NC2 is 3-NC0-C, (0 is NC2; 0 is NM2; NM2 >= NC2).
