main:- EstadoInicial = [0,0,0,0,l], EstadoFinal = [1,1,1,1,r],
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

unPaso(2, [0,0,P5,P8,l], [1,1,P5,P8,r]).
%unPaso(2, [P1,0,P5,P8,l], [P1,1,P5,P8,r]).

unPaso(5, [0,P2,0,P8,l], [1,P2,1,P8,r]).
unPaso(5, [P1,0,0,P8,l], [P1,1,1,P8,r]).
%unPaso(5, [P1,P2,0,P8,l], [P1,P2,1,P8,r]).

unPaso(8, [0,P2,P5,0,l], [1,P2,P5,1,r]).
unPaso(8, [P1,0,P5,0,l], [P1,1,P5,1,r]).
unPaso(8, [P1,P2,0,0,l], [P1,P2,1,1,r]).
%unPaso(8, [P1,P2,P5,0,l], [P1,P2,P5,1,r]).

unPaso(1, [1,P2,P5,P8,r], [0,P2,P5,P8,l]).
unPaso(2, [P1,1,P5,P8,r], [P1,0,P5,P8,l]).
%unPaso(5, [P1,P2,1,P8,r], [P1,P2,0,P8,l]).
%unPaso(8, [P1,P2,P5,1,r], [P1,P2,P5,0,l]).
