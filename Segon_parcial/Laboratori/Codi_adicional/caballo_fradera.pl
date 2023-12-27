% estat: [fila, columna]

main:- EstadoInicial = [1,1],     EstadoFinal = [8,8],
    between(1,1000,CosteMax),            % Buscamos solución de coste 0; si no, de 1, etc.
    camino( CosteMax, EstadoInicial, EstadoFinal, [EstadoInicial], Camino ),
    reverse(Camino, Camino1), write(Camino1), write(' con coste '), write(CosteMax), nl, halt.

camino( 0, E,E, C,C ).              % Caso base: cuando el estado actual es el estado final.
camino( CosteMax, EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ):-
    CosteMax>0,
    unPaso( CostePaso, EstadoActual, EstadoSiguiente ),  % En B.1 y B.2, CostePaso es 1.
    \+member( EstadoSiguiente, CaminoHastaAhora ),
    CosteMax1 is CosteMax-CostePaso,
    camino( CosteMax1, EstadoSiguiente, EstadoFinal, [EstadoSiguiente|CaminoHastaAhora], CaminoTotal ).

% transicions
unPaso(1,[X,Y], [X2,Y2]):- between(1,8,X2), between(1,8,Y2), (X2 is X+2 ; X2 is X-2), (Y2 is Y+1 ; Y2 is Y-1).
unPaso(1,[X,Y], [X2,Y2]):- between(1,8,X2), between(1,8,Y2), (X2 is X+1 ; X2 is X-1), (Y2 is Y+2 ; Y2 is Y-2).
