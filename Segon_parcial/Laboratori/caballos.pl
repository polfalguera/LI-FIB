main:- EstadoInicial = [1,1],     EstadoFinal = [8,8],
    between(1,1000,CosteMax),            % Buscamos soluciÃ³n de coste 0; si no, de 1, etc.
    camino( CosteMax, EstadoInicial, EstadoFinal, [EstadoInicial], Camino ),
    reverse(Camino, Camino1), write(Camino1), write(' con coste '), write(CosteMax), nl, halt.

camino( 0, E,E, C,C ).              % Caso base: cuando el estado actual es el estado final.
camino( CosteMax, EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ):-
    CosteMax>0,
    unPaso( CostePaso, EstadoActual, EstadoSiguiente ),  % En B.1 y B.2, CostePaso es 1.
    \+member( EstadoSiguiente, CaminoHastaAhora ),
    CosteMax1 is CosteMax-CostePaso,
    camino( CosteMax1, EstadoSiguiente, EstadoFinal, [EstadoSiguiente|CaminoHastaAhora], CaminoTotal ).

unPaso(1, [Filai,Columnai], [Filaj,Columnaj] ) :- between(1,8,Filaj), between(1,8,Columnaj), (Filaj is Filai + 1 ; Filaj is Filai - 1), (Columnaj is Columnai + 2 ; Columnaj is Columnai - 2).
unPaso(1, [Filai,Columnai], [Filaj,Columnaj] ) :- between(1,8,Filaj), between(1,8,Columnaj), (Filaj is Filai + 2 ; Filaj is Filai - 2), (Columnaj is Columnai + 1 ; Columnaj is Columnai - 1).

