 solucio( SOL ) :- 
    SOL = [ [1,_C1,_Pr1,_A1,_B1,_Pa1],  % numcasa, color, professio, animal, beguda, pais
            [2,_C2,_Pr2,_A2,_B2,_Pa2],
            [3,_C3,_Pr3,_A3,_B3,_Pa3],  
            [4,_C4,_Pr4,_A4,_B4,_Pa4],
            [5,_C5,_Pr5,_A5,_B5,_Pa5] ],
     member( [_,vermell,_,_,_,peru] , SOL ),      % 1 - El que vive en la casa roja es de Peru
     member( [_,_,_,perro,_,francia] , SOL ),  % 2 - Al franc ́es le gusta el perro
     member( [_,_,pintor,_,_,japon] , SOL ),     % 3 - El pintor es japones
     member( [_,_,_,_,ron,china] , SOL ),        % 4 - Al chino le gusta el ron
     member( [1,_,_,_,_,hungria] , SOL ),        % 5 - El hungaro vive en la primera casa
     member( [_,verde,_,_,conac,_] , SOL ),      % 6 - Al de la casa verde le gusta el conac
     member( [NV,verde,_,_,_,_] , SOL ),
     member( [NB,blanca,_,_,_,_] , SOL ),
     NB is NV+1,                                 % 7 - La casa verde esta justo a la izquierda de la blanca
     member( [_,_,escultor,caracol,_,_] , SOL),  % 8 - El escultor cria caracoles
     member( [_,amarilla,actor,_,_,_] , SOL ),   % 9 - El de la casa amarilla es actor
     member( [3,_,_,_,cava,_] , SOL ),           % 10 - El de la tercera casa bebe cava
     member( [NA,_,actor,_,_,_] , SOL ),
     member( [NC,_,_,caballo,_,_] , SOL ), 
     1 is abs(NA-NC),                            % 11 - El que vive al lado del actor tiene un caballo
     member( [NAzul,azul,_,_,_,_] , SOL ),
     member( [NH,_,_,_,_,hungria] , SOL),
     1 is abs(NAzul-NH),                         % 12 - El hungaro vive al lado de la casa azul
     member( [_,_,notario,_,whisky,_] , SOL),    % 13 - Al notario la gusta el whisky
     member( [NM,_,medico,_,_,_] , SOL ),
     member( [NArd,_,_,ardilla,_,_] , SOL),
     1 is abs(NM-NArd),                          % 14 - El que vive al lado del medico tiene un ardilla
     writeSol(SOL).

writeSol(Sol):-
        member(Casa,Sol), write(Casa), nl, fail.
writeSol(_).
    


    

