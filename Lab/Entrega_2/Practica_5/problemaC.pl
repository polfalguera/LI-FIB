programa(P):- append(['begin'], InsEnd, P), append(Ins, ['end'], InsEnd), instrucciones(Ins), write('yes'), !.
programa(_):- write('no').

instrucciones(Ins):- \+ member(';', Ins), instruccion(Ins), !.
instrucciones(Ins):- append(I, [';'|RIns], Ins), instruccion(I), instrucciones(RIns), !.

instruccion([Res, '=', Var1, '+', Var2]):- variable(Res), variable(Var1), variable(Var2), !.
instruccion(['if'|Res]):- append(Body, ['endif'], Res), append([[Var1], ['='], [Var2], ['then'], InsIf, ['else'], InsElse], Body), variable(Var1), variable(Var2), instrucciones(InsIf), instrucciones(InsElse), !.

% Predicats útils %
variable('x').
variable('y').
variable('z').
% Predicats útils %