
symbolicOutput(0).  % set to 1 for DEBUGGING: to see symbolic output only; 0 otherwise.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% We want to schedule a series of events within the next few days.
%% Each event should have a moderator assigned to it. There is a limit
%% on the number of events per day and also on the amount of days
%% where events with the same moderator can take place. Finally, we
%% have a list of possible moderators and days for every event.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%% begin input %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

numEvents(15).
numDays(4).
numModerators(4).
maxEventsPerDay(4).
maxDaysPerModerator(2).

%event( id, listPossibleModerators, listPossibleDays).
event(1, [  2  ,4],[  2    ]).
event(2, [1,  3  ],[1,2,  4]).
event(3, [1,2  ,4],[    3  ]).
event(4, [1,  3,4],[1,2,  4]).
event(5, [  2,3,4],[1,  3,4]).
event(6, [    3,4],[1,2,3  ]).
event(7, [1,2    ],[1,2,  4]).
event(8, [1,2  ,4],[    3,4]).
event(9, [  2,3  ],[1,2,3  ]).
event(10,[    3,4],[1,2,  4]).
event(11,[1,2  ,4],[1,2,3  ]).
event(12,[  2,3  ],[    3,4]).
event(13,[1,  3,4],[1,2    ]).
event(14,[  2,3  ],[1,  3,4]).
event(15,[  2  ,4],[  2,3  ]).

%%%%%%% end input %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%% Some helpful definitions to make the code cleaner: ====================================

% Tot això són llistes.

event(E):-             event(E,_,_).
eventModerators(E,M):- event(E,M,_).
eventDays(E,D):-       event(E,_,D).
day(D):-               numDays(N), between(1,N,D).
moderator(M):-         numModerators(N), between(1,N,M).

%%%%%%% End helpful definitions ===============================================================


%%%%%%%  1. SAT Variables: ====================================================================

satVariable( ed(E,D) ):- event(E), day(D). % "Event E takes places in day D."
satVariable( em(E,M) ):- event(E), moderator(M). % "Event E is moderated by moderator M."
satVariable( md(M,D) ):- moderator(M), day(D). % "Moderator M works on day D."

%%%%%%%  2. Clause generation for the SAT solver: =============================================

writeClauses:-  
    eachEventExactlyOneModerator,     % Cada event té exactament un moredaror.
    eachEventExactlyOneDay,           % Cada event té lloc exactament un dia.
    maxEventsDay,                     % Nombre màxim d'events en un dia.
    maxsDaysModerator,                % Nombre màxims de dies que treballa un moderador.
    relation,                         % Generació de la relació "Un moderador M treballa el dia D en l'event E."  
    true,!.
writeClauses:- told, nl, write('writeClauses failed!'), nl,nl, halt.

eachEventExactlyOneModerator:- event(E), findall( em(E,M), (eventModerators(E,LM), member(M,LM)), Lits), exactly(1, Lits), fail.
eachEventExactlyOneModerator.

eachEventExactlyOneDay:- event(E), findall(ed(E,D), (eventDays(E,LD), member(D,LD)), Lits), exactly(1, Lits), fail.
eachEventExactlyOneDay.

maxEventsDay:- day(D), maxEventsPerDay(M), findall(ed(E,D), event(E), Lits), atMost(M, Lits), fail.
maxEventsDay.

maxsDaysModerator:- moderator(M), maxDaysPerModerator(N), findall(md(M,D), day(D), Lits), atMost(N, Lits),  fail.
maxsDaysModerator.  

relation:- moderator(M), event(E), day(D), writeOneClause([-em(E,M),-ed(E,D),md(M,D)]), fail.
relation.                                                 % em(E,M) & ed(E,D) => md(M,D)
                                                          % Negat: (-em(E,M) | -ed(E,D)) | md(M,D)

%%%%%%%  3. DisplaySol: show the solution. Here M contains the literals that are true in the model:

% displaySol(M):- nl, write(M), nl, nl, fail.
displaySol(M):-
    day(D), nl, write('Day '), write(D), write(': '),
    findall(E-Mod,(member(ed(E,D),M), member(em(E,Mod),M)), L),
    member(E-Mod,L), write(' event('), write(E), write(')-mod('), write(Mod), write(') '), fail.
displaySol(M):-nl,
    moderator(Mod), nl, findall(D,(member(ed(E,D),M), member(em(E,Mod),M)), L),
    write('Moderator '), write(Mod), write( ' works: '), sort(L,L1), write(L1), fail.
displaySol(_).

%%%%%%% =======================================================================================



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Everything below is given as a standard library, reusable for solving
%%    with SAT many different problems.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%% Cardinality constraints on arbitrary sets of literals Lits: ===========================

exactly(K,Lits):- symbolicOutput(1), write( exactly(K,Lits) ), nl, !.
exactly(K,Lits):- atLeast(K,Lits), atMost(K,Lits),!.

atMost(K,Lits):- symbolicOutput(1), write( atMost(K,Lits) ), nl, !.
atMost(K,Lits):-   % l1+...+ln <= k:  in all subsets of size k+1, at least one is false:
      negateAll(Lits,NLits),
      K1 is K+1,    subsetOfSize(K1,NLits,Clause), writeOneClause(Clause),fail.
atMost(_,_).

atLeast(K,Lits):- symbolicOutput(1), write( atLeast(K,Lits) ), nl, !.
atLeast(K,Lits):-  % l1+...+ln >= k: in all subsets of size n-k+1, at least one is true:
      length(Lits,N),
      K1 is N-K+1,  subsetOfSize(K1, Lits,Clause), writeOneClause(Clause),fail.
atLeast(_,_).

negateAll( [], [] ).
negateAll( [Lit|Lits], [NLit|NLits] ):- negate(Lit,NLit), negateAll( Lits, NLits ),!.

negate( -Var,  Var):-!.
negate(  Var, -Var):-!.

subsetOfSize(0,_,[]):-!.
subsetOfSize(N,[X|L],[X|S]):- N1 is N-1, length(L,Leng), Leng>=N1, subsetOfSize(N1,L,S).
subsetOfSize(N,[_|L],   S ):-            length(L,Leng), Leng>=N,  subsetOfSize( N,L,S).


%%%%%%% Express equivalence between a variable and a disjunction or conjunction of literals ===

% Express that Var is equivalent to the disjunction of Lits:
expressOr( Var, Lits ):- symbolicOutput(1), write( Var ), write(' <--> or('), write(Lits), write(')'), nl, !.
expressOr( Var, Lits ):- member(Lit,Lits), negate(Lit,NLit), writeOneClause([ NLit, Var ]), fail.
expressOr( Var, Lits ):- negate(Var,NVar), writeOneClause([ NVar | Lits ]),!.

%% expressOr(a,[x,y]) genera 3 clausulas (como en la Transformación de Tseitin):
%% a == x v y
%% x -> a       -x v a
%% y -> a       -y v a
%% a -> x v y   -a v x v y

% Express that Var is equivalent to the conjunction of Lits:
expressAnd( Var, Lits) :- symbolicOutput(1), write( Var ), write(' <--> and('), write(Lits), write(')'), nl, !.
expressAnd( Var, Lits):- member(Lit,Lits), negate(Var,NVar), writeOneClause([ NVar, Lit ]), fail.
expressAnd( Var, Lits):- findall(NLit, (member(Lit,Lits), negate(Lit,NLit)), NLits), writeOneClause([ Var | NLits]), !.


%%%%%%% main: =================================================================================

main:-  symbolicOutput(1), !, writeClauses, halt.   % print the clauses in symbolic form and halt Prolog
main:-  initClauseGeneration,
        tell(clauses), writeClauses, told,          % generate the (numeric) SAT clauses and call the solver
        tell(header),  writeHeader,  told,
        numVars(N), numClauses(C),
        write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
        shell('cat header clauses > infile.cnf',_),
        write('Calling solver....'), nl,    
        shell('/mnt/c/Users/Msi/c/Users/Msi/Documents/ENGINYERIA_INFORMATICA/A3-Q2/LI-FIB/Lab/Practica_3/kissat-master/build/kissat -v infile.cnf > model', Result),  % if sat: Result=10; if unsat: Result=20.
        treatResult(Result),!.

treatResult(20):- write('Unsatisfiable'), nl, halt.
treatResult(10):- write('Solution found: '), nl, see(model), symbolicModel(M), seen, displaySol(M), nl,nl,halt.
treatResult( _):- write('cnf input error. Wrote anything strange in your cnf?'), nl,nl, halt.


initClauseGeneration:-  %initialize all info about variables and clauses:
        retractall(numClauses(   _)),
        retractall(numVars(      _)),
        retractall(varNumber(_,_,_)),
        assert(numClauses( 0 )),
        assert(numVars(    0 )),     !.

writeOneClause([]):- symbolicOutput(1),!, nl.
writeOneClause([]):- countClause, write(0), nl.
writeOneClause([Lit|C]):- w(Lit), writeOneClause(C),!.
w(-Var):- symbolicOutput(1), satVariable(Var), write(-Var), write(' '),!.
w( Var):- symbolicOutput(1), satVariable(Var), write( Var), write(' '),!.
w(-Var):- satVariable(Var),  var2num(Var,N),   write(-), write(N), write(' '),!.
w( Var):- satVariable(Var),  var2num(Var,N),             write(N), write(' '),!.
w( Lit):- told, write('ERROR: generating clause with undeclared variable in literal '), write(Lit), nl,nl, halt.


% given the symbolic variable V, find its variable number N in the SAT solver:
:-dynamic(varNumber / 3).
var2num(V,N):- hash_term(V,Key), existsOrCreate(V,Key,N),!.
existsOrCreate(V,Key,N):- varNumber(Key,V,N),!.                            % V already existed with num N
existsOrCreate(V,Key,N):- newVarNumber(N), assert(varNumber(Key,V,N)), !.  % otherwise, introduce new N for V

writeHeader:- numVars(N),numClauses(C), write('p cnf '),write(N), write(' '),write(C),nl.

countClause:-     retract( numClauses(N0) ), N is N0+1, assert( numClauses(N) ),!.
newVarNumber(N):- retract( numVars(   N0) ), N is N0+1, assert(    numVars(N) ),!.

% Getting the symbolic model M from the output file:
symbolicModel(M):- get_code(Char), readWord(Char,W), symbolicModel(M1), addIfPositiveInt(W,M1,M),!.
symbolicModel([]).
addIfPositiveInt(W,L,[Var|L]):- W = [C|_], between(48,57,C), number_codes(N,W), N>0, varNumber(_,Var,N),!.
addIfPositiveInt(_,L,L).
readWord( 99,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ c
readWord(115,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ s
readWord(-1,_):-!, fail. %end of file
readWord(C,[]):- member(C,[10,32]), !. % newline or white space marks end of word
readWord(Char,[Char|W]):- get_code(Char1), readWord(Char1,W), !.

%%%%%%% =======================================================================================
