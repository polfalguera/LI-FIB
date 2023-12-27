make: pizarraLaboProlog exercicis_prolog_P2

pizarraLaboProlog: pizarraLaboProlog.pl
	swipl -q -O -g main --stand_alone=true -o pizarraLaboProlog -c pizarraLaboProlog.pl

exercicis_prolog_P2: exercicis_prolog_P2.pl
	swipl -q -O -g main --stand_alone=true -o exercicis_prolog_P2 -c exercicis_prolog_P2.pl

clean:
	rm pizarraLaboProlog
	rm exercicis_prolog_P2
