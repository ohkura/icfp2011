all:	run pack

run:	run_stupid_dec run2
	cp run2 ../run

clean:	
	rm -f *.cmi *.cmo *.cmx *~ *.o run_stupid_dec run2 ../run

run_stupid_dec:	simulator.ml run_stupid_dec.ml
	ocamlopt simulator.ml run_stupid_dec.ml -o run_stupid_dec

run2:	simulator.ml run2.ml
	ocamlopt simulator.ml run2.ml -o run2

run4:	simulator.ml run4.ml
	ocamlopt simulator.ml run4.ml -o run4

pack:
	tar cvf - ../ --exclude .git --exclude pre | gzip > ../../submission.tar.gz
