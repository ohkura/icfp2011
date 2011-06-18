all:	run clean_for_submit pack

run:	run_nop run_stupid_dec run2 run_exp
	cp run2 ../run

clean:
	rm -f *.cmi *.cmo *.cmx *~ *.o run_nop run_stupid_dec run2 ../run

clean_for_submit:
	rm -f *.cmi *.cmo *.cmx *~ *.o

run_nop:	simulator.ml utility.ml strategy.ml run_nop.ml
	ocamlopt simulator.ml utility.ml strategy.ml run_nop.ml -o run_nop

run_stupid_dec:	simulator.ml utility.ml strategy.ml run_stupid_dec.ml
	ocamlopt simulator.ml utility.ml strategy.ml run_stupid_dec.ml -o run_stupid_dec

run2:	simulator.ml utility.ml strategy.ml run2.ml
	ocamlopt simulator.ml utility.ml strategy.ml run2.ml -o run2

run_exp:	simulator.ml utility.ml strategy.ml run_exp.ml
	ocamlopt simulator.ml utility.ml strategy.ml run_exp.ml -o run_exp

pack:
	tar cvf - ../ --exclude .git --exclude pre | gzip > ../../submission.tar.gz
