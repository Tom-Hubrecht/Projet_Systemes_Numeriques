CMX=graph.cmx netlist_ast.cmx netlist_parser.cmx netlist_printer.cmx\
	netlist_lexer.cmx netlist.cmx scheduler.cmx netlist_simulator.cmx
GENERATED = netlist_lexer.ml

all: simulator

simulator:$(CMX)
	ocamlopt -o simulator $(CMX)
	mv simulator ../simulator

.SUFFIXES: .mli .ml .cmi .cmx .mll .mly

.mli.cmi:
	ocamlopt -c $<

.ml.cmx:
	ocamlopt -c  $<

.mll.ml:
	ocamllex $<

.mly.ml:
	menhir -v $<

.mly.mli:
	ocamlyacc -v $<

clean:
	# Don't delete .cmi files for Merlin
	rm -f *.cm[x] *.o *~ $(GENERATED)

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend

.PHONY: all clean
