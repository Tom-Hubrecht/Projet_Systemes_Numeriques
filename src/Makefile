CMX=graph.cmx netlist_ast.cmx netlist_parser.cmx netlist_printer.cmx\
	netlist_lexer.cmx netlist.cmx scheduler.cmx netlist_aux.cmx\
	netlist_simulator.cmx
GENERATED=netlist_lexer.ml
BIN=simulator

all: $(BIN)

$(BIN): $(CMX)
	ocamlopt -o $(BIN) $(CMX)

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

# Don't delete .cmi files for Merlin
clean_cmx:
	rm -f *.cmx *.o *~ $(BIN) $(GENERATED)

clean: clean_cmx
	rm -f *.cmi

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend

.PHONY: all clean clean_cmx
