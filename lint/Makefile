APP			= peval1 
OCAMLC		= ocamlc
OCAMLDEP 	= ocamldep

OBJS1 = lexer.cmo parser.cmo ast.cmo interpreter.cmo peval1.cmo
OBJSAST = lexer.cmo parser.cmo ast.cmo print_ast.cmo
DEPEND = lexer.ml parser.ml

all: .depend print_ast peval1

$(APP): $(OBJS1)
	$(OCAMLC) -o $@ $(OBJS1)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmo:
	$(OCAMLC) -c $<

parser.ml: parser.mly
	ocamlyacc -v $<

lexer.ml: lexer.mll
	ocamllex $<

# Clean up
.PHONY: clean
clean:
	rm -f *.cm[io]
	rm -f lexer.ml
	rm -f parser.ml
	rm -f parser.mli
	rm -f parser.output

# Dependencies
.depend: $(DEPEND)
	$(OCAMLDEP) *.mli *.ml > .depend

include .depend 