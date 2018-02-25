
OBJS = support.cmo syntax.cmo parser.cmo lexer.cmo main.cmo

all: $(OBJS) f

include .depend

f: $(OBJS)
	@echo Linking $@
	ocamlc -o $@ $(OBJS)

%.cmi : %.mli
	ocamlc -c $<

%.cmo : %.ml
	ocamlc -c $<

parser.ml parser.mli: parser.mly
	ocamlyacc -v parser.mly

%.ml %.mli: %.mll
	@rm -f $@
	ocamllex $<

depend:
	ocamldep *.mli *.ml > .depend
