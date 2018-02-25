OBJS = support.cmo main.cmo

all: $(OBJS) f

include .depend

f: $(OBJS)
	@echo Linking $@
	ocamlc -o $@ $(OBJS)

%.cmi : %.mli
	ocamlc -c $<

%.cmo : %.ml
	ocamlc -c $<

depend:
	ocamldep *.mli *.ml > .depend
