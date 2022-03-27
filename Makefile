obj = \
      ast.cmo lexer.cmo parser.cmo main.cmo
obj_opt = ${obj:.cmo=.cmx}
target = modalProover
OCAMLC = ocamlc -g
OCAMLOPT = ocamlopt
SOURCES = modal.ml modal.mli proof.ml proof.mli modal.mllib main.ml ast.ml

all: format $(target)

opt: $(target).opt

$(target): $(obj)
	$(OCAMLC) -o $@ $(obj)

$(target).opt: $(obj_opt)
	$(OCAMLOPT) -o $@ $(obj_opt)

format:
	ls $(SOURCES) | xargs ocamlformat -i

%.cmo: %.ml %.cmi
	$(OCAMLC) -c $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) -c $<

%.ml: %.mll
	ocamllex $<

%.ml %.mli: %.mly
	menhir --dump --explain $<

%.cmi: %.mli
	$(OCAMLC) -c $<

main.cmo: 
	$(OCAMLC) -c main.ml

main.cmx:
	$(OCAMLOPT) -c main.ml

lexer.cmo: parser.ml parser.cmi lexer.ml
	$(OCAMLC) -c lexer.ml

lexer.cmx: parser.ml parser.cmi lexer.ml
	$(OCAMLOPT) -c lexer.ml

clean:
	rm -f *.cmo *.cmi *.o *.cmx core $(target) parser.conflicts parser.automaton $(obj) lexer.ml 
	rm -rdf _build
