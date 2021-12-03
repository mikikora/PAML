obj = \
      modal.cmo proof.cmo ast.cmo lexer.cmo parser.cmo main.cmo
target = modalProover
OCAMLC = ocamlc -g
SOURCES = modal.ml modal.mli proof.ml proof.mli modal.mllib main.ml ast.ml

all: format $(target)

$(target): $(obj)
	$(OCAMLC) -o $@ $(obj)

format:
	ls $(SOURCES) | xargs ocamlformat -i

%.cmo: %.ml %.cmi
	$(OCAMLC) -c $<

%.ml: %.mll
	ocamllex $<

%.ml %.mli: %.mly
	menhir --dump --explain $<

%.cmi: %.mli
	$(OCAMLC) -c $<

main.cmo: 
	$(OCAMLC) -c main.ml

lexer.cmo: parser.ml parser.cmi lexer.ml
	$(OCAMLC) -c lexer.ml

clean:
	rm -f *.cmo *.cmi *.o *.cmx core $(target) parser.conflicts parser.automaton $(obj) lexer.ml
	rm -rdf _build
