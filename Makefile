obj = \
      syntax.cmo core.cmo proof_syntax.cmo proof.cmo
target = modal_proover
OCAMLC = ocamlc -g
SOURCES = syntax.ml syntax.mli core.ml core.mli proof.ml proof.mli proof_syntax.ml proof_syntax.mli

all: $(obj)

format:
	ls $(SOURCES) | xargs ocamlformat -i

%.cmo: %.ml %.cmi
	$(OCAMLC) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<

clean:
	rm -f *.cmo *.cmi $(target)
	rm -rdf _build

