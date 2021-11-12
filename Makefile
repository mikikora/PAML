SOURCES = modal.ml modal.mli proof.ml proof.mli modal.mllib
CMFILES = modal.cma modal.cmi proof.cmi

all: format modal.cma modal.cmi proof.cmi

build: $(SOURCES)
	rm -f $(CMFILES)
	ocamlbuild modal.cma

modal.cma: build
	cp _build/$@ $@

modal.cmi: build
	cp _build/$@ $@

proof.cmi: build
	cp _build/$@ $@

clean:
	rm -f $(CMFILES)
	ocamlbuild -clean

run: all
	utop modal.cma

format:
	ls $(SOURCES) | xargs ocamlformat -i 
