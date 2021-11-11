SOURCES = modal.ml modal.mli
CMFILES = modal.cma modal.cmi

all: modal.cma modal.cmi

build: $(SOURCES)
	rm -f $(CMFILES)
	ocamlbuild modal.cma

modal.cma: build
	cp _build/$@ $@

modal.cmi: build
	cp _build/$@ $@

clean:
	rm -f $(CMFILES)
	ocamlbuild -clean

run: modal.cma modal.cmi
	utop modal.cma
