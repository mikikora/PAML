obj = \
      relation.cmo syntax.cmo ast.cmo error.cmo lexer.cmo parser.cmo core.cmo \
	  proof_syntax.cmo proof.cmo commands.cmo file_handler.cmo main.cmo 
target = modal_proover
SOURCES = \
	syntax.ml syntax.mli core.ml core.mli proof.ml proof.mli proof_syntax.ml proof_syntax.mli \
	ast.mli commands.ml commands.mli main.ml relation.ml relation.mli lexer.mli error.ml error.mli \
	file_handler.ml file_handler.mli

all: $(target) clean

$(target): $(obj)
	ocamlc -o $@ $(obj)

format:
	ls $(SOURCES) | xargs ocamlformat -i

%.cmo: %.ml %.cmi
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

%.ml: %.mll
	ocamllex $<

parser.ml parser.mli: parser.mly 
	@rm -f parser.ml parser.mli parser.automaton parser.conflicts
	menhir parser.mly

parser.cmo: parser.ml parser.cmi lexer.cmi
	ocamlc -c $<

lexer.cmo: lexer.ml parser.cmi lexer.cmi
	ocamlc -c $<

main.cmo: 
	ocamlc -c main.ml

ast.cmo: ast.mli ast.cmi
	ocamlc -c -impl ast.mli

clean:
	rm -f *.cmo *.cmi parser.output parser.conflicts parser.automaton $(obj) lexer.ml parser.ml parser.mli
	rm -rdf _build

nuke: clean
	rm -f $(target)

install: nuke all 

run: all
	rlwrap ./$(target)
