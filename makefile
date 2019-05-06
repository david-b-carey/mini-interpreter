all: miniml expr

miniml: miniml.ml
	ocamlbuild miniml.byte

expr: expr.ml
	ocamlbuild expr.byte

clean:
	rm -rf _build *.byte