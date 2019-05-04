all: miniml

miniml: miniml.ml
	ocamlbuild miniml.byte

clean:
	rm -rf _build *.byte