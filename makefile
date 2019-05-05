all: miniml unit_tests

miniml: miniml.ml
	ocamlbuild miniml.byte

unit_tests: unit_tests.ml
	ocamlbuild unit_tests.byte

clean:
	rm -rf _build *.byte