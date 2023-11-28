.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

check:
	@bash check.sh

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

finalcheck:
	@bash check.sh final

zip:
	dune clean
	rm -f board.zip
	zip -r board.zip . -x@exclude.lst

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh