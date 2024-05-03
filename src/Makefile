FILES = dune dune-project ast.ml astscanner.ml irgen.ml lily.ml parser.mly parserscanner.mly sast.ml scanner.mll semant.ml test0.ml test1.ml test2.ml tokenize.ml

all : test1

lily : $(FILES)
	ocamlbuild -pkgs llvm lily.native

test0 : $(FILES)
	ocamlbuild test0.native

test1 : $(FILES)
	dune build test1.exe
	ln -s ./_build/default/test1.exe test1.exe

test2 : $(FILES)
	ocamlbuild -r test2.native

.PHONY : clean
clean :
	dune clean
	rm -f test1.exe test0.exe test2.exe lily.exe