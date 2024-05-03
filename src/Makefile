FILES = dune dune-project ast.ml astscanner.ml irgen.ml lily.ml parser.mly parserscanner.mly sast.ml scanner.mll semant.ml test0.ml test1.ml test2.ml tokenize.ml
OUT_FILES = test1.exe test0.exe test2.exe lily.exe test1.native test0.native test2.native lily.native scanner.ml parser.ml parser.mli parserscanner.ml parserscanner.mli *.cmi *.cmo

all : test1

lily : $(FILES)
	ocamlbuild -pkgs llvm lily.native

test0 : $(FILES)
	dune build test0.exe
	ln -s ./_build/default/test0.exe test0.native

test1 : $(FILES)
	dune build test1.exe
	ln -s ./_build/default/test1.exe test1.native

test2 : $(FILES)
	ocamlbuild -r test2.native

.PHONY : clean
clean :
	dune clean
	rm -f $(OUT_FILES)