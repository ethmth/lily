all : test1

lily : ast.ml irgen.ml lily.ml parser.mly sast.ml scanner.mll semant.ml tokenize.ml clean
	ocamlbuild -pkgs llvm lily.native

test0 : astscanner.ml parserscanner.mly scanner.mll tokenize.ml test0.ml clean
	ocamlbuild test0.native

test1 : ast.ml irgen.ml lily.ml parser.mly sast.ml scanner.mll semant.ml  tokenize.ml test1.ml clean
	ocamlbuild test1.native

test2 : ast.ml irgen.ml lily.ml parser.mly sast.ml scanner.mll semant.ml tokenize.ml test2.ml clean
	ocamlbuild test2.native


true: traverse


.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli parserscanner.ml parserscanner.mli scanner.ml test0.native test1.native test2.native _build
