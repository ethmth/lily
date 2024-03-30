all : lily

lily : ast.ml irgen.ml lily.ml parser.mly sast.ml scanner.mll semant.ml
	ocamlbuild -pkgs llvm lily.native

test1 : ast.ml irgen.ml lily.ml parser.mly sast.ml scanner.mll semant.ml test1.ml
	ocamlbuild test1.native

test2 : ast.ml irgen.ml lily.ml parser.mly sast.ml scanner.mll semant.ml test2.ml
	ocamlbuild test2.native


.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml test1.native test2.native _build