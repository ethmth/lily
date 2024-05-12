FILES = dune dune-project ast.ml astscanner.ml irgen.ml lily.ml parser.mly parserscanner.mly readers.ml sast.ml scanner.mll semant.ml stdlib.lily structs.ml test0.ml test1.ml test2.ml tokenize.ml presemant.ml
OUT_FILES = test1.exe test0.exe test2.exe lily.exe test1.native test0.native test2.native lily.native scanner.ml parser.ml parser.mli parserscanner.ml parserscanner.mli *.cmi *.cmo

all : lily

lily : $(FILES) clean
	dune build lily.exe
	ln -s ./_build/default/lily.exe lily.native

test0 : $(FILES) clean
	dune build test0.exe
	ln -s ./_build/default/test0.exe test0.native

test1 : $(FILES) clean
	dune build test1.exe
	ln -s ./_build/default/test1.exe test1.native

test2 : $(FILES) clean
	dune build test2.exe
	ln -s ./_build/default/test2.exe test2.native

.PHONY : watch1
watch1 : clean
	dune build --watch --terminal-persistence=clear-on-rebuild test1.exe

.PHONY : watch2
watch2 : clean
	dune build --watch --terminal-persistence=clear-on-rebuild test2.exe

.PHONY : watch3
watch3 : clean
	dune build --watch --terminal-persistence=clear-on-rebuild lily.exe

.PHONY : watch
watch : watch3

.PHONY : clean
clean :
	dune clean
	rm -f $(OUT_FILES)