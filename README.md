# LILY

LILY (Lightweight Integrated List Syntax), is a Programming Language implemented using OCaml.

Currently, the programming language implementation can be found in the `src/` directory, and examples can be found in the `test/` directory.


## Team Members

- [Chimaobi Onwuka](https://github.com/chimaobionwuka) - ceo2134 - Language Guru
- [Michaela Gary](https://github.com/michaelagary) - mng2136 - Manager
- [Ethan Thomas](https://github.com/ethmth) - emt2188 - System Architect
- [Tani Omoyeni](https://github.com/tmo2121) - tmo2121 - Tester
- [Jianjie Sun](https://github.com/cszswx) - js6412 - Tester


## Current Progress

- [x] `scanner.mll` - Scanner. mostly done unless new bugs are detected or extra tokens need added.
- [x] `tokenize.ml` - This function acts as a middleman between the Scanner and the Parser to produce `INDENT`, `DEDENT`, and `NEWLINE` tokens from detected indentation. Should not need changed.
- [x] `parserscanner.mly` - Dummy parser. This is a dummy parser only for testing the Scanner. You should not need to adjust this unless you want to test the scanner using `test0`. This just prints the scanned tokens.
- [x] `astscanner.ml` - Dummy AST. Works with dummy parser to just print scanned tokens. Should not need changed unless you want to run `test0`.

**`test0` - tests scanner (everything above this)** - to run `test0`, run `make test0` (see "How to run tests") after changing `scanner.mll` and `tokenize.ml` to open `Parserscanner` instead of `Parser`.

- [ ] `parser.mly` - Parser. The parser still needs major work (see `TODO`s). It currently has a very limited subset of features. However, currently there are no Shift/Reduce conflicts and it parses the 3 sample files. Try not to commit anything that doesn't build or causes Shift/Reduce conflicts (meaning test the parser on the sample programs using `test1` before you commit).
- [ ] `ast.ml` - AST. AST still needs major work as changes are made to the parser. Similarly, the pretty-print functions need to be adjusted to use LILY syntax instead of MicroC syntax.

**`test1` - tests parser and ast (everything above this)** - to run `test1`, run `make test1` and follow the "How to run tests" instructions below.

- [ ] `semant.ml` - Semantic checker.
- [ ] `sast.ml`

**`test2` - tests semantic checking (everything above this)** - to run `test2`, run `make test2` and follow the "How to run tests" instructions below.


- [ ] `irgen.ml` - IR Generation
 
**`lily.ml` - tests everything** - to use, run `make lily` and `./lily.native ...`

## How to run tests

```sh
cd src/
```

### Build the test

```sh
make test1
```

### Run the test on a specific file

```sh
cat ../test/example.lily | ./test1.native
```

### Clean up when you're done

```sh
make clean
```



## Hello World Front-End Progress Update

So far, we have begun implementation on the scanner, parser, and AST.

### `scanner.mll`

Since our language is indentation-based like python, we've added
special tokens INDENT and DEDENT that detect increases and decreases in
tab indentation. The implementation of this scanning is crude and does not
yet allow for multiple DEDENTs at once (for example, ending a function and 
its nested if statement at once), but works for the limited subset of programs
we have so far.

More work needs to done on the scanner (see `TODO`s in `scanner.mll`) to parse 
for multiple DEDENTs, as well as to determine whether NEWLINE tokens are needed.

We have also implemented a dummy parser `parserscanner.mly` and `astscanner.ml`
that simply prints out the lexxed tokens for testing and debug purposes.

### `parser.mly`

Currently, our parser works for a very limited set of sample LILY programs,
specifically `example.lily` and `hello_world.lily`. Our parser still has 10+
shift/reduce conflicts that need to be sorted out. The parser also still needs
to implement a lot of LILY functionality that it is currently missing, specifically 
lists, elifs, for loops, and try statements.

### `ast.ml`

Our AST currently works hand-in-hand with our parser, containing the different 
expression and statement types as well as useful functions. The same improvements
that need to be made to our parser need to be made in the corresponding location in
`ast.ml`

### Other Files

Currently, all of the other files are taken directly from the MicroC example
and we do not claim ownership of the code.
