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

- [x] `parser.mly` - Parser. The parser still needs major work (see `TODO`s). It currently has a very limited subset of features. However, currently there are no Shift/Reduce conflicts and it parses the 3 sample files. Try not to commit anything that doesn't build or causes Shift/Reduce conflicts (meaning test the parser on the sample programs using `test1` before you commit).
- [x] `ast.ml` - AST. AST still needs major work as changes are made to the parser. Similarly, the pretty-print functions need to be adjusted to use LILY syntax instead of MicroC syntax.

**`test1` - tests parser and ast (everything above this)** - to run `test1`, run `make test1` and follow the "How to run tests" instructions below.

- [ ] `semant.ml` - Semantic checker.
- [ ] `sast.ml`

**`test2` - tests semantic checking (everything above this)** - to run `test2`, run `make test2` and follow the "How to run tests" instructions below.


- [ ] `irgen.ml` - IR Generation
 
**`lily.ml` - tests everything** - to use, run `make lily` and `./lily.native ...`

## Environment

### Tested Package versions
- Ocaml - 5.1.1
- `llvm` - 16.0.6+nnp
- `dune` - 3.15.2

### Setup Procedure
1. [Install and Initialize](https://ocaml.org/docs/installing-ocaml) `opam`

2. Create the `lily` environment switch:
```sh
opam update
opam switch create lily 5.1.1
# For testing the original MicroC:
# opam switch create microc 4.14.2

# restart your shell
# and verify you're on switch lily:
opam switch
```

3. Install necessary packages:

```sh
opam install ocaml-lsp-server.1.17.0 odoc.2.4.2 ocamlformat.0.26.2 utop.2.14.0 dune.3.15.2 llvm.16.0.6+nnp
# For testing the original MicroC:
# opam install ocaml-lsp-server.1.17.0 odoc.2.4.2 ocamlformat.0.26.2 utop.2.14.0 dune.3.15.2 llvm.14.0.6
```

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