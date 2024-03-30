# LILY

LILY (Lightweight Integrated List Syntax), is a Programming Language implemented using OCaml.

Currently, the programming language implementation can be found in the `src/` directory, and examples can be found in the `test/` directory.

## Parser Instructions

```sh
cd src/
```

### Build the LILY parser

```sh
ocamlbuild test1.native
# OR
make test1
```

### Run the LILY parser

```sh
./test1.native
```

#### Test on specific file

```sh
cat ../test/example.mc | ./test1.native
```

## Compiler Instructions (NOT TESTED YET)

You must have the llvm ocaml package installed (`opam install llvm` should work)

### Build the LILY compiler

```sh
ocamlbuild -pkgs llvm lily.native
# OR
make lily
```

### Run the LILY compiler and generate llvm code

```
./lily.native -l ../test/example.mc > example.out
```

### Run the llvm code

```
lli example.out
```

## File Overview

### Compiler files (`src/`)

- `ast.ml`: abstract syntax tree (AST) definition
- `scanner.mll`: scanner
- `parser.mly`: parser
- `sast.ml`: definition of the semantically-checked AST
- `semant.ml`: semantic checking
- `irgen.ml`: LLVM IR code generator

### Other files

- `src/test1.ml`: the file to test the scanner and parser
- `src/test2.ml`: the file to test the semantic checker
- `src/lily.ml`: top-level file to test and run the LILY compiler
- `test/example.mc`: a sample microc source code
- `test/example.out`: a sample compiled code of example.mc
