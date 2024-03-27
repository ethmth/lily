# LILY

LILY (Lightweight Integrated List Syntax), is a Programming Language implemented using OCaml.

> :warning: **Repo not fully ready**: The files are not yet ready to be worked on. I (Ethan) will update them soon by copying the NanoC/MicroC examples from class.

Currently, the programming language implementation can be found in the `src/` directory, and examples can be found in the `examples/` directory.

PLEASE DO NOT COMMIT BINARY FILES OR COMMIT DIRECTLY TO THE `main` BRANCH.

## Parser Instructions (from NanoC)

```
cd src/
```

### Build the NanoC parser

```
ocamlbuild test.native
```

### Run the NanoC parser

```
./test.native
```

#### Test on specific file

```
cat ../test/example.mc | ./test.native
```

### Compiler files

- `ast.ml`: abstract syntax tree (AST)
- `scanner.mll`: scanner
- ~~`nanocparse.mly`~~ `parser.mly`: parser

### Other files

- `test.ml`: top-level file to test and run the scanner
- ~~`example.mc`: a sample NanoC source code~~
- ~~`example.out`: a sample parsed code of example.mc~~
