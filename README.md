# LILY

LILY (Lightweight Integrated List Syntax), is a Programming Language implemented using OCaml.

Currently, the programming language implementation can be found in the `src/` directory, and examples can be found in the `test/` directory.


## Team Members

- [Chimaobi Onwuka](https://github.com/chimaobionwuka) - ceo2134 - Language Guru
- [Michaela Gary](https://github.com/michaelagary) - mng2136 - Manager
- [Ethan Thomas](https://github.com/ethmth) - emt2188 - System Architect
- [Tani Omoyeni](https://github.com/tmo2121) - tmo2121 - Tester
- [Jianjie Sun](https://github.com/cszswx) - js6412 - Tester


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
cat ../test/example.lily | ./test1.native
```
