## Build Instructions

The LILY compiler compiles. If you cannot get it to compile on your system, it is probably a `PEBCAK` issue.

### Required Package versions
- OCaml - 5.1.1
- LLVM - 16.0.6
- opam package `llvm` - 16.0.6+nnp
- opam package `dune` - 3.15.2

### Install Packages

1. Install Ocaml, LLVM 16, and [`opam`](https://ocaml.org/docs/installing-ocaml) on your system (Process Varies).

2. Create the `lily` environment switch with OCaml 5.1.1:
```sh
opam update
opam switch create lily 5.1.1
# For testing the original MicroC:
# opam switch create microc 4.14.2

# restart your shell
# and verify you're on switch lily:
# opam switch
```

3. Install necessary packages:

```sh
opam install ocaml-lsp-server.1.17.0 odoc.2.4.2 ocamlformat.0.26.2 utop.2.14.0 dune.3.15.2 llvm.16.0.6+nnp
# For testing the original MicroC:
# opam install ocaml-lsp-server.1.17.0 odoc.2.4.2 ocamlformat.0.26.2 utop.2.14.0 dune.3.15.2 llvm.14.0.6
```

4. Build the LILY compiler

```sh
# Enter the src/ code directory.
cd src/
make
```

This will produce a `lily.native` file, the executable for your compiler. For standard library functions to work,
you must not move this executable file from the same directory as `stdlib.lily`.



5. Run the LILY compiler on a LILY script (such as the example above)

```sh
# This will produce LLVM IR code.
./lily.native ../text/oneslide.lily

# To actually run the script, pipe the LLVM IR code into lli
./lily.native ../text/oneslide.lily | lli
```

### Run test suite

To automatically run all tests in the `test/` folder, edit the `LLI_COMMAND` field in `run_tests.sh` to the `lli` executable on your system, and run:

```sh
./run_tests.sh
```
