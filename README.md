# LILY

LILY (Lightweight Integrated List Syntax), is a Programming Language 

This is the source code for LILY's compiler, implemented using OCaml.

Currently, the programming language implementation can be found in the `src/` directory, and examples can be found in the `test/` directory.

## Team Members

- [Chimaobi Onwuka](https://github.com/chimaobionwuka) - ceo2134 - Language Guru
- [Michaela Gary](https://github.com/michaelagary) - mng2136 - Manager
- [Ethan Thomas](https://github.com/ethmth) - emt2188 - System Architect
- [Tani Omoyeni](https://github.com/tmo2121) - tmo2121 - Tester
- [Jianjie Sun](https://github.com/cszswx) - js6412 - Tester

## Language Features

- [x] **Indentation** - Lily's parser uses indentation-style blocking (like Python) instead of bracket-style or C-style.
- [x] **Printing** - The built in `print()` function can take an arbitrary number of primitive (`Int`, `Float`, `Char`) arguments to print in Python-style.
- [x] **List Printing** - Calling `print(list)` on a single list will print the list nicely formatted.
- [x] **Nested Functions** - Lily allows for nested functions.
- [x] **Function Overloading** - Lily allows for overloading functions.
- [x] **For loops and while loops** - Lily supports `for` loops and `while` loops in the style specified in the LRM.
- [x] **For `in` loops** - Lily supports iterating over a list in the style `for element in list`.
- [x] **Lists** - Lily supports the declaration and referencing of `lists` in the style specified in the LRM.
- [x] **Standard Functions** - `stdlib.lily` contains `print`-ing functions, `sum`, `min`, `max`, and more.
- [x] **List Operations** - `stdlib.lily` contains `range`, `copy`, `append`, `remove`, `reverse`, `concatenate`, and `pop`.
- [x] **Sorting Functions** - `stdlib.lily` contains `mergesort` and `selectionsort` functions, allowing the user to pick a preferred algorithm.
- [x] **Search Functions** - `stdlib.lily` contains `binarysearch` and `search` (linear), allowing the user to pick the most efficient algorithm for their use case (sorted data vs. unsorted data).
- [x] **"Pseudo-Randomness"** - `stdlib.lily` contains a crude pseudorandom function `random(a, b)`.
- [x] **Multi-List Functions** - `stdlib.lily` contains `shuffle(list1, list2)` and `merge(list1, list2)`. Users can also define their own multi-list functions.

## File Overview

- [x] `scanner.mll`
- [x] `tokenize.ml` - This function acts as a middleman between the Scanner and the Parser to produce `INDENT`, `DEDENT`, and `NEWLINE` tokens from detected indentation.
- [x] `parser.mly`
- [x] `ast.ml`

**`test1` - tests parser and ast (everything above this)** - to run `test1`, run `make test1` and follow the "How to run tests" instructions below.

- [x] `presemant.ml` - Semantic checking pre-processing step. Allows conversion of single `stmts` into a list of multiple `stmts` before being passed to `semant.ml`. Used to allow for functions that take generic lists as arguments.
- [x] `semant.ml`
- [x] `sast.ml`
- [x] `structs.ml` - Includes the definition of the `FuncId` struct which is needed for the `FuncMap` in `semant.ml`, used for storing a map of functions (needed to allow for function overloading).

**`test2` - tests semantic checking (everything above this)** - to run `test2`, run `make test2` and follow the "How to run tests" instructions below.

- [x] `irgen.ml`
- [x] `stdlib.lily` - Standard library functions, written in LILY
- [x] `readers.ml` - Provides useful function definitions for reading from files and reading from stdin, needed to include `stdlib.lily` functions in user programs.

**`lily.ml` - the compiler (tests everything)** - to use, run `make` and `./lily.native <filename.lily>` to compile files into llvm.

## Example Program (LILY in one slide)

```rb
def grades_to_gpa_values(grades::char) -> float list:

    let length: int = len(grades)
    
    def grade_to_gpa_value(grade: char) -> float:
        if (grade == 'A'):
            return 4.0
        if (grade == 'B'):
            return 3.0
        if (grade == 'C'):
            return 2.0
        if (grade == 'D'):
            return 1.0
        return 0.0

    let gpas::float = []
    for grade in grades:
        let gpa: float = grade_to_gpa_value(grade)
        gpas.append(gpa)

    return gpas

let my_grades:: char = ['A', 'A', 'B', 'B', 'C', 'C', 'A', 'A', 'F']
my_grades.mergesort()

print(my_grades)

let my_gpas:: float = grades_to_gpa_values(my_grades)

print(my_gpas)

let gpa: float = sum(my_gpas) / flt(len(my_gpas))
print(gpa)

my_gpas.remove(0.0)

gpa = sum(my_gpas) / flt(len(my_gpas))
print(gpa)
```

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
