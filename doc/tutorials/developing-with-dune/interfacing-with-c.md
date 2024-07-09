Interfacing with C
==================

In this chapter, we're going to extend our calculator with a new function.
The difference with `sin` is that we're going to use a C implementation of the
function because it is not available in `Stdlib`. To do so, we're going to use
{doc}`(foreign_stubs) </reference/foreign-stubs>` to implement the function in
C.

## Create a test

Add a new test in `test/calc.t`:

```console
  $ calc -e 'log10(123456)'
```

Run `dune runtest` and see the failure.
Run `dune promote` to add the failure to the test file.
Our goal in the rest of the chapter is to change the output of this test.

## Lexing

We have a tiny change to make in `lib/lexer.mll`: extend function names so that
they can contain numbers (but not at the beginning).

```{code-block} ocaml
let ident = letter (letter | digit)+
```

## Evaluation

Let's extend our `eval` function in `lib/cli.ml` to handle a `log10` function.
Instead of implementing the function in OCaml, we declare it as an `external`
(with its type).

```{code-block} ocaml
:emphasize-lines: 1,12
external log10_c : float -> float = "calc_log10"

let rec eval = function
  | Ast.Int n -> VInt n
  | Float f -> VFloat f
  | Ident "pi" -> VFloat (2. *. Stdlib.acos 0.)
  | Ident _ -> failwith "unknown ident"
  | Op (Add, a, b) -> eval_number_op ( + ) ( +. ) (eval a) (eval b)
  | Op (Mul, a, b) -> eval_number_op ( * ) ( *. ) (eval a) (eval b)
  | Op (Div, a, b) -> eval_number_op ( / ) ( /. ) (eval a) (eval b)
  | Call ("sin", e) -> VFloat (Stdlib.sin (as_float (eval e)))
  | Call ("log10", e) -> VFloat (log10_c (as_float (eval e)))
  | Call _ -> failwith "unknown function"
```

## Create Foreign Stubs

The final part is to implement `calc_log10` as a C function and link it with
our library.

Let's create a file `lib/calc_stubs.c`:

:::{literalinclude} interfacing-with-c/lib/calc_stubs.c
:language: c
:::

:::{note}

The interface between C and OCaml code uses a C type called `value`.

Values of this type can be converted from and to `double` using `Double_val`
and `caml_copy_double`.

They need to be registered with the garbage collector using the `CAMLparam1`
and `CAMLreturn` macros.
:::

And we finally we specify to Dune that this file is part of the library in
`lib/dune`:

```{code-block} dune
:emphasize-lines: 4-6
(library
 (name calc)
 (libraries cmdliner)
 (foreign_stubs
  (language c)
  (names calc_stubs)))
```

Run the tests again with `dune runtest`.
At that point, the output should be correct.
Call `dune promote` to update the expected output.

## Conclusion

In this chapter, we've extended our library with some C code. The mechanism to
do so is called {doc}`foreign stubs </reference/foreign-stubs>`.

::::{dropdown} Checkpoint
:icon: location

This is how the project looks like at the end of this chapter.

:::{literalinclude} introduction/dune-project
:caption: dune-project (unchanged)
:language: dune
:::

:::{literalinclude} structure/bin/dune
:caption: bin/dune (unchanged)
:language: dune
:::

:::{literalinclude} structure/bin/calc.ml
:caption: bin/calc.ml (unchanged)
:language: ocaml
:::

:::{literalinclude} interfacing-with-c/lib/dune
:caption: lib/dune
:language: dune
:::

:::{literalinclude} development-cycle/lib/ast.ml
:caption: lib/ast.ml (unchanged)
:language: ocaml
:::

:::{literalinclude} interfacing-with-c/lib/calc_stubs.c
:caption: lib/calc_stubs.c
:language: c
:::

:::{literalinclude} interfacing-with-c/lib/cli.ml
:caption: lib/cli.ml
:language: ocaml
:::

:::{literalinclude} interfacing-with-c/lib/lexer.mll
:caption: lib/lexer.mll
:language: ocaml
:::

:::{literalinclude} development-cycle/lib/parser.mly
:caption: lib/parser.mly (unchanged)
:language: ocaml
:::

:::{literalinclude} structure/test/dune
:caption: test/dune (unchanged)
:language: dune
:::

:::{literalinclude} interfacing-with-c/test/calc.t
:caption: test/calc.t
:language: cram
:::

::::
