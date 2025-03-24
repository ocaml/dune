Improving Structure
===================

Our calculator is fairly monolithic at this stage.
Instead of a single executable, we're going to extract a library and create
some tests.
For now, this is just a cram test that will call the executable, but this
structure will later allow adding unit tests for the library.

## Extract a Library

Create folders `bin`, `lib` and `test`, for binary, library, and tests,
respectively.

Run the following command to install [cmdliner](https://ocaml.org/p/cmdliner/1.3.0/doc/index.html):

```sh
opam install cmdliner.1.3.0
```

Let's create a library. Create `lib/dune` with the `(ocamllex)` and `(menhir)` stanzas from the original `dune` file and a new `(library)` stanza:

:::{literalinclude} structure/lib/dune
:language: dune
:emphasize-lines: 1-3
:::

:::{note}
This is the whole contents of the file. The `(library)` part is highlighted to
show that it's the part that we've just added.
:::

:::{note}

We're defining a {doc}`library </reference/dune/library>` that depends on the
`cmdliner` library.

Libraries can either be defined in your project, or provided by an opam
package. In the case of `cmdliner`, this is the latter, since we've installed
it just before.

{doc}`The OCaml Ecosystem </explanation/ocaml-ecosystem>` covers the difference
between packages, libraries, and modules.
:::

Move `ast.ml`, `lexer.mll`, and `parser.mly` to the `lib` directory.

Now we're going to move `calc.ml` to `lib/cli.ml` and replace it by the following:

:::{literalinclude} structure/lib/cli.ml
:language: ocaml
:::

:::{note}
Two things are happening here.

We are adding a second code path to evaluate a `string` directly, so we extract
an `eval_lb` function that operates on a `lexbuf` (the "source" a lexer can
read from).

We are also moving to `cmdliner` for command-line parsing. This consists in:
- an `info` value (of type `Cmdliner.Cmd.info`) which contains metadata for the program (used in help, etc)
- a `term` value (of type `unit Cmdliner.Term.t`) which sets up arguments and calls `eval_lb` with the right `lexbuf`
- a `cmd` value (of type `unit Cmdliner.Cmd.t`) grouping `info` and `term` together 
- a `main` function of type `unit -> 'a` to run `cmd`
:::

## Extract an Executable

Let's create an executable in `bin`. To do so, create a `bin/dune` file with the following contents:

:::{literalinclude} structure/bin/dune
:language: dune
:::

And `bin/calc.ml` with a single function call:

:::{literalinclude} structure/bin/calc.ml
:language: ocaml
:::

Delete the `dune` at the root.

## Create a Test

Create `test/calc.t` with the following contents.

:::{important}
In {doc}`cram tests </reference/cram>`, commands start with two spaces, a
dollar sign, and a space.

Make sure to include **two spaces** at the beginning of the line.
:::

:::{literalinclude} structure/test/calc.t
:language: cram
:lines: 1
:::

Now create `test/dune` to inform Dune that cram tests will use our `calc`
executable and need to be executed again when it changes:

:::{literalinclude} structure/test/dune
:language: dune
:::

At this stage, we're ready to run our test.

Let's do this with `dune runtest`.

It's displaying a diff:

```diff
   $ calc -e '1+2'
+  3
```

Now, run `dune promote`. The contents of `test/calc.t` have changed. Most
editors will pick this up automatically, but it might be necessary to reload
the file to see the change.

Finally, run `dune runtest`. Nothing happens.

Now, run the calculator by running `dune exec calc` to confirm that the
interactive mode still works.

:::{note}
What happened here? This Dune feature, where some tests can edit the source
file, is called {doc}`promotion </concepts/promotion>`.

{doc}`Cram tests </reference/cram>` contain both commands and their expected input.
We did not include any output in the initial cram test. When running `dune
runtest` for the first time, Dune executes the commands, and calls `diff`
between the *expected output* (in `test/calc.t`: no output at all) and the *actual
output* (from running the command: the line "3"), which will display added
lines with a `+` sign and deleted lines with a `-` sign.

Running `dune promote` replaces the input file (`test/calc.t`) with the last
*actual output*. So this includes the line with "3".

Running `dune runtest` again will execute the test again and compare the
*expected output* (`test/calc.t` with the "3" line in it) with the *actual
output* and finds no difference. This means that the test passes.
:::

::::{dropdown} Checkpoint
:icon: location

This is how the project looks like at the end of this chapter.

:::{literalinclude} introduction/dune-project
:caption: dune-project (unchanged)
:language: dune
:::

:::{literalinclude} structure/bin/dune
:caption: bin/dune
:language: dune
:::

:::{literalinclude} structure/bin/calc.ml
:caption: bin/calc.ml
:language: ocaml
:::

:::{literalinclude} structure/lib/dune
:caption: lib/dune
:language: dune
:::

:::{literalinclude} introduction/ast.ml
:caption: lib/ast.ml (unchanged)
:language: ocaml
:::

:::{literalinclude} structure/lib/cli.ml
:caption: lib/cli.ml
:language: ocaml
:::

:::{literalinclude} introduction/lexer.mll
:caption: lib/lexer.mll (unchanged)
:language: ocaml
:::

:::{literalinclude} introduction/parser.mly
:caption: lib/parser.mly (unchanged)
:language: ocaml
:::

:::{literalinclude} structure/test/dune
:caption: test/dune
:language: dune
:::

:::{literalinclude} structure/test/calc.t
:caption: test/calc.t
:language: cram
:::

::::
