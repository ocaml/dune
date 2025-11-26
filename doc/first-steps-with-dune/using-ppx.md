Using a PPX Preprocessor
========================

Our calculator is pretty much opaque: we feed it a string, and it displays a
result (on an error message), but we have now way to know what the internal expression looks like.

In this chapter, we're going to use a `ppx` deriver to generate a `pp_expr`
function that can display expressions.

## Prerequisites

Install [ppx_deriving](https://github.com/ocaml-ppx/ppx_deriving) by running:

```sh
opam install ppx_deriving.5.2.1
```

## Create a test

Add a new test in `test/calc.t`:

```console
$ calc --debug-ast -e '2 * sin (pi / 2)'
```

Run `dune runtest` and see the failure.
Run `dune promote` to add the failure to the test file.
Our goal in the rest of the chapter is to change the output of this test.

## Use `ppx_deriving.show`

Add an `[@@deriving show]` attribute on types in `lib/ast.ml`:

```{code-block} ocaml
:emphasize-lines: 5,13
type op =
  | Add
  | Mul
  | Div
[@@deriving show]

type expr =
  | Int of int
  | Float of float
  | Ident of string
  | Op of op * expr * expr
  | Call of string * expr
[@@deriving show]
```

This will generate `pp_op`, `show_op`, `pp_expr`, and `show_expr` functions.

To do do we need to instruct Dune to use `ppx_deriving.show` as a preprocessor by updating `lib/dune`:

```{code-block} dune
:emphasize-lines: 4-5
(library
 (name calc)
 (libraries cmdliner)
 (preprocess
  (pps ppx_deriving.show))
 (foreign_stubs
  (language c)
  (names calc_stubs)))
```

## Add a `--debug-ast` Flag

We have a few edits to make to `lib/cli.ml`.

First, add a new `cmdliner` flag to parse the command-line and pass it to
`repl` and `eval_lb`:

```{code-block} ocaml
:emphasize-lines: 6-8,11-12
let term =
  let open Cmdliner.Term.Syntax in
  let+ expr_opt =
    let open Cmdliner.Arg in
    value & opt (some string) None & info [ "e" ]
  and+ debug_ast =
    let open Cmdliner.Arg in
    value & flag & info [ "debug-ast" ]
  in
  match expr_opt with
  | Some s -> eval_lb ~debug_ast (Lexing.from_string s)
  | None -> repl ~debug_ast
```

Then, forward it from `repl` to `eval_lb`:

```{code-block} ocaml
:emphasize-lines: 1,5
let repl ~debug_ast =
  while true do
    Printf.printf ">> %!";
    let lb = Lexing.from_channel Stdlib.stdin in
    eval_lb ~debug_ast lb
  done
```

Finally, update `eval_lb` to use it:

```{code-block} ocaml
:emphasize-lines: 1,4
let eval_lb ~debug_ast lb =
  try
    let expr = Parser.main Lexer.token lb in
    if debug_ast then Format.eprintf "[debug] %a\n" Ast.pp_exp expr;
    let v = eval expr in
    Printf.printf "%s\n" (value_to_string v)
  with Parser.Error ->
    Printf.printf "parse error near character %d" lb.lex_curr_pos
```

Run the tests again with `dune runtest`.
At that point, the output should be correct.
Call `dune promote` to update the expected output.

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

:::{literalinclude} using-ppx/lib/dune
:caption: lib/dune
:language: dune
:::

:::{literalinclude} using-ppx/lib/ast.ml
:caption: lib/ast.ml
:language: ocaml
:::

:::{literalinclude} interfacing-with-c/lib/calc_stubs.c
:caption: lib/calc_stubs.c (unchanged)
:language: c
:::

:::{literalinclude} using-ppx/lib/cli.ml
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

:::{literalinclude} using-ppx/test/calc.t
:caption: test/calc.t
:language: cram
:::

::::
