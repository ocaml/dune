The Development Cycle
=====================

Our calculator now has a better structure, but it still does not do much.

In this chapter, we are going to make several iterations where we add a cram
test, see it fail, implement the missing feature, and repeat. This is how many
OCaml projects are developed (including Dune itself).

:::{note}
There are many ways to do this: tests can be written first or last; some prefer
to `promote` only the fixed version.

In this tutorial, we're going to write tests first and `promote` the erroneous
version but as you become more familiar with Dune, you'll be able to explore
variations of this loop.
:::

## Display Errors

So far, our calculator is not doing any error handling.

We're going to create a test with an error, see how the calculator behaves, and
add a better error message.

### Create a Test

Add a new test in `test/calc.t` with the following content. Note that we do
not specify any output for the command. As before, make sure to include two
spaces before the `$` sign.

```{code-block} cram
:emphasize-lines: 4
  $ calc -e '1+2'
  3

  $ calc -e '1+'
```

Next, run the tests using `dune runtest`.
Dune will display a diff with the actual output:

```diff
   $ calc -e '1+'
+  calc: internal error, uncaught exception:
+        Calc.Parser.MenhirBasics.Error
+
+  [125]
```

Run `dune promote` and observe that the error message is part of the test.
At that point, running `dune runtest` will succeed.

Our goal for the rest of this section is to make this test display a nice error message.

### Handle the Exception

The message points to an uncaught exception.
Indeed, `Parser.main` can raise `Parser.Error`.
Let's catch this exception in `eval_lb` and display the location of the error
in the input file.

Edit `lib/cli.ml`:

```{code-block} ocaml
:emphasize-lines: 2,5-6
let eval_lb lb =
  try
    let e = Parser.main Lexer.token lb in
    Printf.printf "%d\n" (eval e)
  with Parser.Error ->
    Printf.printf "parse error near character %d" lb.lex_curr_pos
```

:::{note}
This is just an excerpt from the file.
Edit the `eval_lb` to make it look like this.
The modified parts are highlighted.
:::

Now, run the tests again with `dune runtest`. It displays the following diff:

```diff
   $ calc -e '1+'
-  calc: internal error, uncaught exception:
-        Calc.Parser.MenhirBasics.Error
-
-  [125]
+  parse error near character 2
```

Run `dune promote` and note that `test/calc.t` has changed.

Run `dune runtest`. Nothing is displayed, indicated that the test has passed.

:::{note}
This is similar to what happened at the end of {doc}`the previous chapter
<structure>`.

The first `dune runtest` compares the *expected output*
(from `test/calc.t`, the uncaught exception message) to the *actual output*
(our new error message) and displays the diff. So, the uncaught exception
appears as deleted lines (prefixed with `-`) and the new error message appears
as added lines (prefixed with `+`).

Running `dune promote` will copy the last *actual output* to `calc/test.t`.

Running `dune runtest` a second time will compare the *expected output* (the
new message) with the *actual output* of the command (the new message) and find
no difference.
:::

## Add Floats

At this stage, our calculator only supports integers. In this section, we are
going to add support for floating-point numbers and operations.

### Add a Test

First, add a test at the end of `test/calc.t`:

```console
$ calc -e '1+2.5'
```

Run `dune runtest`: this displays an error.

```diff
   $ calc -e '1+2.5'
+  calc: internal error, uncaught exception:
+        Failure("lexing: empty token")
+
+  [125]
```

Run `dune promote` to update the failing test.
Our goal for the rest of this section is to change that test to print `3.5`.

### Add a `Float` constructor

We need to add a new kind of expression. Let's extend the `exp` type in
`lib/ast.ml` to add a new `Float` constructor.

```{code-block} ocaml
:emphasize-lines: 4
type exp =
| Int of int
| Add of exp * exp
| Float of float
```

With this new constructor, we can represent the `2.5` part as `Float 2.5`.

### Lexing and Parsing

We also need to extend our lexer to produce a new token type for floats, and a
production rule in the grammar.

Let's first add a token type in `lib/parser.mly`:

```{code-block} ocaml
:emphasize-lines: 3
%token<int> Int
%token Plus
%token<float> Float
```

A new rule in `lib/lexer.mll`:

```{code-block} ocaml
:emphasize-lines: 7
rule token = parse
    | eof { Parser.Eof }
    | space { token lexbuf }
    | '\n' { Parser.Eof }
    | '+' { Parser.Plus }
    | digit+ { Parser.Int (int_of_string (Lexing.lexeme lexbuf)) }
    | digit+ '.' digit+ { Parser.Float (float_of_string (Lexing.lexeme lexbuf)) }
```

And a new rule in `lib/parser.mly`:

```{code-block} ocaml
:emphasize-lines: 4
expr:
| Int { Int $1 }
| expr Plus expr { Add ($1, $3) }
| Float { Float $1 }
```

### Evaluation

Let's run `dune build`.

With the new constructor, the compiler is now complaining that the pattern
matching in our `eval` function is incomplete.

```
File "lib/cli.ml", line 1, characters 15-70:
1 | let rec eval = function Ast.Int n -> n | Add (a, b) -> eval a + eval b
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error (warning 8 [partial-match]): this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Float _
```

To fix this, we need to tweak `lib/cli.ml` a bit. Instead of returning an
`int`, let's introduce a `value` type that can represent either `int` or
`float`:

```ocaml
type value = VInt of int | VFloat of float

let value_to_string = function
  | VInt n -> string_of_int n
  | VFloat f -> Printf.sprintf "%.6g" f
```

And update the `eval_lb` function to use this printer:

```{code-block} ocaml
:emphasize-lines: 3-5
let eval_lb lb =
  try
    let expr = Parser.main Lexer.token lb in
    let v = eval expr in
    Printf.printf "%s\n" (value_to_string v)
  with Parser.Error ->
    Printf.printf "parse error near character %d" lb.lex_curr_pos
```

Finally, for `eval` itself, we'll use `(+)` or `(+.)` if both values have the
same type, or convert integers to floats if needed:

```ocaml
let rec eval = function
  | Ast.Int n -> VInt n
  | Float f -> VFloat f
  | Add (a, b) -> (
      match (eval a, eval b) with
      | VInt na, VInt nb -> VInt (na + nb)
      | VFloat fa, VFloat fb -> VFloat (fa +. fb)
      | VInt na, VFloat fb -> VFloat (float na +. fb)
      | VFloat fa, VInt nb -> VFloat (fa +. float nb))
```

With this implementation done, call `dune runtest` and notice that it changes
the output. Call `dune promote` to update the test.

## Pi

In this section, we're going to add named constants, and `pi` in particular.

### Create a test

Add a new test in `test/calc.t`:

```console
  $ calc -e '1+pi'
```

Run `dune runtest` and see the failure.
Run `dune promote` to add the failure to the test file.
Our goal in the rest of the section is to change the output of this test.

### Add a Constructor

Let's add an new constructor in `lib/ast.ml`:

```{code-block} ocaml
:emphasize-lines: 5
type exp =
  | Int of int
  | Add of exp * exp
  | Float of float
  | Ident of string
```

That way, `pi` is going to be represented as `Ident "pi"`.

### Lexing and Parsing

We now need to parse these constants.

Let's add a new token in `lib/parser.mly`:

```{code-block} ocaml
:emphasize-lines: 5
%token Eof
%token<int> Int
%token<float> Float
%token Plus
%token<string> Ident
```

Produce it using a new lexing rule in `lib/lexer.mll`:

```{code-block} ocaml
:emphasize-lines: 3-5,14
let digit = ['0'-'9']

let letter = ['a'-'z']

let ident = letter+

rule token = parse
    | eof { Parser.Eof }
    | space { token lexbuf }
    | '\n' { Parser.Eof }
    | '+' { Parser.Plus }
    | digit+ { Parser.Int (int_of_string (Lexing.lexeme lexbuf)) }
    | digit+ '.' digit+ { Parser.Float (float_of_string (Lexing.lexeme lexbuf)) }
    | ident { Parser.Ident (Lexing.lexeme lexbuf) }
```

And handle it as a new derivation in `lib/parser.mly`:

```{code-block} ocaml
:emphasize-lines: 5
expr:
| Int { Int $1 }
| Float { Float $1 }
| expr Plus expr { Add ($1, $3) }
| Ident { Ident $1 }
```

### Evaluation

Finally, we'll have to update our evaluation function to take the new
constructor into account.

OCaml does not have a value for `pi`, but it has a `Stdlib.acos` function.
Since {math}`\cos{\frac{\pi}{2}} = 0`, we can define `pi` as {math}`2 * \arccos
0` .

```{code-block} ocaml
:emphasize-lines: 10-11
let rec eval = function
  | Ast.Int n -> VInt n
  | Float f -> VFloat f
  | Add (a, b) -> (
      match (eval a, eval b) with
      | VInt na, VInt nb -> VInt (na + nb)
      | VFloat fa, VFloat fb -> VFloat (fa +. fb)
      | VInt na, VFloat fb -> VFloat (float na +. fb)
      | VFloat fa, VInt nb -> VFloat (fa +. float nb))
  | Ident "pi" -> VFloat (2. *. Stdlib.acos 0.)
  | Ident _ -> failwith "unknown ident"
```

Finally, let's run our test. `dune runtest` will display a diff with the new
value. Run `dune promote` to accept it.

## Multiplication

### Create a Test

Let's add a new test in `test/calc.t`:

```cram
  $ calc -e '1+2*3'
```

Run the test with `dune runtest`. Notice the error message.

Let's run `dune promote` to add it to the file.

Now, our goal for the rest of this section is to change that to the expected
result.

### Add a Constructor

Let's update the AST `lib/ast.ml`: we're generalizing addition to binary
operations, and create a new `Mul` operation (notice that we remove the line corresponding to the `Add` expression).

```{code-block} ocaml
:emphasize-lines: 1-3,9
type op =
  | Add
  | Mul

type exp =
  | Int of int
  | Float of float
  | Ident of string
  | Op of op * exp * exp
```

### Lexing and Parsing

Let's add a new token for `*` in `lib/parser.mly`:

```{code-block} ocaml
:emphasize-lines: 3
%token<string> Ident
%token Plus
%token Star
```

Then, we'll produce it using a new rule in `lib/lexer.mll`:

```{code-block} ocaml
:emphasize-lines: 4
    | space { token lexbuf }
    | '\n' { Parser.Eof }
    | '+' { Parser.Plus }
    | '*' { Parser.Star }
```

And use that token in a new rule in `lib/parser.mly` (also modifying the `Add`
rule):

```{code-block} ocaml
:emphasize-lines: 4,5
| Int { Int $1 }
| Float { Float $1 }
| Ident { Ident $1 }
| expr Plus expr { Op (Add, $1, $3) }
| expr Star expr { Op (Mul, $1, $3) }
```

We have a last edit to do here, which is to add a precedence annotation:

```{code-block} ocaml
:emphasize-lines: 2
%left Plus
%left Star
```

### Evaluation

Now, we can update our evaluation function in `lib/cli.ml`:

```{code-block} ocaml
:emphasize-lines: 1-6,13-14
let eval_number_op f_int f_float va vb =
  match (va, vb) with
  | VInt na, VInt nb -> VInt (f_int na nb)
  | VFloat fa, VFloat fb -> VFloat (f_float fa fb)
  | VInt na, VFloat fb -> VFloat (f_float (float_of_int na) fb)
  | VFloat fa, VInt nb -> VFloat (f_float fa (float_of_int nb))

let rec eval = function
  | Ast.Int n -> VInt n
  | Float f -> VFloat f
  | Ident "pi" -> VFloat (2. *. Stdlib.acos 0.)
  | Ident _ -> failwith "unknown ident"
  | Op (Add, a, b) -> eval_number_op ( + ) ( +. ) (eval a) (eval b)
  | Op (Mul, a, b) -> eval_number_op ( * ) ( *. ) (eval a) (eval b)
```

With these updates done, let's run `dune runtest`. It displays that the result
is `7`. Call `dune promote` to update the test.

## Division

This section is going to be very similar to the previous one. Instead we're
adding the division operator.

### Create a Test

Add a test in `test/calc.t`:

```cram
  $ calc -e '4/2'
```

Call `dune runtest`, note the error, call `dune promote`.

### Add a Constructor

Add a constructor in `lib/ast.ml`:

```{code-block} ocaml
:emphasize-lines: 4
type op =
  | Add
  | Mul
  | Div
```

### Lexing and Parsing

Update `lib/parser.mly`:

```{code-block} ocaml
:emphasize-lines: 4
%token<string> Ident;
%token Plus
%token Star
%token Slash
```

Then add the right precedence:

```{code-block} ocaml
:emphasize-lines: 2
 %left Plus
+%left Star Slash
```

And the corresponding rule:

```{code-block} ocaml
:emphasize-lines: 4
| Ident { Ident $1 }
| expr Plus expr { Op (Add, $1, $3) }
| expr Star expr { Op (Mul, $1, $3) }
| expr Slash expr { Op (Div, $1, $3) }
```

Finally, add a lexing rule in `lib/lexer.mll`:

```{code-block} ocaml
:emphasize-lines: 3
    | '+' { Parser.Plus }
    | '*' { Parser.Star }
    | '/' { Parser.Slash }
```

### Evaluation

Add a new case in `lib/cli.ml`:

```{code-block} ocaml
:emphasize-lines: 8
let rec eval = function
  | Ast.Int n -> VInt n
  | Float f -> VFloat f
  | Ident "pi" -> VFloat (2. *. Stdlib.acos 0.)
  | Ident _ -> failwith "unknown ident"
  | Op (Add, a, b) -> eval_number_op ( + ) ( +. ) (eval a) (eval b)
  | Op (Mul, a, b) -> eval_number_op ( * ) ( *. ) (eval a) (eval b)
  | Op (Div, a, b) -> eval_number_op ( / ) ( /. ) (eval a) (eval b)
```

Now, running `dune runtest` should display the right result. Run `dune promote`
to accept it.

## Sine

### Create a Test

Create a new test in `test/calc.t`:

```cram
  $ calc -e 'sin (pi / 6)'
```

Call `dune runtest`, note the error, call `dune promote`.

### Add a Constructor

Add a constructor in `lib/ast.ml`:

```{code-block} ocaml
:emphasize-lines: 6
type exp =
  | Int of int
  | Float of float
  | Ident of string
  | Op of op * exp * exp
  | Call of string * exp
```

### Lexing and Parsing

Update `lib/parser.mly` by defining new tokens for parentheses:

```{code-block} ocaml
:emphasize-lines: 5
%token<string> Ident;
%token Plus
%token Star
%token Slash
%token Lpar Rpar
```

And a production for calls:

```{code-block} ocaml
:emphasize-lines: 5
| Ident { Ident $1 }
| expr Plus expr { Op (Add, $1, $3) }
| expr Star expr { Op (Mul, $1, $3) }
| expr Slash expr { Op (Div, $1, $3) }
| Ident Lpar expr Rpar { Call ($1, $3) }
```

Update `lib/lexer.mll`:

```{code-block} ocaml
:emphasize-lines: 4-5
    | '+' { Parser.Plus }
    | '*' { Parser.Star }
    | '/' { Parser.Slash }
    | '(' { Parser.Lpar }
    | ')' { Parser.Rpar }
```

### Evaluation

We'll need a `float` to pass to `Stdlib.sin`, so let's introduce a conversion
function. We'll also add the corresponding cases in `lib/cli.ml`:

```{code-block} ocaml
:emphasize-lines: 1-3,13-14
let as_float = function
  | VInt n -> float_of_int n
  | VFloat f -> f

let rec eval = function
  | Ast.Int n -> VInt n
  | Float f -> VFloat f
  | Ident "pi" -> VFloat (2. *. Stdlib.acos 0.)
  | Ident _ -> failwith "unknown ident"
  | Op (Add, a, b) -> eval_number_op ( + ) ( +. ) (eval a) (eval b)
  | Op (Mul, a, b) -> eval_number_op ( * ) ( *. ) (eval a) (eval b)
  | Op (Div, a, b) -> eval_number_op ( / ) ( /. ) (eval a) (eval b)
  | Call ("sin", e) -> VFloat (Stdlib.sin (as_float (eval e)))
  | Call _ -> failwith "unknown function"
```

Now, run the tests using `dune runtest`.
Accept the correction with `dune promote`.

## Conclusion

We've added several features to our calculator, and added tests in the meantime.
To do so, we've used `dune runtest` and `dune promote`, two of the most useful
Dune commands.

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

:::{literalinclude} structure/lib/dune
:caption: lib/dune (unchanged)
:language: dune
:::

:::{literalinclude} development-cycle/lib/ast.ml
:caption: lib/ast.ml
:language: ocaml
:::

:::{literalinclude} development-cycle/lib/cli.ml
:caption: lib/cli.ml
:language: ocaml
:::

:::{literalinclude} development-cycle/lib/lexer.mll
:caption: lib/lexer.mll
:language: ocaml
:::

:::{literalinclude} development-cycle/lib/parser.mly
:caption: lib/parser.mly
:language: ocaml
:::

:::{literalinclude} structure/test/dune
:caption: test/dune (unchanged)
:language: dune
:::

:::{literalinclude} development-cycle/test/calc.t
:caption: test/calc.t
:language: cram
:::

::::
