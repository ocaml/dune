Unit Tests
==========

We're testing our calculator using cram tests, but in some cases, it can be more comfortable to use unit tests to test internal behaviors.

In this chapter, we're going to extract the function that deals with conversion
to floats and put it in a test harness.

## Install Dependencies

Run `opam install alcotest.1.7.0`.

## Refactor

Let's refactor our `eval_number_op` function in `lib/cli.ml` so that it uses the `as_float` function. Move `as_float` above it so that is can be used.

```{code-block} ocaml
let eval_number_op f_int f_float va vb =
  match (va, vb) with
  | VInt na, VInt nb -> VInt (f_int na nb)
  | _ ->
      let fa = as_float va in
      let fb = as_float vb in
      VFloat (f_float fa fb)
```

## Create a Test Suite

We're first going to move our cram tests so that they can be in `test/cram/`,
while our unit tests will be in `test/unit/`.

Move the contents of the folder `test` into a fresh folder `test/cram/`.

Create a folder `test/unit/`.

Create `test/unit/dune` with the following contents:

```dune
(test
 (name test_calc)
 (libraries alcotest calc))
```

And `test/unit/test_calc.ml`:

```ocaml
open Calc

let test_as_float =
  let test ~name expression ~expected =
    ( Printf.sprintf "as_float(%s)" name,
      `Quick,
      fun () ->
        let got = Cli.as_float expression in
        Alcotest.check
          (Alcotest.float Stdlib.epsilon_float)
          __LOC__ expected got )
  in
  [
    test ~name:"int" (VInt 2) ~expected:2.;
    test ~name:"float" (VFloat 3.5) ~expected:3.5;
  ]

let suite = [ ("as_float", test_as_float) ]

let () = Alcotest.run __FILE__ suite
```

Now run `dune runtest`. In addition to the existing cram tests, this also runs
unit tests.
