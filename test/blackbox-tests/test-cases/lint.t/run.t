The lint alias will run preprocessing actions listed under (lint). It also
defines corrections that may be promoted. The PPX checks for an undeclared
build target and replaces the expression with 42 when that target is hidden by
a sandbox.

  $ cat > correct/add.ml << EOF
  > let () = Printf.printf "%d\n" @@ 1 + 2
  > EOF
  $ dune build correct/lint-secret
  $ dune build @correct/lint
  File "correct/add.ml", line 1, characters 0-0:
  --- correct/add.ml
  +++ correct/add.ml.lint-corrected
  @@ -1 +1 @@
  -let () = Printf.printf "%d\n" @@ 1 + 2
  +let () = Printf.printf "%d\n" @@ 42
  [1]
  $ dune promote correct/add.ml
  Promoting _build/default/correct/add.ml.lint-corrected to correct/add.ml.
  $ cat correct/add.ml
  let () = Printf.printf "%d\n" @@ 42

Starting with Dune 3.26, PPX lint corrections are formatted before promotion:

  $ rm dune-project
  $ make_dune_project 3.26
  $ mkdir bin
  $ cp ../../utils/ocamlformat.exe bin/ocamlformat
  $ cp ../../utils/ocamlformat.exe bin/ocamlformat.exe
  $ export PATH="$PWD/bin:$PATH"
  $ printf '%s\n' 'let () = Printf.printf "%d\n" @@ 1 + 2' > correct/add.ml
  $ dune build @correct/lint --auto-promote > lint.log 2>&1
  [1]
  $ grep 'fake ocamlformat is running' lint.log
  fake ocamlformat is running: "--impl" "--inplace" "--name" "add.ml" "add.ml.lint-corrected"
  $ cat correct/add.ml
  (* fake ocamlformat output *)
