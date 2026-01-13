The lint alias will run preprocessing actions listed under (lint). It also
defines corrections that may be promoted.

  $ cat > correct/add.ml << EOF
  > let () = Printf.printf "%d\n" @@ 1 + 2
  > EOF
  $ dune build @correct/lint
  File "correct/add.ml", line 1, characters 0-0:
  --- correct/add.ml
  +++ correct/add.ml.lint-corrected
  @@ -1 +1 @@
  -let () = Printf.printf "%d\n" @@ 1 + 2
  +let () = Printf.printf "%d\n" @@ 3
  [1]
  $ dune promote correct/add.ml
  Promoting _build/default/correct/add.ml.lint-corrected to correct/add.ml.
  $ cat correct/add.ml
  let () = Printf.printf "%d\n" @@ 3

Melange-specific sources are promoted back to the original `.melange.ml` file.

  $ cat > correct/melange_add.melange.ml << EOF
  > let () = Printf.printf "%d\n" @@ 1 + 2
  > EOF
  $ dune build @correct/lint
  File "correct/melange_add.melange.ml", line 1, characters 0-0:
  --- correct/melange_add.melange.ml
  +++ correct/melange_add.melange.ml.lint-corrected
  @@ -1 +1 @@
  -let () = Printf.printf "%d\n" @@ 1 + 2
  +let () = Printf.printf "%d\n" @@ 3
  [1]
  $ dune promote correct/melange_add.melange.ml
  Promoting _build/default/correct/melange_add.melange.ml.lint-corrected to
    correct/melange_add.melange.ml.
  $ cat correct/melange_add.melange.ml
  let () = Printf.printf "%d\n" @@ 3
