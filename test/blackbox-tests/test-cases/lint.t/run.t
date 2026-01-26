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
