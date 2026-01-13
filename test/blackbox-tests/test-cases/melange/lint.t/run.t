The lint alias will run preprocessing actions listed under (lint). It also
defines corrections that may be promoted.

  $ cat > correct/add.ml << EOF
  > let () = Printf.printf "%d\n" @@ 1 + 2
  > EOF
  $ dune build @correct/lint
  File "correct/.melange_src/add.ml", line 1:
  Error: ppxlib_driver: the rewriting contains parts from another file.
  It is too complicated to reconcile it with the source: correct/add.ml or correct/add.ml and correct/.melange_src/add.ml
  [1]
  $ dune promote correct/add.ml
  Warning: Nothing to promote for correct/add.ml.
  $ cat correct/add.ml
  let () = Printf.printf "%d\n" @@ 1 + 2

