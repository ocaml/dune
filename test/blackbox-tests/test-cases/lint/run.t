The lint alias will run preprocessing actions listed under (lint):

  $ dune build @detect/lint
           ppx alias detect/lint
  File "detect/add.ml", line 1, characters 33-38:
  This addition can be done statically.

When using ppxlib, it is possible to define and promote corrections:

  $ cp correct/add.ml.orig correct/add.ml
  $ dune build @correct/lint --diff-command false
            sh (internal) (exit 1)
  (cd _build/default && /bin/sh -c 'false correct/add.ml correct/add.ml.lint-corrected')
  [1]
  $ dune promote correct/add.ml
  Promoting _build/default/correct/add.ml.lint-corrected to correct/add.ml.
  $ cat correct/add.ml
  let () = Printf.printf "%d\n" @@ 3
