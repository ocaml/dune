The lint alias will run preprocessing actions listed under (lint). It also
defines corrections that may be promoted.

  $ cat > correct/add.ml << EOF
  > let () = Printf.printf "%d\n" @@ 1 + 2
  > EOF
  $ dune build @correct/lint
  $ dune promote correct/add.ml
  Warning: Nothing to promote for correct/add.ml.
  $ cat correct/add.ml
  let () = Printf.printf "%d\n" @@ 1 + 2
