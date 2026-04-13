When a project uses (using menhir ...), the generated opam file should include
a lower bound on the menhir version, since dune assumes menhir supports
--infer-write-query and --infer-read-reply (available since menhir 20180523).

See https://github.com/ocaml/dune/issues/10707

Case 1: menhir used, no explicit menhir dependency declared.
The generated opam file should include "menhir" {>= "20180523"}.

  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (using menhir 2.1)
  > (generate_opam_files true)
  > (package (name foo) (allow_empty))
  > EOF

  $ dune build foo.opam
  $ grep menhir foo.opam
    "menhir" {>= "20180523"}

Case 2: user already declared menhir with a sufficient lower bound.
The auto-injected constraint should not duplicate it.

  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (using menhir 2.1)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends (menhir (>= 20211128))))
  > EOF

  $ dune build foo.opam
  $ grep menhir foo.opam
    "menhir" {>= "20211128"}

Case 3: user declared menhir with no version constraint.
The auto-injected lower bound should be merged in.

  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (using menhir 2.1)
  > (generate_opam_files true)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends menhir))
  > EOF

  $ dune build foo.opam
  $ grep menhir foo.opam
    "menhir" {>= "20180523"}
