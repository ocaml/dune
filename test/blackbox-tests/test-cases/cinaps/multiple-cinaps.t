Multiple cinaps stanzas in the same dune file

  $ cat > dune-project <<EOF
  > (lang dune 2.8)
  > (using cinaps 1.0)
  > EOF

  $ cat > dune <<EOF
  > (cinaps (files *.ml))
  > (cinaps (files *.mli))
  > EOF

  $ dune runtest --diff-command diff 2>&1 | sed -E 's/[^ ]+sh/\$sh/'
  File ".cinaps.6c465b1c/cinaps.ml-gen", line 1, characters 9-28:
  1 | let () = Cinaps_runtime.init ()
               ^^^^^^^^^^^^^^^^^^^
  Error: This function has type unit -> unit
         It is applied to too many arguments; maybe you forgot a `;'.
  File ".cinaps.d7d795ae/cinaps.ml-gen", line 1, characters 9-28:
  1 | let () = Cinaps_runtime.init ()
               ^^^^^^^^^^^^^^^^^^^
  Error: This function has type unit -> unit
         It is applied to too many arguments; maybe you forgot a `;'.

  $ touch foo.ml
  $ cat > dune <<EOF
  > (cinaps (files foo.ml))
  > (cinaps (files *.ml))
  > EOF
  $ dune runtest --diff-command diff 2>&1 | sed -E 's/[^ ]+sh/\$sh/'
  Error: Multiple rules generated for
  _build/default/.cinaps.a7811055/cinaps.ml-gen:
  - dune:1
  - dune:2
