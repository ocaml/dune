If a generated directory is "overlaid" by a source dir, then things break.

  $ mkdir .cinaps

  $ cat > dune-project <<EOF
  > (lang dune 2.2)
  > (using cinaps 1.0)
  > EOF

  $ cat > dune <<EOF
  > (cinaps (files *.ml))
  > (dirs .cinaps)
  > EOF

  $ dune build @cinaps
  File "dune", line 1, characters 0-21:
  1 | (cinaps (files *.ml))
      ^^^^^^^^^^^^^^^^^^^^^
  File ".cinaps.6c465b1c/cinaps.ml-gen", line 1, characters 9-28:
  1 | let () = Cinaps_runtime.init ()
               ^^^^^^^^^^^^^^^^^^^
  Error: This function has type unit -> unit
         It is applied to too many arguments; maybe you forgot a `;'.
  [1]
