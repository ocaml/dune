  $ cat > dune-project << EOF
  > (lang dune 3.6)
  > (generate_opam_files)
  > (package
  >  (allow_empty)
  >  (name p)
  >  (depends
  >   (dune (>= 3.6))))
  > EOF

  $ dune build
  File "dune-project", line 7, characters 8-16:
  7 |   (dune (>= 3.6))))
              ^^^^^^^^
  Warning: This constraint is redundant with the project's (lang dune).

  $ grep '>=' p.opam
    "dune" {>= "3.6" & >= "3.6"}
