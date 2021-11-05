Copy files from inside a directory target

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (using directory-targets 0.1)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (target (dir foo))
  >  (action (system "mkdir foo && touch foo/{x,y,z}")))
  > (copy_files foo/*)
  > EOF

  $ dune build
  File "dune", line 4, characters 12-17:
  4 | (copy_files foo/*)
                  ^^^^^
  Error: Cannot find directory: foo
  [1]
  $ ls _build/default/
