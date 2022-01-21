Copy files from inside a directory target

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (using directory-targets 0.1)
  > EOF

Copy from a generated sub-directory
-----------------------------------

  $ cat >dune <<EOF
  > (rule
  >  (target (dir foo))
  >  (deps (sandbox always))
  >  (action (system "mkdir foo && touch foo/x foo/y foo/z")))
  > (copy_files foo/*)
  > EOF

  $ dune build
  File "dune", line 5, characters 12-17:
  5 | (copy_files foo/*)
                  ^^^^^
  Error: Cannot find directory: foo
  [1]

  $ ls _build/default/

Copy from a generated directory somewhere else
----------------------------------------------

  $ rm -f dune
  $ mkdir a b
  $ cat >a/dune <<EOF
  > (rule
  >  (target (dir foo))
  >  (deps (sandbox always))
  >  (action (system "mkdir foo && touch foo/x foo/y foo/z")))
  > EOF

  $ cat >b/dune <<EOF
  > (copy_files ../a/foo/*)
  > EOF

  $ dune build b
  File "b/dune", line 1, characters 12-22:
  1 | (copy_files ../a/foo/*)
                  ^^^^^^^^^^
  Error: Cannot find directory: a/foo
  [1]

  $ dir="_build/default/b"
  $ if [ -d $dir ]; then echo "$dir exists"; else echo "$dir does not exist"; fi
  _build/default/b does not exist
