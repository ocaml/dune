Copy files from inside a directory target

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (using directory-targets 0.1)
  > EOF

Copy from a generated sub-directory
-----------------------------------

This test just documents that copying from a generated sub-directory
causes a cycle. In theory, it would be possible to avoid but it would
requires deep changes in Dune. The cycle exists at the moment because
Dune loads all the rules of a directory at once.

  $ cat >dune <<EOF
  > (rule
  >  (target (dir foo))
  >  (deps (sandbox always))
  >  (action (system "mkdir foo && touch foo/x foo/y foo/z")))
  > (copy_files foo/*)
  > EOF

  $ dune build
  Error: Dependency cycle between:
     Computing directory contents of _build/default
  -> { dir = In_build_dir "default/foo"
     ; predicate = Glob Glob "*"
     ; only_generated_files = false
     }
  -> Computing directory contents of _build/default
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

  $ ls _build/default/b
  x
  y
  z
