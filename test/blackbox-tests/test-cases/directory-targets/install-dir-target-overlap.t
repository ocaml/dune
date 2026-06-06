Installing multiple directories to the same share_root destination reports a
conflict instead of crashing dune.
Originally reported as https://github.com/ocaml/dune/issues/13307 and
https://github.com/ocaml/dune/issues/14422

  $ cat >dune-project <<EOF
  > (lang dune 3.20)
  > (using directory-targets 0.1)
  > (package (name shared_files))
  > EOF

Create two rules that create directories and an install stanza that 
tries place the contents of both at the same location

  $ cat >dune <<EOF
  > (rule
  >  (target
  >   (dir a))
  >  (action
  >   (progn
  >    (run mkdir -p a/share)
  >    (run touch a/share/readme_a.txt))))
  > (rule
  >  (target
  >   (dir b))
  >  (action
  >   (progn
  >    (run mkdir -p b/share)
  >    (run touch b/share/readme_b.txt))))
  > (install
  >  (section share_root)
  >  (dirs
  >   (b/share as .)
  >   (a/share as .)))
  > EOF

This reports the same duplicate-target error as overlapping file install entries.
  $ dune build @install
  Error: Multiple rules generated for _build/install/default/share:
  - dune:18
  - dune:19
  -> required by _build/default/shared_files.install
  -> required by alias install
  [1]

The same applies when both directories are installed under the same explicit
share_root subdirectory, as reported in #14422.

  $ cat >dune <<EOF
  > (rule
  >  (target
  >   (dir a))
  >  (action
  >   (progn
  >    (run mkdir -p a/share)
  >    (run touch a/share/readme_a.txt))))
  > (rule
  >  (target
  >   (dir b))
  >  (action
  >   (progn
  >    (run mkdir -p b/share)
  >    (run touch b/share/readme_b.txt))))
  > (install
  >  (section share_root)
  >  (dirs
  >   (b/share as man)
  >   (a/share as man)))
  > EOF

  $ dune build @install
  Error: Multiple rules generated for _build/install/default/share/man:
  - dune:18
  - dune:19
  -> required by _build/default/shared_files.install
  -> required by alias install
  [1]
