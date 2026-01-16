Installing multiple directories into share_root currently crashes dune
Originally reported as https://github.com/ocaml/dune/issues/13307

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
  > 
  > (rule
  >  (target
  >   (dir b))
  >  (action
  >   (progn
  >    (run mkdir -p b/share)
  >    (run touch b/share/readme_b.txt))))
  > 
  > (install
  >  (section share_root)
  >  (dirs
  >   (b/share as .)
  >   (a/share as .)))
  > EOF

This would ideally produce a _build/install/default/share/ with both .txt files,
or perhaps an error about two rules targetting the same thing, but crashes instead.
  $ dune build @install 2>&1 | grep "must not crash"
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  [1]
