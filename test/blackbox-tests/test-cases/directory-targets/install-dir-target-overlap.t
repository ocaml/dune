Two `(install (dirs ...))` entries that resolve to the same destination
directory now merge into a single install dir containing the union of the
contributing files. Originally reported as
https://github.com/ocaml/dune/issues/13307.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name shared_files))
  > EOF

Two rules build separate directory targets; one install stanza puts both
of their contents at the same install location (`share_root` mapped to
`.`).

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

  $ dune build @install 2>&1
  $ ls _build/install/default/share
  readme_a.txt
  readme_b.txt
