  $ dune build --root target && cat target/_build/default/dir/*
  Entering directory 'target'
  bar contents
  foo contents

  $ dune build --root dep
  Entering directory 'dep'
  File "dune", line 1, characters 0-68:
  1 | (alias
  2 |  (name default)
  3 |  (deps dir)
  4 |  (action (bash "cat %{deps}/*")))
  Error: No rule found for dir
  [1]

We should not be able to produce a directory in a rule that already exists
  $ dune build --display=short --root no-overlapping-rules
  Entering directory 'no-overlapping-rules'
      ocamldep .foo.eobjs/foo.ml.d
        ocamlc .foo.eobjs/byte/foo.{cmi,cmo,cmt}
      ocamlopt .foo.eobjs/native/foo.{cmx,o}
      ocamlopt foo.exe
           foo dir (exit 2)
  (cd _build/default && ./foo.exe dir)
  Fatal error: exception Unix.Unix_error(Unix.EEXIST, "mkdir", "dir")
  [1]
