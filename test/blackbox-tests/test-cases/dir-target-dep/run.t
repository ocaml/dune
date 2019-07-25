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

Dune crashes if there's a file named after the directory target
  $ dune build --root file-overlap-dir @all
  Entering directory 'file-overlap-dir'
  Error: Multiple rules generated for _build/default/dir:
  - file present in source tree
  - dune:1
  Hint: rm -f dir
  [1]

directory target and (mode promote) results in a crash
  $ dune build --root mode-promote @all 2>&1 | grep -v line
  Entering directory 'mode-promote'
  Error: exception Sys_error("Is a directory")
  Backtrace:
  
  I must not segfault.  Uncertainty is the mind-killer.  Exceptions are
  the little-death that brings total obliteration.  I will fully express
  my cases.  Execution will pass over me and through me.  And when it
  has gone past, I will unwind the stack along its path.  Where the
  cases are handled there will be nothing.  Only I will remain.
