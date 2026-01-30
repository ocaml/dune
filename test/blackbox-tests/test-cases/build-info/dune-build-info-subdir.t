Demonstrate using a library defined inside a (subdir ..) stanza along side with
dune-build-info.

  $ make_dune_project 3.15

  $ touch foo.ml

  $ cat >dune <<EOF
  > (executable
  >  (name foo)
  >  (libraries bar dune-build-info))
  > (subdir bar
  >  (rule (write-file bar.ml ""))
  >  (library
  >   (name bar)))
  > EOF

  $ dune build ./foo.exe
