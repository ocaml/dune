Demonstrate using a library defined inside a (subdir ..) stanza along side with
dune-build-info.

  $ cat >dune-project <<EOF
  > (lang dune 3.15)
  > EOF

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
