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
  Warning: Unable to read directory bar. Ignoring.
  Remove this message by ignoring by adding:
  (dirs \ bar)
  to the dune file: dune
  Reason: opendir(bar): No such file or directory
