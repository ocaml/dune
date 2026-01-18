Referring to files with a glob in the bin section of the install stanza

  $ cat >dune-project <<EOF
  > (lang dune 3.6)
  > (package (name foo))
  > EOF

Make some scripts to install in bin.
  $ cat >hello.sh <<EOF
  > #!/bin/sh
  > echo "Hello, World!"
  > EOF

  $ cat >foo.sh <<EOF
  > #!/bin/sh
  > echo foo
  > EOF

Refer to the scripts with a glob.
  $ cat >dune <<EOF
  > (install
  >  (section bin)
  >  (files (glob_files *.sh)))
  > EOF

  $ dune build @install

  $ find _build/install/default | sort
  _build/install/default
  _build/install/default/bin
  _build/install/default/bin/foo.sh
  _build/install/default/bin/hello.sh
  _build/install/default/lib
  _build/install/default/lib/foo
  _build/install/default/lib/foo/META
  _build/install/default/lib/foo/dune-package
