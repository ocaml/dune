Allow binary name to vary using pforms

  $ cat >dune-project <<EOF
  > (lang dune 3.14)
  > (package (name foo))
  > EOF

  $ cat >dune <<EOF
  > (install
  >  (section bin)
  >  (files (script as %{read:foo})))
  > EOF

  $ cat >script <<EOF
  > #!/bin/sh
  > echo script
  > EOF
  $ chmod +x script

  $ runtest() {
  > printf $1 > foo
  > dune build foo.install && cat _build/default/foo.install
  > }

  $ runtest x
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
  ]
  bin: [
    "_build/install/default/bin/x"
  ]

  $ runtest y
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
  ]
  bin: [
    "_build/install/default/bin/y"
  ]
