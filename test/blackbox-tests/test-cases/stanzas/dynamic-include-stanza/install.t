Test that install stanzas work with dynamic_include.
See https://github.com/ocaml/dune/issues/13492

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (package (name foo))
  > EOF

  $ mkdir a b

  $ cat >a/dune <<EOF
  > (rule
  >  (with-stdout-to dune.inc
  >   (echo "(install (section share) (files foo.txt))")))
  > EOF

  $ cat >b/dune <<EOF
  > (dynamic_include ../a/dune.inc)
  > (rule (with-stdout-to foo.txt (echo "hello")))
  > EOF

Build to generate the include file:

  $ dune build a/dune.inc
  $ cat _build/default/a/dune.inc
  (install (section share) (files foo.txt))

The install stanza is being silently ignored - the package appears empty:

  $ dune build @install
  Error: The package foo does not have any user defined stanzas attached to it.
  If this is intentional, add (allow_empty) to the package definition in the
  dune-project file
  -> required by _build/default/foo.install
  -> required by alias install
  [1]

For comparison, if we put the install stanza directly in the dune file it works:

  $ cat >b/dune <<EOF
  > (install (section share) (files foo.txt))
  > (rule (with-stdout-to foo.txt (echo "hello")))
  > EOF

  $ dune build @install
  $ cat _build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
  ]
  share: [
    "_build/install/default/share/foo/foo.txt"
  ]
