Test for packages with an extra-source file with the same name as a
file in the package's source.

  $ . ./helpers.sh
  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 1)
  > (source
  >  (copy $PWD/foo-source))
  > (extra_sources
  >  (foo.txt
  >   (fetch
  >    (url file://$PWD/foo.txt))))
  > EOF

  $ mkdir -p foo-source
  $ echo "from source" > foo-source/foo.txt

  $ echo "from extra source" > foo.txt

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package
  >  (allow_empty)
  >  (name a)
  >  (depends foo))
  > EOF

  $ build_pkg foo

Make sure that the package's source directory ends up with the version
of foo.txt from extra_sources:
  $ cat _build/_private/default/.pkg/$($dune pkg print-digest foo)/source/foo.txt
  from extra source
