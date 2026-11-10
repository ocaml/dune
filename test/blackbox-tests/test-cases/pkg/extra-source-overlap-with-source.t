Test for packages with an extra-source file with the same name as a
file in the package's source.

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

Even without build or install actions, overlays must not modify prepared inputs.

  $ cat _build/_private/default/.pkg/$($dune pkg print-digest foo)/source/foo.txt
  from source
  $ cat foo-source/foo.txt
  from source

If foo.txt exists in both the main source and extra_sources, a build action
must read the extra_sources version. The workspace file and the file in the
prepared source directory must retain their original contents.

  $ make_lockpkg foo <<EOF
  > (version 1)
  > (build (run cp foo.txt observed))
  > (source (copy $PWD/foo-source))
  > (extra_sources
  >  (foo.txt (fetch (url file://$PWD/foo.txt))))
  > EOF
  $ echo 'share: [ "observed" ]' > foo-source/foo.install
  $ build_pkg foo
  $ digest=$($dune pkg print-digest foo)
  $ cat _build/_private/default/.pkg/$digest/target/share/foo/observed
  from extra source
  $ cat foo-source/foo.txt
  from source
  $ cat _build/_private/default/.pkg/$digest/source/foo.txt
  from source
