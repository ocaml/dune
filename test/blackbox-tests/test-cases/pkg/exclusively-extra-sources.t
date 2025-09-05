Test for packages with no source field but with extra_sources.

  $ . ./helpers.sh
  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 1)
  > (extra_sources
  >  (foo.txt
  >   (fetch
  >    (url file://$PWD/foo.txt))))
  > EOF

  $ touch foo.txt

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package
  >  (allow_empty)
  >  (name a)
  >  (depends foo))
  > EOF

  $ dune build
