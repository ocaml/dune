We test that a directory target with only other subdirs can be
properly promoted.

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (targets (dir foo))
  >  (mode (promote (until-clean)))
  >  (action
  >    (progn
  >      (run mkdir -p foo/bar)
  >      (run touch foo/bar/file1)
  >      (run mkdir -p foo/bar/baz/qux)
  >      (run touch foo/bar/baz/qux/file2))))
  > EOF

  $ dune build foo
  $ ls foo/bar/baz/qux
  file2
