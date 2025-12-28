Test that dune install accepts relative paths for --prefix that are outside the workspace.
This is a regression test for issue #12241.

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (package (name foo))
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name foo)
  >  (public_name foo))
  > EOF

  $ cat > foo.ml <<EOF
  > let x = 1
  > EOF

  $ dune build @install

Test that relative path outside workspace works (previously failed with "path outside the workspace"):

  $ dune install --prefix ../install-target --dry-run --display short 2>&1 | grep "Creating directory" | head -1
  Creating directory $TESTCASE_ROOT/../install-target/lib/foo
