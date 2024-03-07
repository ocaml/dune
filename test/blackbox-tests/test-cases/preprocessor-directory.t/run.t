This test exercises the logic for choosing the directory from which to invoke
the preprocessor command. There are two rewrite.sh scripts in this test: one in
the root directory which rewrites the source to print "foo ..." and one in the
src directory which writes the source to print "bar ...".

The preprocessor command is invoked from the workspace root:
  $ cat dune-project
  (lang dune 3.7)
  (package (name foo))
  $ dune exec ./src/foo.exe
  foo (from the rewrite.sh script in the project root directory)

This is unintended. We should be printing bar:
  $ dune exec ./src/foo.exe
  foo (from the rewrite.sh script in the project root directory)
