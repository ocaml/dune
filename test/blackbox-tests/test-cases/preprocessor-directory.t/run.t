This test exercises the logic for choosing the directory from which to invoke
the preprocessor command. There are two rewrite.sh scripts in this test: one in
the root directory which rewrites the source to print "foo ..." and one in the
src directory which writes the source to print "bar ...".

Initially the project uses dune lang 3.7 so the preprocessor command is invoked
from the project root:
  $ cat dune-project
  (lang dune 3.7)
  (package (name foo))
  $ dune exec ./src/foo.exe
  foo (from the rewrite.sh script in the project root directory)

Change the versiono of the dune language to 3.8:
  $ sed -i.bak 's/(lang dune 3.7)/(lang dune 3.8)/' $(readlink dune-project)

Now the preprocessor should be invoked from the src directory because the dune
file which specifies the preprocessor is located there:
  $ cat dune-project
  (lang dune 3.8)
  (package (name foo))
  $ dune exec ./src/foo.exe
  bar (from the rewrite.sh script in the src directory)
