Generating a version.ml from a jbuild/dune project should work either with the
immediate project, or as part of an embedded build in a subdirectory.

  $ cd a-dune-proj && dune build version.ml --root=.

Now lets try with a jbuild project in the subdirectory:

  $ cd a-jbuild-proj && dune build version.ml --root=.

Now lets try it from the current directory:

  $ dune build a-dune-proj/version.ml --root=.
  $ dune build a-jbuild-proj/version.ml --root=.

