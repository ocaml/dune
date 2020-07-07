Generating a version.ml from a jbuild/dune project should work either with the
immediate project, or as part of an embedded build in a subdirectory.

  $ (cd a-dune-proj && dune build version.ml --root=.)

Now lets try with a jbuild project in the subdirectory:

  $ (cd a-jbuild-proj && dune build version.ml --root=.)
  File "jbuild", line 1, characters 0-0:
  Error: jbuild files are no longer supported, please convert this file to a
  dune file instead.
  Note: You can use "dune upgrade" to convert your project to dune.
  [1]

Now lets try it from the current directory:

  $ dune build a-dune-proj/version.ml --root=.
  File "a-jbuild-proj/jbuild", line 1, characters 0-0:
  Error: jbuild files are no longer supported, please convert this file to a
  dune file instead.
  Note: You can use "dune upgrade" to convert your project to dune.
  [1]
  $ dune build a-jbuild-proj/version.ml --root=.
  File "a-jbuild-proj/jbuild", line 1, characters 0-0:
  Error: jbuild files are no longer supported, please convert this file to a
  dune file instead.
  Note: You can use "dune upgrade" to convert your project to dune.
  [1]

