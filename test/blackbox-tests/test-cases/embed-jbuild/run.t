Generating a version.ml from a jbuild/dune project should work either with the
immediate project, or as part of an embedded build in a subdirectory.

  $ cd a-dune-proj && dune build version.ml --root=.

Now lets try with a jbuild project in the subdirectory:

  $ cd a-jbuild-proj && dune build version.ml --root=.
  File "jbuild", line 1, characters 0-0:
  Warning: jbuild files are not allowed inside Dune 2.0 projects, please
  convert this file to a dune file instead.
  Note: You can use "dune upgrade" to convert your project to dune.
  File "jbuild", line 1, characters 0-18:
  1 | (jbuild_version 1)
      ^^^^^^^^^^^^^^^^^^
  Error: 'jbuild_version' was deleted in version 1.0 of the dune language
  [1]

Now lets try it from the current directory:

  $ dune build a-dune-proj/version.ml --root=.
  File "a-jbuild-proj/jbuild", line 1, characters 0-0:
  Warning: jbuild files are not allowed inside Dune 2.0 projects, please
  convert this file to a dune file instead.
  Note: You can use "dune upgrade" to convert your project to dune.
  File "a-jbuild-proj/jbuild", line 1, characters 0-18:
  1 | (jbuild_version 1)
      ^^^^^^^^^^^^^^^^^^
  Error: 'jbuild_version' was deleted in version 1.0 of the dune language
  [1]
  $ dune build a-jbuild-proj/version.ml --root=.
  File "a-jbuild-proj/jbuild", line 1, characters 0-0:
  Warning: jbuild files are not allowed inside Dune 2.0 projects, please
  convert this file to a dune file instead.
  Note: You can use "dune upgrade" to convert your project to dune.
  File "a-jbuild-proj/jbuild", line 1, characters 0-18:
  1 | (jbuild_version 1)
      ^^^^^^^^^^^^^^^^^^
  Error: 'jbuild_version' was deleted in version 1.0 of the dune language
  [1]

