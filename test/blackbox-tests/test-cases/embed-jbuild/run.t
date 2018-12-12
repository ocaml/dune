Generating a version.ml from a jbuild/dune project should work either with the
immediate project, or as part of an embedded build in a subdirectory.

  $ cd a-dune-proj && dune build version.ml --root=.

Now lets try with a jbuild project in the subdirectory:

  $ cd a-jbuild-proj && dune build version.ml --root=.

Now lets try it from the current directory:

  $ dune build a-dune-proj/version.ml --root=.
  File "a-dune-proj/dune", line 5, characters 29-49:
  5 |     (echo "let version = \"%{version:a-dune-proj}\""))))
                                   ^^^^^^^^^^^^^^^^^^^^
  Error: Package "a-dune-proj" doesn't exist in the current project.
  [1]
  $ dune build a-jbuild-proj/version.ml --root=.
  File "a-jbuild-proj/jbuild", line 7, characters 10-54:
  7 |     (echo "let version = \"${version:a-jbuild-proj}\"")))))
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Package "a-jbuild-proj" doesn't exist in the current project.
  [1]

