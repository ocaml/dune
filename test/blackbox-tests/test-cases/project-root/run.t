%{SCOPE_ROOT} (or ${SCOPE_ROOT} in jbuild files) refers to the root of the
project.

  $ dune runtest
  File "jbuild-file/a/jbuild", line 1, characters 0-0:
  Warning: jbuild files are deprecated, please convert this file to a dune file
  instead.
  Note: You can use "dune upgrade" to convert your project to dune.
  File "jbuild-file/a/b/jbuild", line 1, characters 0-0:
  Warning: jbuild files are deprecated, please convert this file to a dune file
  instead.
  Note: You can use "dune upgrade" to convert your project to dune.
  File "jbuild-file/a/jbuild", line 2, characters 3-17:
  2 |   ((name runtest)
         ^^^^^^^^^^^^^^
  Error: Atom expected
  [1]
