%{SCOPE_ROOT} (or ${SCOPE_ROOT} in jbuild files) refers to the root of the
project.

  $ dune runtest
  File "jbuild-file/a/jbuild", line 1, characters 0-0:
  Warning: jbuild files are deprecated, please convert this file to a dune file instead.
  Note: You can use "dune upgrade" to convert your project to dune.
  File "jbuild-file/a/b/jbuild", line 1, characters 0-0:
  Warning: jbuild files are deprecated, please convert this file to a dune file instead.
  Note: You can use "dune upgrade" to convert your project to dune.
  From dune-file/a/b/: ../../..
  From dune-file/a/: ../..
  From jbuild/a/b/: ../../..
  From jbuild/a/: ../..
  From root: .
