In dune files
-------------

Duplicating a field in a dune file is an error:

  $ dune build --root dune
  Entering directory 'dune'
  File "dune", line 4, characters 1-20:
  4 |  (action (echo bar)))
       ^^^^^^^^^^^^^^^^^^^
  Error: Field "action" is present too many times
  [1]

In jbuild files
---------------

For backward compatibility, it is only a warning in jbuild files:

  $ dune build --root jbuild
  Entering directory 'jbuild'
  File "jbuild", line 1, characters 0-0:
  Warning: jbuild files are not allowed inside Dune 2.0 projects, please
  convert this file to a dune file instead.
  Note: You can use "dune upgrade" to convert your project to dune.
  File "jbuild", line 2, characters 2-16:
  2 |  ((name default)
        ^^^^^^^^^^^^^^
  Error: Atom expected
  [1]
