fallback isn't allowed in dune

  $ dune build --root dune1
  Entering directory 'dune1'
  File "dune", line 2, characters 1-11:
  2 |  (fallback)
       ^^^^^^^^^^
  Error: 'fallback' was renamed to '(mode fallback)' in the 1.0 version of the dune language
  [1]

2nd fallback form isn't allowed either

  $ dune build --root dune2
  Entering directory 'dune2'
  File "dune", line 2, characters 1-17:
  2 |  (fallback false)
       ^^^^^^^^^^^^^^^^
  Error: 'fallback' was renamed to '(mode fallback)' in the 1.0 version of the dune language
  [1]

But it is allowed in jbuilder

  $ jbuilder build --root jbuild
  The jbuilder binary is deprecated and will cease to be maintained in July 2019.
  Please switch to dune instead.
  Entering directory 'jbuild'
  File "jbuild", line 1, characters 0-0:
  Warning: jbuild files are deprecated, please convert this file to a dune file instead.
  Note: You can use "dune upgrade" to convert your project to dune.
