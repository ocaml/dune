public_name field is deprecated
  $ cat > dune << EOF
  > (coq.theory
  >  (name foo)
  >  (public_name Foo))
  > EOF

  $ dune build
  File "dune", line 3, characters 1-18:
  3 |  (public_name Foo))
       ^^^^^^^^^^^^^^^^^
  Warning: 'public_name' was deprecated in version 0.5 of the Coq language.
  Please use 'package' instead.

both package and public_name field is an error
  $ cat > dune << EOF
  > (coq.theory
  >  (name foo)
  >  (public_name Foo)
  >  (package Foo))
  > EOF

  $ dune build
  File "dune", line 3, characters 1-18:
  3 |  (public_name Foo)
       ^^^^^^^^^^^^^^^^^
  Warning: 'public_name' was deprecated in version 0.5 of the Coq language.
  Please use 'package' instead.
  File "dune", line 3, characters 14-17:
  3 |  (public_name Foo)
                    ^^^
  Error: Cannot both use 'package' and 'public_name', please remove
  'public_name' as it has been deprecated since version 0.5 of the Coq
  langugage. It will be removed before version 1.0.
  [1]
