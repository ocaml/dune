The libraries field is deprecated
  $ cat > dune << EOF
  > (library
  >  (public_name bar.foo)
  >  (name foo))
  > 
  > (coq.theory
  >  (name bar)
  >  (libraries bar.foo))
  > EOF

  $ dune build
  File "dune", line 7, characters 1-20:
  7 |  (libraries bar.foo))
       ^^^^^^^^^^^^^^^^^^^
  Warning: 'libraries' was deprecated in version 0.5 of the Coq language. It
  has been renamed to 'plugins'.

Having both a libraries and plugins field is an error
  $ cat > dune << EOF
  > (library
  >  (public_name bar.foo)
  >  (name foo))
  > 
  > (coq.theory
  >  (name bar)
  >  (libraries bar.foo)
  >  (plugins bar.foo))
  > EOF

  $ dune build
  File "dune", line 7, characters 1-20:
  7 |  (libraries bar.foo)
       ^^^^^^^^^^^^^^^^^^^
  Warning: 'libraries' was deprecated in version 0.5 of the Coq language. It
  has been renamed to 'plugins'.
  File "dune", line 7, characters 12-19:
  7 |  (libraries bar.foo)
                  ^^^^^^^
  Error: Cannot both use 'plugins' and 'libraries', please remove 'libraries'
  as it has been deprecated since version 0.5 of the Coq language. It will be
  removed before version 1.0.
  [1]

