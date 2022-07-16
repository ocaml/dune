The libraries field is deprecated
  $ cat > dune << EOF
  > (library
  >  (name foo))
  > 
  > (coq.theory 
  >  (name bar)
  >  (libraries foo))
  > EOF

  $ dune build
  File "dune", line 6, characters 1-16:
  6 |  (libraries foo))
       ^^^^^^^^^^^^^^^
  Warning: 'libraries' was deprecated in version 0.5 of the Coq language. It
  has been renamed to 'plugins'.

Having both a libraries and plugins field is an error
  $ cat > dune << EOF
  > (library
  >  (name foo))
  > 
  > (coq.theory 
  >  (name bar)
  >  (libraries foo)
  >  (plugins foo))
  > EOF

  $ dune build
  File "dune", line 6, characters 1-16:
  6 |  (libraries foo)
       ^^^^^^^^^^^^^^^
  Warning: 'libraries' was deprecated in version 0.5 of the Coq language. It
  has been renamed to 'plugins'.
  File "dune", line 6, characters 12-15:
  6 |  (libraries foo)
                  ^^^
  Error: Cannot both use 'plugins' and 'libraries', please remove 'libraries'
  as it has been deprecated since version 0.5 of the Coq language. It will be
  removed before version 1.0.
  [1]

