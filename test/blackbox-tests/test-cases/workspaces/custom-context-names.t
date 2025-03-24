
  $ cat > dune-workspace << EOF
  > (lang dune 3.13)
  > (context default)
  > (context
  >  (default
  >   (name log)))
  > EOF

  $ dune build
  File "dune-workspace", line 5, characters 8-11:
  5 |   (name log)))
              ^^^
  Error: "log" is an invalid context name.
  [1]

  $ cat > dune-workspace << EOF
  > (lang dune 3.13)
  > (context default)
  > (context
  >  (default
  >   (name install)))
  > EOF
  $ dune build 2>&1 | grep "must not crash"
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the


