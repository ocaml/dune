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
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
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
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
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

