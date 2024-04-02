When --only-packages is passed, it runs, as long as the version is new enough

  $ dune build --only-packages a @runtest
  File "dune", lines 10-12, characters 0-61:
  10 | (rule
  11 |   (package a)
  12 |   (action (copy test_temp.ml test_b.ml)))
  Error: 'package' in short-form 'rule' is only available since version 3.8 of
  the dune language. Please update your dune-project file to have (lang dune
  3.8).
  [1]

The version is too old, bump it to 3.8 where this was fixed:

  $ cat > dune-project-3.8 <<EOF
  > (lang dune 3.8)
  > EOF
  $ mv dune-project-3.8 dune-project
  $ dune build --only-packages a @runtest
  A
  A
