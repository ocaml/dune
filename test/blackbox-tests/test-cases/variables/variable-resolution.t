Test various aspect of variable resolution

  $ test ()
  > {
  >   cat >dune
  >   echo
  >   echo "*** Behavior with Dune 2.8 ***"
  >   echo '(lang dune 2.8)' > dune-project
  >   dune build
  >   echo "exit code: $?"
  >   echo
  >   echo "*** Behavior with Dune 3.0 ***"
  >   echo '(lang dune 3.0)' > dune-project
  >   dune build
  >   echo "exit code: $?"
  > }

Unknown variable:

  $ test <<EOF
  > (alias
  >  (name default)
  >  (deps %{unknown}))
  > EOF
  
  *** Behavior with Dune 2.8 ***
  File "dune", line 3, characters 7-17:
  3 |  (deps %{unknown}))
             ^^^^^^^^^^
  Error: Unknown variable %{unknown}
  exit code: 1
  
  *** Behavior with Dune 3.0 ***
  File "dune", line 3, characters 7-17:
  3 |  (deps %{unknown}))
             ^^^^^^^^^^
  Error: Unknown variable %{unknown}
  exit code: 1

Unknown variable that we don't need to resolve:

  $ test <<EOF
  > (alias
  >  (name other)
  >  (deps %{unknown}))
  > EOF
  
  *** Behavior with Dune 2.8 ***
  exit code: 0
  
  *** Behavior with Dune 3.0 ***
  File "dune", line 3, characters 7-17:
  3 |  (deps %{unknown}))
             ^^^^^^^^^^
  Error: Unknown variable %{unknown}
  exit code: 1

Specific variable used at the wrong place:

  $ test <<EOF
  > (alias
  >  (name default)
  >  (deps %{library-name}))
  > EOF
  
  *** Behavior with Dune 2.8 ***
  File "dune", line 3, characters 7-22:
  3 |  (deps %{library-name}))
             ^^^^^^^^^^^^^^^
  Error: %{library-name} isn't allowed in this position.
  exit code: 1
  
  *** Behavior with Dune 3.0 ***
  File "dune", line 3, characters 7-22:
  3 |  (deps %{library-name}))
             ^^^^^^^^^^^^^^^
  Error: %{library-name} isn't allowed in this position.
  exit code: 1

Specific variable used at the wrong place that we don't need to
resolve:

  $ test <<EOF
  > (alias
  >  (name other)
  >  (deps %{library-name}))
  > EOF
  
  *** Behavior with Dune 2.8 ***
  exit code: 0
  
  *** Behavior with Dune 3.0 ***
  exit code: 0
