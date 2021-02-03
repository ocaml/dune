Test various aspect of variable resolution

  $ test ()
  > {
  >   cat >dune
  >   echo
  >   echo "*** Behavior with Dune 2.8 ***"
  >   echo '(lang dune 2.8)' > dune-project
  >   dune build
  >   echo "exit code: $?"
  > }

Unknown variable:

  $ test <<EOF
  > (alias
  >  (name default)
  >  (deps %{unknwon}))
  > EOF
  
  *** Behavior with Dune 2.8 ***
  File "dune", line 3, characters 9-17:
  3 |  (deps %{unknwon}))
               ^^^^^^^^
  Error: Unknown variable "unknwon"
  exit code: 1

Unknown variable that we don't need to resolve:

  $ test <<EOF
  > (alias
  >  (name other)
  >  (deps %{unknwon}))
  > EOF
  
  *** Behavior with Dune 2.8 ***
  exit code: 0

Specific variable used at the wrong place:

  $ test <<EOF
  > (alias
  >  (name default)
  >  (deps %{library-name}))
  > EOF
  
  *** Behavior with Dune 2.8 ***
  File "dune", line 3, characters 9-22:
  3 |  (deps %{library-name}))
               ^^^^^^^^^^^^^
  Error: Unknown variable "library-name"
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
