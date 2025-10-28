Test setup_scripts feature for cram tests

Create a project with a helper script:

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > EOF

  $ cat > helpers.sh << 'EOF'
  > #!/bin/sh
  > test_helper() {
  >   echo "Helper called: $1"
  > }
  > export MY_VAR="test_value_from_helper"
  > EOF

  $ cat > dune << EOF
  > (cram
  >  (setup_scripts helpers.sh))
  > EOF

  $ cat > basic.t << 'EOF'
  > Test that setup scripts are sourced and functions are available
  > 
  >   $ test_helper "foo"
  > 
  > Test that variables from setup scripts are available
  > 
  >   $ echo $MY_VAR
  > 
  > Check if setup script is visible in test directory
  > 
  >   $ ! [ -e *.sh ] && echo "No sh files"
  > EOF

Run the test:

  $ dune runtest
  File "basic.t", line 1, characters 0-0:
  Error: Files _build/default/basic.t and _build/default/basic.t.corrected
  differ.
  [1]

  $ dune promote
  Promoting _build/default/basic.t.corrected to basic.t.

  $ cat basic.t
  Test that setup scripts are sourced and functions are available
  
    $ test_helper "foo"
    Helper called: foo
  
  Test that variables from setup scripts are available
  
    $ echo $MY_VAR
    test_value_from_helper
  
  Check if setup script is visible in test directory
  
    $ ! [ -e *.sh ] && echo "No sh files"
    No sh files
