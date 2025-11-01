Test that setup scripts are NOT visible in the test directory

Create a project with a helper script:

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > (cram enable)
  > EOF

  $ cat > helpers.sh << 'EOF'
  > #!/bin/sh
  > test_helper() {
  >   echo "Helper called"
  > }
  > EOF

  $ cat > dune << EOF
  > (cram
  >  (setup_scripts helpers.sh))
  > EOF

  $ cat > visibility.t << 'EOF'
  > Test that the setup script is NOT visible in the test directory
  > 
  >   $ ls -la
  >   total 0
  >   drwxr-xr-x 1 username group 0 Oct 28 00:00 .
  >   drwxr-xr-x 1 username group 0 Oct 28 00:00 ..
  >   -rw-r--r-- 1 username group 0 Oct 28 00:00 visibility.t
  > 
  > Test that we can still call the helper function
  > 
  >   $ test_helper
  >   Helper called
  > EOF

Run the test:

  $ dune runtest
  File "visibility.t", line 1, characters 0-0:
  Error: Files _build/default/visibility.t and
  _build/default/visibility.t.corrected differ.
  [1]
