Test if setup scripts are visible in test directory

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > EOF

  $ cat > secret.sh << 'EOF'
  > MY_SECRET="should_not_be_visible"
  > EOF

  $ cat > dune << EOF
  > (cram
  >  (setup_scripts secret.sh))
  > EOF

  $ cat > check.t << 'EOF'
  >   $ ls *.sh 2>&1 || echo "No .sh files found"
  >   No .sh files found
  > EOF

  $ dune runtest
  File "check.t", line 1, characters 0-0:
  Error: Files _build/default/check.t and _build/default/check.t.corrected
  differ.
  [1]
