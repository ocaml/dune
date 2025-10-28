Test evaluation order when multiple cram stanzas introduce different setup scripts

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > EOF

Create two different setup scripts:

  $ cat > first.sh << 'EOF'
  > #!/bin/sh
  > setup() {
  >   echo first.sh
  > }
  > EOF

  $ cat > second.sh << 'EOF'
  > #!/bin/sh
  > setup() {
  >  echo second.sh
  > }
  > EOF

Create two cram stanzas - one applying to test-a.t, another to test-b.t:

  $ cat > dune << EOF
  > (cram
  >  (setup_scripts first.sh))
  > 
  > (cram
  >  (setup_scripts second.sh))
  > EOF

  $ cat > test.t << 'EOF'
  >   $ setup
  > EOF

Run tests to see the actual behavior:

  $ dune runtest
  File "test.t", line 1, characters 0-0:
  Error: Files _build/default/test.t and _build/default/test.t.corrected
  differ.
  [1]

  $ dune promote
  Promoting _build/default/test.t.corrected to test.t.

  $ cat test.t
    $ setup
    first.sh
