Test that setup scripts accumulate when multiple stanzas match the same test

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > EOF

Create three different setup scripts:

  $ cat > base.sh << 'EOF'
  > #!/bin/sh
  > SCRIPTS_LOADED="base"
  > EOF

  $ cat > helper1.sh << 'EOF'
  > #!/bin/sh
  > SCRIPTS_LOADED="$SCRIPTS_LOADED,helper1"
  > helper1_func() {
  >   echo "Helper 1 called"
  > }
  > EOF

  $ cat > helper2.sh << 'EOF'
  > #!/bin/sh
  > SCRIPTS_LOADED="$SCRIPTS_LOADED,helper2"
  > helper2_func() {
  >   echo "Helper 2 called"
  > }
  > EOF

Create multiple cram stanzas that all match test.t:

  $ cat > dune << EOF
  > (cram
  >  (setup_scripts base.sh))
  > 
  > (cram
  >  (applies_to test)
  >  (setup_scripts helper1.sh))
  > 
  > (cram
  >  (applies_to test)
  >  (setup_scripts helper2.sh))
  > EOF

Create a test that uses functions from all scripts:

  $ cat > test.t << 'EOF'
  >   $ echo "Scripts loaded: $SCRIPTS_LOADED"
  >   Scripts loaded: base,helper1,helper2
  > 
  >   $ helper1_func
  >   Helper 1 called
  > 
  >   $ helper2_func
  >   Helper 2 called
  > EOF

Run the test:

  $ dune runtest
  File "test.t", line 1, characters 0-0:
  Error: Files _build/default/test.t and _build/default/test.t.corrected
  differ.
  [1]

  $ dune promote
  Promoting _build/default/test.t.corrected to test.t.

  $ cat test.t
    $ echo "Scripts loaded: $SCRIPTS_LOADED"
    Scripts loaded: base
  
    $ helper1_func
    Helper 1 called
  
    $ helper2_func
    Helper 2 called
