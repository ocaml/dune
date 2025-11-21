Test that external (absolute path) setup scripts work and are NOT deleted

First, create an external script in /tmp:

  $ EXTERNAL_SCRIPT="/tmp/dune_test_external_helper_$$.sh"
  $ cat > "$EXTERNAL_SCRIPT" << 'EOF'
  > #!/bin/sh
  > external_helper() {
  >   echo "External helper called"
  > }
  > export EXTERNAL_VAR="from_external_script"
  > EOF
  $ chmod +x "$EXTERNAL_SCRIPT"

Create a project that uses the external script:

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > (cram enable)
  > EOF

  $ cat > dune << EOF
  > (cram
  >  (setup_scripts $EXTERNAL_SCRIPT))
  > EOF

Create a test that uses the external helper:

  $ cat > external.t << 'EOF'
  > Test that external helper is available
  > 
  >   $ external_helper
  >   External helper called
  > 
  >   $ echo $EXTERNAL_VAR
  >   from_external_script
  > EOF

Run the test:

  $ dune runtest

Verify the external script still exists (was NOT deleted):

  $ if [ -f "$EXTERNAL_SCRIPT" ]; then
  >   echo "External script still exists"
  > else
  >   echo "External script was deleted"
  > fi
  External script still exists
