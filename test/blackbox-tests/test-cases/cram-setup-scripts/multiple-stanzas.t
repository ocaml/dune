Test evaluation order when multiple cram stanzas introduce different setup scripts

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > (cram enable)
  > EOF

Create two different setup scripts:

  $ cat > first.sh << 'EOF'
  > #!/bin/sh
  > ORDER="first"
  > show_order() {
  >   echo "Order: $ORDER"
  > }
  > EOF

  $ cat > second.sh << 'EOF'
  > #!/bin/sh
  > ORDER="$ORDER,second"
  > show_final() {
  >   echo "Final: $ORDER"
  > }
  > EOF

Create two cram stanzas - one applying to test-a.t, another to test-b.t:

  $ cat > dune << EOF
  > (cram
  >  (applies_to test-a.t)
  >  (setup_scripts first.sh))
  > 
  > (cram
  >  (applies_to test-b.t)
  >  (setup_scripts second.sh))
  > EOF

Create test-a.t which should have first.sh:

  $ cat > test-a.t << 'EOF'
  >   $ echo "ORDER is: $ORDER"
  >   ORDER is: first
  > 
  >   $ type show_order >/dev/null 2>&1 && echo "show_order exists" || echo "show_order missing"
  >   show_order exists
  > 
  >   $ type show_final >/dev/null 2>&1 && echo "show_final exists" || echo "show_final missing"
  >   show_final missing
  > EOF

Create test-b.t which should have second.sh:

  $ cat > test-b.t << 'EOF'
  >   $ echo "ORDER is: $ORDER"
  >   ORDER is: ,second
  > 
  >   $ type show_order >/dev/null 2>&1 && echo "show_order exists" || echo "show_order missing"
  >   show_order missing
  > 
  >   $ type show_final >/dev/null 2>&1 && echo "show_final exists" || echo "show_final missing"
  >   show_final exists
  > EOF

Run tests to see the actual behavior:

  $ dune runtest
  File "test-a.t", line 1, characters 0-0:
  Error: Files _build/default/test-a.t and _build/default/test-a.t.corrected
  differ.
  File "test-b.t", line 1, characters 0-0:
  Error: Files _build/default/test-b.t and _build/default/test-b.t.corrected
  differ.
  [1]
