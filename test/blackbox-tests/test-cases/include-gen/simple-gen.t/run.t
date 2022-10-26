Testing include with generated Dune files

  $ cat > dune-project << EOF
  > (lang dune 3.6)
  > (using generated_include 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (mode promote)
  >  (targets foo.inc)
  >  (action
  >   (with-stdout-to %{targets}
  >    (echo "(rule\n (targets bar)\n (action\n  (with-stdout-to \%{targets}\n   (echo \"\"))))"))))
  > 
  > (include foo.inc)
  > EOF

  $ dune build bar
  Error: Don't know how to build "bar".
  [1]

  $ dune build foo.inc
  $ dune build bar

  $ ls _build/default
  bar
  foo.inc

  $ cat _build/default/foo.inc
  (rule
   (targets bar)
   (action
    (with-stdout-to %{targets}
     (echo ""))))
