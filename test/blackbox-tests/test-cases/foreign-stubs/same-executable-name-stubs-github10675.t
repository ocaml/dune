When several executables have the same name and different sets of foreign
stubs, dune should not crash. See #10675.

  $ cat > dune << EOF
  > (executable
  >  (enabled_if false)
  >  (name main))
  >  
  > (executable
  >  (name main)
  >  (foreign_stubs
  >   (language c)
  >   (names startup)))
  > EOF

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > EOF

  $ touch startup.c main.ml

  $ dune build
