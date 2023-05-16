The META file for plugins is built before calling coqdep
  $ cat > dune << EOF
  > (library
  >  (public_name bar.foo)
  >  (name foo))
  > 
  > (coq.theory
  >  (name bar)
  >  (plugins bar.foo))
  > EOF

  $ dune build .bar.theory.d
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  $ ls _build/install/default/lib/bar
  META

