The META file for plugins is built before calling coqdep
  $ cat > dune << EOF
  > (library
  >  (public_name bar.foo)
  >  (name foo))
  > 
  > (rocq.theory
  >  (name bar)
  >  (plugins bar.foo))
  > EOF

  $ dune build .bar.theory.d
  $ ls _build/install/default/lib/bar
  META

