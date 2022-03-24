When a test is disabled through enabled_if, it should not generate any rule. In
particular, in #5505 it would try to use a disabled library and this would print
a "Library X is hidden (unsatisfied `enabled_if`)" message.

  $ cat > dune-project << EOF
  > (lang dune 2.3)
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name l)
  >  (modules l)
  >  (foreign_stubs
  >   (language c)
  >   (names stubs))
  >  (enabled_if false))
  > 
  > (test
  >  (name t)
  >  (modules t)
  >  (libraries l)
  >  (enabled_if false))
  > EOF

  $ touch t.ml l.ml stubs.c

  $ dune build
