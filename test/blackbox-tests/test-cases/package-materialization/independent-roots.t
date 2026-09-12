Package closure includes all implementations from required packages.
Unlike a linking closure, it does not select just one implementation of each
virtual library.

  $ make_dune_project 3.24
  $ cat >>dune-project <<'EOF'
  > (package (name roots))
  > (package (name virtual-support))
  > (package (name alternative-support))
  > EOF

  $ mkdir roots-a roots-b virtual-lib default-impl alternative-impl
  $ cat >virtual-lib/dune <<'EOF'
  > (library
  >  (name virtual_support)
  >  (public_name virtual-support)
  >  (wrapped false)
  >  (virtual_modules virtual_support)
  >  (default_implementation virtual-support.default))
  > EOF
  $ echo 'val value : int' >virtual-lib/virtual_support.mli

  $ cat >default-impl/dune <<'EOF'
  > (library
  >  (name default_impl)
  >  (public_name virtual-support.default)
  >  (implements virtual-support))
  > EOF
  $ echo 'let value = 1' >default-impl/virtual_support.ml

  $ cat >alternative-impl/dune <<'EOF'
  > (library
  >  (name alternative_impl)
  >  (public_name alternative-support)
  >  (implements virtual-support))
  > EOF
  $ echo 'let value = 2' >alternative-impl/virtual_support.ml

The first root selects the alternative implementation while the second root
uses the virtual library on its own and therefore needs the default.

  $ cat >roots-b/dune <<'EOF'
  > (library
  >  (name roots_b)
  >  (public_name roots.b)
  >  (libraries virtual-support))
  > EOF
  $ echo 'let value = Virtual_support.value' >roots-b/roots_b.ml

  $ cat >roots-a/dune <<'EOF'
  > (library
  >  (name roots_a)
  >  (public_name roots.a)
  >  (libraries roots.b alternative-support))
  > EOF
  $ echo 'let value = Roots_b.value' >roots-a/roots_a.ml

A third independent root explicitly requires the default implementation.
Combining all roots into one compilation closure would incorrectly report
competing implementations, even though each library compiles separately.

  $ mkdir roots-c
  $ cat >roots-c/dune <<'EOF'
  > (library
  >  (name roots_c)
  >  (public_name roots.c)
  >  (libraries virtual-support.default))
  > EOF
  $ echo 'let value = Virtual_support.value' >roots-c/roots_c.ml

  $ cat >dune <<'EOF'
  > (rule
  >  (target result)
  >  (deps (package roots))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:ocamlfind} query -predicates byte -format "%d/%A"
  >     virtual-support.default))))
  > (rule
  >  (target alternative-result)
  >  (deps (package roots))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:ocamlfind} query -predicates byte -format "%d/%A"
  >     alternative-support))))
  > EOF

Both implementations and their archives are present in the scoped layout.

  $ dune build result
  $ test -f "$(cat _build/default/result)"

  $ dune build alternative-result
  $ test -f "$(cat _build/default/alternative-result)"
