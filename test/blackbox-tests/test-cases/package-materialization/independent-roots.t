Each library installed by an explicitly requested package is an independent
closure root. Combining the roots before resolving virtual implementations can
suppress a default implementation needed by one root.

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

Each root is closed independently, so the layout includes both the alternative
implementation selected by `roots.a` and the default needed by `roots.b`.

  $ dune build result
  $ cat _build/default/result | censor
  $PWD/_build/install/default/.packages/$DIGEST/lib/virtual-support/default/default_impl.cma
  $ test -f "$(cat _build/default/result)"

  $ dune build alternative-result
  $ cat _build/default/alternative-result | censor
  $PWD/_build/install/default/.packages/$DIGEST/lib/alternative-support/alternative_impl.cma
  $ test -f "$(cat _build/default/alternative-result)"
