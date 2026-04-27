Test what happens with circular package dependencies.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name a) (depends b))
  > (package (name b) (depends a))
  > EOF
  $ mkdir a-src b-src
  $ cat >a-src/dune <<EOF
  > (library (public_name a))
  > EOF
  $ cat >a-src/a.ml <<EOF
  > let x = 1
  > EOF
  $ cat >b-src/dune <<EOF
  > (library (public_name b))
  > EOF
  $ cat >b-src/b.ml <<EOF
  > let y = 2
  > EOF

Non-strict (default) uses Top_closure which detects the cycle:

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package a))
  >  (action (with-stdout-to out (echo "ok"))))
  > EOF
  $ dune build out 2>&1

What about depending on both?

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package a) (package b))
  >  (action (with-stdout-to out2 (echo "ok"))))
  > EOF
  $ dune build out2 2>&1

BUG: strict mode silently accepts circular deps because it skips the
closure. The cycle should probably be reported regardless.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (strict_package_deps true)
  > (package (name a) (depends b))
  > (package (name b) (depends a))
  > EOF
  $ rm -rf _build
  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package a))
  >  (action (with-stdout-to out (echo "ok"))))
  > EOF
  $ dune build out 2>&1

BUG: strict with both also silently accepts the cycle:

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package a) (package b))
  >  (action (with-stdout-to out2 (echo "ok"))))
  > EOF
  $ rm -rf _build
  $ dune build out2 2>&1
