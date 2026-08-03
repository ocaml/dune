Declared circular `depends` between packages does not block rule-level
`(deps (package ...))`. The cycle is in opam metadata only; the build
artifacts form a DAG, so there is no build cycle to report.

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

Non-strict, single package:

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package a))
  >  (action (with-stdout-to out (echo "ok"))))
  > EOF
  $ dune build out 2>&1

Non-strict, both packages:

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package a) (package b))
  >  (action (with-stdout-to out2 (echo "ok"))))
  > EOF
  $ dune build out2 2>&1

Strict, single package:

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

Strict, both packages:

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package a) (package b))
  >  (action (with-stdout-to out2 (echo "ok"))))
  > EOF
  $ rm -rf _build
  $ dune build out2 2>&1
