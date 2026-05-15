Test that (deps (package foo)) only includes foo in the layout, not its
transitive package dependencies. Actions should declare what they need.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name foo) (depends bar))
  > (package (name bar) (depends baz))
  > (package (name baz))
  > EOF

  $ mkdir foo-src bar-src baz-src

  $ cat >foo-src/dune <<EOF
  > (library (public_name foo))
  > EOF

  $ cat >foo-src/foo.ml <<EOF
  > let x = 1
  > EOF

  $ cat >bar-src/dune <<EOF
  > (library (public_name bar))
  > EOF

  $ cat >bar-src/bar.ml <<EOF
  > let y = 2
  > EOF

  $ cat >baz-src/dune <<EOF
  > (library (public_name baz))
  > EOF

  $ cat >baz-src/baz.ml <<EOF
  > let z = 3
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package foo))
  >  (action (with-stdout-to out (echo "ok"))))
  > EOF

  $ dune build out

Only foo appears — not bar or baz, even though foo declares (depends bar)
and bar declares (depends baz):

  $ dune rules --format=json _build/default/out | jq 'include "dune"; .[] | ruleDepFilePaths' | censor | grep dune-package | sort
  "_build/install/default/.packages/$DIGEST/lib/foo/dune-package"
