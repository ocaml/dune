Test that (strict_package_deps) does not affect the install layout. The
layout currently uses immediate package dependencies only.
strict_package_deps controls validation in install_rules, not the layout.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (strict_package_deps true)
  > (package (name foo) (depends bar))
  > (package (name bar) (depends baz))
  > (package (name baz))
  > EOF

  $ mkdir foo-src bar-src baz-src

  $ cat >foo-src/dune <<EOF
  > (library
  >  (public_name foo)
  >  (libraries bar))
  > EOF

  $ cat >foo-src/foo.ml <<EOF
  > let x = 1
  > EOF

  $ cat >bar-src/dune <<EOF
  > (library
  >  (public_name bar)
  >  (libraries baz))
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

The missing library closure is unchanged by strict_package_deps, so only foo
appears:

  $ dune rules --format=json _build/default/out | jq_dune '.[] | ruleDepFilePaths' | censor | grep dune-package | sort
  "_build/install/default/.packages/$DIGEST/lib/foo/dune-package"
