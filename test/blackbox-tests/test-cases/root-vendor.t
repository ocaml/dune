
  $ mkdir -p vendored
  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (package (name a))
  > EOF
  $ cat > dune <<EOF
  > (vendored_dirs vendored)
  > EOF
  $ cat > dune <<EOF
  > (library
  >  (public_name a))
  > EOF
  $ cat > vendored/dune-project <<EOF
  > (lang dune 3.7)
  > EOF
  $ cat > vendored/dune <<EOF
  > (executable
  >  (name test)
  >  (libraries base))
  > EOF
  > touch vendored/test.ml

  $ dune top -p a | grep base

try again, with `(vendored_dirs *)` inside `vendor/`

  $ rm -rf "$PWD"/*
  $ mkdir -p vendor/vendored
  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (package (name a))
  > EOF
  $ cat > vendor/dune <<EOF
  > (vendored_dirs *)
  > EOF
  $ cat > dune <<EOF
  > (library
  >  (public_name a))
  > EOF
  $ cat > vendor/vendored/dune-project <<EOF
  > (lang dune 3.7)
  > EOF
  $ cat > vendor/vendored/dune <<EOF
  > (executable
  >  (name test)
  >  (libraries base))
  > EOF
  > touch vendor/vendored/test.ml

  $ dune top -p a
  #directory "$TESTCASE_ROOT/_build/default/.a.objs/byte";;
  #load "$TESTCASE_ROOT/_build/default/a.cma";;

