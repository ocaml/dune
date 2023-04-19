
  $ mkdir -p vendored
  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (package (name a))
  > EOF
  $ cat > dune <<EOF
  > (vendored_dirs vendored)
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
  [1]

