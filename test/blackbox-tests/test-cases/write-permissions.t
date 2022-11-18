Check that dune <= 2.3 leaves write permissions alone.

  $ mkdir 2.3 2.4
  $ cat > 2.3/dune-project <<EOF
  > (lang dune 2.3)
  > EOF
  $ cat > 2.3/dune <<EOF
  > (rule
  >   (deps source)
  >   (targets target)
  >   (action (bash "cat source source > target")))
  > EOF
  $ cat > 2.3/source <<EOF
  > \_o< COIN
  > EOF
  $ dune build --root 2.3 target | head -c1
  Entering directory '2.3'
  Leaving directory '2.3'
  $ dune_cmd stat permissions 2.3/_build/default/target | head -c1
  6

Check that dune >= 2.4 removes target write permissions.

  $ cat > 2.4/dune-project <<EOF
  > (lang dune 2.4)
  > (package
  >   (name foo))
  > EOF
  $ touch 2.4/foo.ml
  $ cat > 2.4/dune <<EOF
  > (executable
  >   (public_name foo))
  > (rule
  >   (deps source)
  >   (targets target)
  >   (action (bash "cat source source > target")))
  > (install
  >   (section bin)
  >   (package foo)
  >   (files foo.exe))
  > (install
  >   (section share)
  >   (package foo)
  >   (files target))
  > EOF
  $ cat > 2.4/source <<EOF
  > \_o< COIN
  > EOF
  $ dune build --root 2.4 foo.exe @install
  Entering directory '2.4'
  Leaving directory '2.4'
  $ dune_cmd stat permissions 2.4/_build/default/foo.exe | head -c1
  5
  $ dune install --root 2.4 --prefix ./
  Entering directory '2.4'
  Installing lib/foo/META
  Installing lib/foo/dune-package
  Installing bin/foo
  Installing bin/foo.exe
  Installing share/foo/target
  Leaving directory '2.4'
  $ dune_cmd stat permissions 2.4/bin/foo.exe | head -c1
  7
  $ dune_cmd stat permissions 2.4/share/foo/target | head -c1
  6
