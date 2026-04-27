Test that (deps (package ...)) works with externally installed packages.
Installed packages (found via findlib) go through the Installed codepath,
not the layout. The layout only applies to Local (workspace) packages.

Install package "a" into a prefix:

  $ mkdir a consumer prefix

  $ cat >a/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name a))
  > EOF

  $ cat >a/dune <<EOF
  > (library (public_name a))
  > EOF

  $ cat >a/a.ml <<EOF
  > let msg = "hello from lib a"
  > EOF

  $ dune build --root a @install
  $ dune install --root a --prefix $PWD/prefix 2>/dev/null
  $ test -f prefix/lib/a/META

Now create a consumer project that depends on the installed package.
The consumer uses (deps (package a)) and ocamlfind to verify the
package is findable:

  $ cat >consumer/dune-project <<EOF
  > (lang dune 3.24)
  > EOF

  $ cat >consumer/dune <<'EOF'
  > (rule
  >  (deps (package a))
  >  (action (with-stdout-to out
  >   (run ocamlfind query a))))
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root consumer out
  $ cat consumer/_build/default/out
  $TESTCASE_ROOT/prefix/lib/a
