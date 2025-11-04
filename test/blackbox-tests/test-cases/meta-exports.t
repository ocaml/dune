Check our handling of `exports` in META files. We begin with a test showing that
we can *consume* META files containing an `exports` field.

To do this, first we create a Findlib hierarchy containing two installed
packages, `foo` and `bar`. The package `foo` is empty and only exists to
re-export `bar`. The package `bar` consists of a bytecode library, `bar.cma`.

  $ mkdir -p _install/foo
  $ cat >_install/foo/META <<EOF
  > requires = "bar"
  > EOF

  $ mkdir -p _install/bar
  $ cat >_install/bar/META <<EOF
  > archive(byte) = "bar.cma"
  > EOF
  $ cat >_install/bar/bar.ml <<EOF
  > let x = 42
  > EOF
  $ ocamlc -a -o _install/bar/bar.cma _install/bar/bar.ml

We now define a Dune project that will consume `foo`.

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (modes byte)
  >  (libraries foo))
  > EOF
  $ cat >main.ml <<EOF
  > let () = print_int Bar.x; print_newline ()
  > EOF

Compilation works with `(implicit_transitive_deps)`:

  $ OCAMLPATH=$(pwd)/_install dune exec ./main.exe
  42

However, the compilation without `(implicit_transitive_deps)` fails:

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (implicit_transitive_deps false)
  > EOF

  $ OCAMLPATH=$(pwd)/_install dune exec ./main.exe
  File "main.ml", line 1, characters 19-22:
  1 | let () = print_int Bar.x; print_newline ()
                         ^^^
  Error: Unbound module Bar
  [1]

Next, we add the `exports` field to `foo`'s `META` file:

  $ cat >_install/foo/META <<EOF
  > requires = "bar"
  > exports = "bar"
  > EOF

and compilation now works again:

  $ OCAMLPATH=$(pwd)/_install dune exec ./main.exe
  42

----------------------------------------------------------------

Next, we check that we can *produce* META files with the `export` field.  To do
this, we define two Dune libraries `foo` and `bar`, where `foo` depends on `bar`
using `(re_export)`.

  $ cat >dune-project.gen <<'EOF'
  > cat <<EOF2
  > (lang dune $VERSION)
  > (package (name foo))
  > (package (name bar))
  > EOF2
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (public_name bar)
  >  (modules bar))
  > (library
  >  (public_name foo)
  >  (libraries (re_export bar))
  >  (modules foo))
  > EOF
  $ true >bar.ml
  $ true >foo.ml

First we try with dune version 3.16 (it should not generate the `exports` field):

  $ VERSION=3.16 sh dune-project.gen >dune-project
  $ dune build && dune install --libdir $(pwd)/_local --prefix $(pwd)
  $ cat _local/foo/META
  description = ""
  requires = "bar"
  archive(byte) = "foo.cma"
  archive(native) = "foo.cmxa"
  plugin(byte) = "foo.cma"
  plugin(native) = "foo.cmxs"

Now with dune version 3.17 (it should generate the `exports` field):

  $ VERSION=3.17 sh dune-project.gen >dune-project
  $ dune build && dune install --libdir $(pwd)/_local --prefix $(pwd)
  $ cat _local/foo/META
  description = ""
  requires = "bar"
  exports = "bar"
  archive(byte) = "foo.cma"
  archive(native) = "foo.cmxa"
  plugin(byte) = "foo.cma"
  plugin(native) = "foo.cmxs"
