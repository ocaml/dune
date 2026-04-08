Sandboxed package builds need library file deps declared properly.

When building with DUNE_SANDBOX=symlink and -p (release/package mode),
dependency libraries' .cmi files must be available in the sandbox. If
library file deps are not declared, the sandbox won't contain them and
compilation will fail.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > (package (name mypkg) (allow_empty))
  > EOF

  $ mkdir types_lib
  $ cat > types_lib/dune <<EOF
  > (library (name types_lib) (public_name mypkg.types_lib))
  > EOF
  $ cat > types_lib/types_lib.ml <<EOF
  > type expr = Int of int | Add of expr * expr
  > EOF

  $ mkdir consumer_lib
  $ cat > consumer_lib/dune <<EOF
  > (library
  >  (name consumer_lib)
  >  (public_name mypkg.consumer_lib)
  >  (libraries types_lib))
  > EOF
  $ cat > consumer_lib/consumer_lib.ml <<EOF
  > let make_int i : Types_lib.expr = Int i
  > let make_add a b : Types_lib.expr = Add (a, b)
  > EOF

The build must succeed — the sandbox must contain types_lib's .cmi files:

  $ DUNE_SANDBOX=symlink dune build -p mypkg 2>&1 | grep -i error
  [1]
