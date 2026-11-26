Verify the cm_kind/-opaque rules for library file deps in compile rules.

[mylib] is a wrapped library exposing one entry [Mylib]. The
executable has two modules: [Uses_lib] which references [Mylib] in
its [.ml] (but not its [.mli]) and [Main] which references only
[Uses_lib]. [Main] has no [.mli].

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library (name mylib))
  > EOF
  $ cat > lib/mylib.ml <<EOF
  > let value = 42
  > EOF
  $ cat > lib/mylib.mli <<EOF
  > val value : int
  > EOF

  $ cat > dune <<EOF
  > (executable (name main) (libraries mylib))
  > EOF
  $ cat > uses_lib.ml <<EOF
  > let get () = Mylib.value
  > EOF
  $ cat > uses_lib.mli <<EOF
  > val get : unit -> int
  > EOF
  $ cat > main.ml <<EOF
  > let () = print_int (Uses_lib.get ())
  > EOF

  $ dune build ./main.exe

[Uses_lib.cmx] keeps the glob: [Uses_lib.ml] references [Mylib], a
wrapped library that falls back to a directory glob over its public
cmi dir.

  $ dune rules --root . --format=json --deps _build/default/.main.eobjs/native/dune__exe__Uses_lib.cmx |
  > jq -r 'include "dune"; .[] | depsGlobPredicates'
  *.cmi

[Main.cmx] has no inter-library deps. Under [-opaque] (the default
profile), [Uses_lib]'s references to [Mylib] are sealed behind
[Uses_lib]'s [.cmi] and don't propagate to [Main]'s native compile.

  $ dune rules --root . --format=json --deps _build/default/.main.eobjs/native/dune__exe__Main.cmx |
  > jq -r 'include "dune"; .[] | depsGlobPredicates'
