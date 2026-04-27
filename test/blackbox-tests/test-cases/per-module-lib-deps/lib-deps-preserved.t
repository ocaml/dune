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

[Uses_lib.cmx] keeps the glob: [Uses_lib.ml] references [Mylib], and
[mylib] is wrapped — wrapped libraries fall back to a glob over their
public cmi dir even under per-module filtering, since ocamldep's
[-modules] output cannot distinguish [Foo.Bar.x] from [Foo.Baz.x].

  $ dune rules --root . --format=json --deps _build/default/.main.eobjs/native/dune__exe__Uses_lib.cmx |
  > jq -r 'include "dune"; .[] | depsGlobPredicates'
  *.cmi

[Main.cmx] has no inter-library deps. [Main.ml] references only
[Uses_lib], an intra-library module filtered out before the
inter-library filter runs. [Uses_lib] is a transitive intra-library
dep; its [.mli] has no cross-library references. [Uses_lib.ml] does
reference [Mylib], but those references propagate to the consumer
only through [Uses_lib.cmx] for cross-module inlining. Under the
default profile dune builds with [-opaque], which disables that
inlining, so [Uses_lib.ml]'s references are sealed behind its [.cmi].

  $ dune rules --root . --format=json --deps _build/default/.main.eobjs/native/dune__exe__Main.cmx |
  > jq -r 'include "dune"; .[] | depsGlobPredicates'
