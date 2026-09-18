Menhir group interfaces should have valid inferred interfaces (#8989), even
when their result type is an abstract type from a sibling module.

  $ make_menhir_project 3.11 2.1

  $ cat >dune <<EOF
  > (include_subdirs qualified)
  > (executable (name foo))
  > EOF
  $ cat >foo.ml <<'EOF'
  > let () = ignore (Group.main (fun _ -> Group.EOF) (Lexing.from_string ""))
  > EOF

  $ mkdir group
  $ cat >group/dune <<EOF
  > (menhir (modules group))
  > EOF

  $ cat >group/group.mly <<EOF
  > %{
  > module M = M
  > %}
  > %token EOF
  > 
  > %start<M.t> main
  > %%
  > 
  > main:
  > | EOF { M.x }
  > EOF

  $ cat >group/m.mli <<'EOF'
  > type t
  > val x : t
  > EOF
  $ cat >group/m.ml <<'EOF'
  > type t = T
  > let x = T
  > EOF

  $ dune build
  File "group/group__mock.ml.mock", line 1:
  Warning 63 [erroneous-printed-signature]: The printed interface differs from
    the inferred interface. The inferred interface contained items which could
    not be printed properly due to name collisions between identifiers.
    File "_none_", line 1:
    Definition of module Dune__exe__Group__/2 Beware
    that this warning is purely informational and will not catch all instances
    of erroneous printed interface.

The parser builds, but the inferred interface contains an invalid module alias.
Menhir only consumes the semantic-action types, so a successful build alone
does not demonstrate that inference produced valid OCaml.

  $ dune exec ./foo.exe
  $ cat _build/default/group/group__mock.mli.inferred
  type token = EOF
  module M = Dune__exe__Group__/2.M
  val menhir_begin_marker : int
  val xv_main : M.t
  val menhir_end_marker : int
  $ ocamlc -stop-after parsing -intf _build/default/group/group__mock.mli.inferred
  File "_build/default/group/group__mock.mli.inferred", line 2, characters 29-30:
  2 | module M = Dune__exe__Group__/2.M
                                   ^
  Error: Syntax error
  [2]

An inferred first-class module type can carry the invalid path into the
generated parser itself, so this is not only an informational warning.

  $ cat >group/m.mli <<'EOF'
  > module type S = sig end
  > module Impl : S
  > EOF
  $ cat >group/m.ml <<'EOF'
  > module type S = sig end
  > module Impl = struct end
  > EOF
  $ cat >group/group.mly <<'EOF'
  > %token EOF
  > %start <unit> main
  > %%
  > main: v=value EOF { ignore v }
  > value: { (module M.Impl : M.S) }
  > EOF
  $ dune build 2>errors
  [1]
  $ cat _build/default/group/group__mock.mli.inferred
  type token = EOF
  val menhir_begin_marker : int
  val xv_value : (module Dune__exe__Group__/2.M.S)
  val xv_main : unit
  val menhir_end_marker : int
  $ sed -n '/Error: Syntax error/{p;q;}' errors
  Error: Syntax error

Nested group interfaces should also work when the directory is renamed. Use
`merge_into` so the group interface is not named after the grammar file. With
Dune 3.25's default development flags, warning 63 prevents the parser from
building.

  $ mkdir -p nested/outer/group
  $ cat >nested/dune-project <<'EOF'
  > (lang dune 3.25)
  > (using menhir 2.1)
  > EOF
  $ cat >nested/dune <<'EOF'
  > (include_subdirs
  >  (mode qualified)
  >  (dirs (outer/group as outer/parser)))
  > (executable (name foo))
  > EOF
  $ cat >nested/foo.ml <<'EOF'
  > let () =
  >   ignore (Outer.Parser.main (fun _ -> Outer.Parser.EOF) (Lexing.from_string ""))
  > EOF
  $ cat >nested/outer/group/dune <<'EOF'
  > (menhir
  >  (modules grammar)
  >  (merge_into group))
  > EOF
  $ cp group/group.mly nested/outer/group/grammar.mly
  $ cp group/m.ml group/m.mli nested/outer/group/
  $ dune exec --root=nested ./foo.exe
  Entering directory 'nested'
  File "outer/group/group__mock.ml.mock", line 1:
  Error (warning 63 [erroneous-printed-signature]): The printed interface
    differs from the inferred interface. The inferred interface contained items
    which could not be printed properly due to name collisions between
    identifiers.
    File "_none_", line 1:
    Definition of module Dune__exe__Outer__Parser__/2
    Beware that this warning is purely informational and will not catch all
    instances of erroneous printed interface.
  Leaving directory 'nested'
  [1]
