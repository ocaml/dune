Menhir group interfaces should have valid inferred interfaces (#8989), even
when their result type is an abstract type from a sibling module.

  $ make_menhir_project 3.13 3.0

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

Show the generated inference query through the grammar's header.

  $ sed -n '1,/^module M = M$/p' _build/default/group/group__mock.ml.mock
  open! struct include Dune__menhir__Group__Group__mock end
  
  type token = 
    | EOF
  
  # 1 "group/group.mly"
    
  module M = M

The mock opens a private alias interface containing the enclosing scopes in
their original order.

  $ cat _build/default/.foo.eobjs/dune__menhir__Group__Group__mock.mli
  include module type of struct
    include Dune__exe
    include Dune__exe__Group__
  end
  $ ocamldep -modules -impl _build/default/group/group__mock.ml.mock
  _build/default/group/group__mock.ml.mock: Dune__menhir__Group__Group__mock Lexing M

The inferred interface must also be valid OCaml. Menhir only consumes the
semantic-action types, so a successful build alone does not demonstrate this.

  $ dune exec ./foo.exe
  $ cat _build/default/group/group__mock.mli.inferred
  type token = EOF
  module M = Dune__exe__Group__M
  val menhir_begin_marker : int
  val xv_main : M.t
  val menhir_end_marker : int
  $ ocamlc -stop-after parsing -intf _build/default/group/group__mock.mli.inferred

Inferred first-class module types must also use valid paths in the generated
parser.

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
  $ dune build
  $ cat _build/default/group/group__mock.mli.inferred
  type token = EOF
  val menhir_begin_marker : int
  val xv_value : (module Dune__exe__Group__M.S)
  val xv_main : unit
  val menhir_end_marker : int

Nested group interfaces should also work when the directory is renamed. Use
`merge_into` so the group interface is not named after the grammar file.

  $ mkdir -p nested/outer/group
  $ cat >nested/dune-project <<'EOF'
  > (lang dune 3.25)
  > (using menhir 3.0)
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
