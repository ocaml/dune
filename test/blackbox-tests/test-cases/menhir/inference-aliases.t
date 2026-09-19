Exercise Menhir inference through qualified aliases (#8989). In particular,
an enclosing group's handwritten interface can shadow an alias even when the
parser's own group interface is handled separately.

  $ mkdir -p nested/outer/inner
  $ cat >nested/dune-project <<'EOF'
  > (lang dune 3.11)
  > (using menhir 2.1)
  > EOF
  $ cat >nested/dune <<'EOF'
  > (include_subdirs qualified)
  > (executable (name main))
  > EOF
  $ cat >nested/main.ml <<'EOF'
  > module Parser = Outer.Inner.Make (struct
  >   type t = string
  >   let value = "parameter"
  >   let offset = 5
  > end)
  > let () =
  >   let count, value =
  >     Parser.main (fun _ -> Parser.EOF) (Lexing.from_string "")
  >   in
  >   Printf.printf "%d:%s\n" count value
  > EOF
  $ cat >nested/outer/outer.ml <<'EOF'
  > module Inner = Inner
  > EOF
  $ cat >nested/outer/ast.mli <<'EOF'
  > module type S = sig val value : int end
  > module Impl : S
  > EOF
  $ cat >nested/outer/ast.ml <<'EOF'
  > module type S = sig val value : int end
  > module Impl = struct let value = 1 end
  > EOF

The innermost Marker must take precedence over the ancestor's Marker. The
parameter's abstract type must also survive inference and functor application.

  $ echo 'let value = "outer"' >nested/outer/marker.ml
  $ echo 'let value = 42' >nested/outer/inner/marker.ml
  $ cat >nested/outer/inner/dune <<'EOF'
  > (menhir (modules inner))
  > EOF
  $ cat >nested/outer/inner/inner.mly <<'EOF'
  > %parameter <Param : sig type t val value : t val offset : int end>
  > %token EOF
  > %start <int * Param.t> main
  > %%
  > main: p=package v=provided EOF {
  >   let module P = (val p : Ast.S) in
  >   P.value + Marker.value + Param.offset, v
  > }
  > package: { (module Ast.Impl : Ast.S) }
  > provided: { Param.value }
  > EOF

Currently, the inferred package type refers to the shadowed ancestor alias.

  $ dune exec --root=nested ./main.exe 2>errors
  [1]
  $ cat nested/_build/default/outer/inner/inner__mock.mli.inferred
  module Make :
    (Param : sig type t val value : t val offset : int end) ->
      sig
        type token = EOF
        val menhir_begin_marker : int
        val xv_provided : Param.t
        val xv_package : (module Dune__exe__Outer__/2.Ast.S)
        val xv_main : int * Param.t
        val menhir_end_marker : int
      end
  $ sed -n '/Error: Syntax error/{p;q;}' errors
  Error: Syntax error

Inference must not need every sibling's interface. The parser's own CMI does
not exist yet, and an unrelated module need not even type-check. Building only
the generated parser interface should leave that unrelated module unbuilt.

  $ mkdir isolated
  $ cat >isolated/dune-project <<'EOF'
  > (lang dune 3.25)
  > (using menhir 2.1)
  > EOF
  $ cat >isolated/dune <<'EOF'
  > (library (name parsers))
  > (menhir (modules parser))
  > EOF
  $ echo 'let value = 42' >isolated/value.ml
  $ echo 'let value = undefined' >isolated/unused.ml
  $ cat >isolated/parser.mly <<'EOF'
  > %token EOF
  > %start <int> main
  > %%
  > main: EOF { Value.value }
  > EOF
  $ dune build --root=isolated parser.mli
  $ test ! -f isolated/_build/default/.parsers.objs/byte/parsers__Unused.cmi
  $ test ! -f isolated/_build/default/.parsers.objs/byte/parsers__Parser.cmi

Inferred result types must keep their dependencies when a sibling's interface
changes. This parser returns an abstract type without declaring it in the
grammar.

  $ mkdir -p incremental/group
  $ cat >incremental/dune-project <<'EOF'
  > (lang dune 3.25)
  > (using menhir 2.1)
  > EOF
  $ cat >incremental/dune <<'EOF'
  > (include_subdirs qualified)
  > (executable (name main))
  > EOF
  $ cat >incremental/group/dune <<'EOF'
  > (menhir (modules group))
  > EOF
  $ cat >incremental/group/group.mly <<'EOF'
  > %token EOF
  > %start <_> main
  > %%
  > main: EOF { M.value }
  > EOF
  $ printf 'type t\nval value : t\n' >incremental/group/m.mli
  $ printf 'type t = int\nlet value = 42\n' >incremental/group/m.ml
  $ cat >incremental/main.ml <<'EOF'
  > let () = ignore (Group.main (fun _ -> Group.EOF) (Lexing.from_string ""))
  > EOF
  $ dune exec --root=incremental ./main.exe

Adding a value changes the sibling's CMI without changing the inferred parser
signature. The parser's CMI must still be rebuilt.

  $ echo 'val extra : int' >>incremental/group/m.mli
  $ echo 'let extra = 0' >>incremental/group/m.ml
  $ dune exec --root=incremental ./main.exe

Menhir group interfaces should retain the guard against referring to Dune's
private alias module, just like handwritten modules do.

  $ mkdir -p hidden/group
  $ cat >hidden/dune-project <<'EOF'
  > (lang dune 3.25)
  > (using menhir 2.1)
  > EOF
  $ cat >hidden/dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name lib)
  >  (wrapped false))
  > EOF
  $ cat >hidden/group/dune <<'EOF'
  > (menhir (modules group))
  > EOF
  $ echo 'let value = 42' >hidden/group/m.ml
  $ cat >hidden/group/group.mly <<'EOF'
  > %token EOF
  > %start <int> main
  > %%
  > main: EOF { Group__.M.value }
  > EOF
  $ dune build --root=hidden group/group.mli 2>hidden-errors
  [1]
  $ sed -n '/this module is shadowed/p;/Error: Unbound module/p' hidden-errors
  this module is shadowed
  Error: Unbound module Group__.M
