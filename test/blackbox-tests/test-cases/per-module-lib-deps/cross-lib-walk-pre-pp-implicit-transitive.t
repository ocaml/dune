Regression guard for cross-library dependency tracking through a
preprocessed entry module.

A consumer compiled against a library with a preprocessed entry
module must still see the [.cmi] of any transitive library whose
type signature leaks through that entry module's interface — even
when the consumer never names the transitive library
syntactically.

The cross-library walker (used to compute per-module dependency
sets) stops at preprocessed entry modules because ocamldep on the
pre-preprocessing source can fail; the neighbour test
[cross-lib-walk-pre-pp-source.t] guards that stopping behaviour.
When the preprocessed module's [.mli] mentions a type from another
library, the walker cannot observe the link. Any per-module
filtering of include flags must be conservative enough to keep the
transitive library on the consumer's include path; otherwise
type-checking the consumer fails the moment it touches the
transitive type.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

A pass-through preprocessor: its sole effect is to mark
[pp_dep]'s modules as preprocessed so the cross-library walker
treats them as opaque.

  $ mkdir pp
  $ cat > pp/dune <<EOF
  > (executable (name pp))
  > EOF
  $ cat > pp/pp.ml <<'EOF'
  > let () =
  >   let ic = open_in_bin Sys.argv.(1) in
  >   try while true do print_endline (input_line ic) done
  >   with End_of_file -> ()
  > EOF

[other_dep] is unwrapped (so tight-eligible) and exposes a record
type:

  $ mkdir other_dep
  $ cat > other_dep/dune <<EOF
  > (library (name other_dep) (wrapped false))
  > EOF
  $ cat > other_dep/other.ml <<EOF
  > type t = { x : int; y : string }
  > let make x y = { x; y }
  > EOF
  $ cat > other_dep/other.mli <<EOF
  > type t = { x : int; y : string }
  > val make : int -> string -> t
  > EOF

[pp_dep] is unwrapped + preprocessed. Its [.mli] mentions
[Other.t]; this is the implicit-transitive link from consumer to
[other_dep] that ocamldep on the consumer cannot observe.

  $ mkdir pp_dep
  $ cat > pp_dep/dune <<EOF
  > (library
  >  (name pp_dep)
  >  (wrapped false)
  >  (libraries other_dep)
  >  (preprocess (action (run %{exe:../pp/pp.exe} %{input-file}))))
  > EOF
  $ cat > pp_dep/d.ml <<EOF
  > let make_thing () = Other.make 1 "hi"
  > EOF
  $ cat > pp_dep/d.mli <<EOF
  > val make_thing : unit -> Other.t
  > EOF

Consumer references [D.make_thing] and accesses its [.x] field.
The field access forces the type checker to load [other.cmi]
even though [Other] is never named in [c.ml].

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (library (name consumer) (wrapped false) (libraries pp_dep))
  > EOF
  $ cat > consumer/c.ml <<EOF
  > let v = D.make_thing ()
  > let _ = v.x
  > EOF

Build the consumer. The build must succeed. If a future change
narrows [c.ml]'s compile rule to drop [other_dep]'s [-I]/[-H] on
the basis of the walker's tight set alone, this fails with
"Unbound record field x".

  $ dune build @check
